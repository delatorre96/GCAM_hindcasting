set_gcam_paths <- function(gcam_path) {
  #Exmple:
  #dir_gcamdata <- "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata"
  dir_gcam <<- gcam_path
  config_file <<-  paste0(gcam_path,'/exe/configuration.xml')
  run_gcam_file <<- paste0(gcam_path,'/exe/run-gcam.bat')
  dir_gcamdata <<- paste0(gcam_path,'/input/gcamdata')
  dir_chunks <<- paste0(dir_gcamdata,'/R')
  dir_csvs_iniciales <<- paste0(dir_gcamdata,'/inst/extdata')
  batch_queries_file <<- paste0(dir_gcam,'/exe/batch_queries/xmldb_batch.xml')

  print(dir_gcam)
  print(config_file)
  print(run_gcam_file)
  print(dir_gcamdata)
  print(dir_chunks)
  print(dir_csvs_iniciales)
  print(batch_queries_file)
}


get_csv_info <- function(csv_file) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE)
  setwd(dir_csvs_iniciales)
  path <- find_csv_file(csv_file, optional = TRUE)
  lines <- readLines(path)
  header_lines <- lines[grepl("^#", lines)]
  df <- read.csv(path, comment.char = '#', check.names = FALSE)
  return(list(header_lines = header_lines, df = df, path = path))
}


run_gcam <- function(bat_path) {
  bat_dir <- dirname(bat_path)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(bat_dir)
  status <- system2("cmd.exe", args = c("/c", basename(bat_path)), stdout = "", stderr = "")
  cat(sprintf("\nGCAM terminó con código de salida %d\n", status))
  return(status)
}

introduceUncertainty <- function(df,
                                 relative_uncertainty,
                                 integer_exponent = FALSE) {

  if ("logit.exponent" %in% names(df)) {

    if (!is.numeric(df$logit.exponent)) {

      warning("'logit.exponent' no es numérica.")

    } else {

      original <- df$logit.exponent

      df$logit.exponent <- sapply(original, function(x) {

        if (is.na(x) || x == 0) {

          return(x)

        }

        factor <- runif(1,
                        1 - relative_uncertainty,
                        1 + relative_uncertainty)

        nuevo <- round(x * factor, digits = 2)

        if (integer_exponent)
          nuevo <- round(nuevo)

        nuevo
      })
    }
  }

  df
}

# introduceUncertainty <- function(df,
#                                  relative_uncertainty,
#                                  integer_exponent = FALSE) {
#   
#   if (!"logit.exponent" %in% names(df))
#     return(df)
#   
#   if (!is.numeric(df$logit.exponent)) {
#     warning("'logit.exponent' no es numérica.")
#     return(df)
#   }
#   
#   idx <- df$subsector == "refined liquids"
#   
#   if (any(idx)) {
#     
#     x <- df$logit.exponent[idx]
#     
#     if (!is.na(x) && x != 0) {
#       
#       factor <- runif(
#         1,
#         1 - relative_uncertainty,
#         1 + relative_uncertainty
#       )
#       
#       nuevo <- round(x * factor, 1)
#       
#       if (integer_exponent)
#         nuevo <- round(nuevo)
#       
#       df$logit.exponent[idx] <- nuevo
#     }
#   }
#   
#   df
# }

change_csvs <- function(logit_EUR_files,
                        relative_uncertainty,
                        integer_exponent){
  for (i in logit_EUR_files){
    l <- get_csv_info(i)
    df_i <- l$df
    headers <- l$header_lines
    path <- l$path
    
    df_i_changed <- introduceUncertainty(df_i,relative_uncertainty, integer_exponent)
    path <- paste0(dir_csvs_iniciales,'/',path)
    
    # Reescribir CSV
    writeLines(headers, path)
    suppressWarnings(
      write.table(df_i_changed, path, sep = ",", append = TRUE,
                  row.names = FALSE, quote = FALSE, na = "")
      
    )
  }
}




append_input <- function(df, output_file) {
  
  # Calcular la iteración
  if (!file.exists(output_file)) {
    
    iteration <- 1
    
  } else {
    
    old <- read.csv(output_file, check.names = FALSE)
    iteration <- max(old$iteration, na.rm = TRUE) + 1
  }
  
  # Añadir columna
  df$iteration <- iteration
  
  # Escribir
  write.table(
    df,
    file = output_file,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(output_file),
    append = file.exists(output_file),
    quote = FALSE,
    na = ""
  )
}



append_iteration_inputs <- function(logit_EUR_files){
  
  for (i in logit_EUR_files){
    l <- get_csv_info(i)
    df_i <- l$df
    name <- tools::file_path_sans_ext(basename(i))
    dir.create(
      file.path(thisScript_path, "Data", "inputs"),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    append_input(
      df_i,
      file.path(
        thisScript_path,
        "Data",
        "inputs",
        paste0(name, ".csv")
      )
    )
    
  }
  
}


# append_iteration_results <- function(data_dir = file.path(getwd(), "Data")) {
#   
#   source_files <- list.files(
#     data_dir,
#     pattern = "0\\.csv$",
#     full.names = TRUE
#   )
#   
#   ## Leer y validar todos los archivos primero
#   dfs <- vector("list", length(source_files))
#   
#   for (i in seq_along(source_files)) {
#     
#     source_file <- source_files[i]
#     
#     lines <- readLines(source_file, warn = FALSE)
#     
#     if (length(lines) < 2 ||
#         grepl("had error", lines[1], ignore.case = TRUE)) {
#       
#       stop(
#         sprintf(
#           "GCAM produjo un resultado inválido en '%s'. No se guardará esta iteración.",
#           basename(source_file)
#         )
#       )
#     }
#     
#     df <- read.csv(
#       source_file,
#       check.names = FALSE,
#       skip = 1
#     )
#     
#     df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
#     
#     if (nrow(df) == 0) {
#       stop(
#         sprintf(
#           "La consulta '%s' no devolvió filas. No se guardará esta iteración.",
#           basename(source_file)
#         )
#       )
#     }
#     
#     dfs[[i]] <- df
#   }
#   
#   ## Si hemos llegado aquí, todos los archivos son válidos
#   
#   for (i in seq_along(source_files)) {
#     
#     source_file <- source_files[i]
#     df <- dfs[[i]]
#     
#     target_file <- sub("0\\.csv$", ".csv", source_file)
#     
#     if (!file.exists(target_file)) {
#       iteration <- 1
#     } else {
#       old <- read.csv(target_file, check.names = FALSE)
#       iteration <- max(old$iteration, na.rm = TRUE) + 1
#     }
#     
#     df$iteration <- iteration
#     
#     write.table(
#       df,
#       file = target_file,
#       sep = ",",
#       row.names = FALSE,
#       col.names = !file.exists(target_file),
#       append = file.exists(target_file),
#       quote = TRUE
#     )
#   }
#   
#   invisible(TRUE)
# }


append_iteration_results <- function(data_dir = file.path(getwd(), "Data")) {
  
  source_files <- list.files(
    data_dir,
    pattern = "0\\.csv$",
    full.names = TRUE
  )
  
  for (source_file in source_files) {
    
    ## Comprobar que el archivo es válido
    lines <- tryCatch(
      readLines(source_file, warn = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(lines) ||
        length(lines) < 2 ||
        grepl("had error", lines[1], ignore.case = TRUE)) {
      
      warning(
        sprintf(
          "Se omite '%s' porque el resultado es inválido.",
          basename(source_file)
        ),
        call. = FALSE
      )
      
      next
    }
    
    ## Leer el CSV
    df <- tryCatch(
      read.csv(
        source_file,
        check.names = FALSE,
        skip = 1
      ),
      error = function(e) NULL
    )
    
    if (is.null(df)) {
      
      warning(
        sprintf(
          "No se pudo leer '%s'. Se omite.",
          basename(source_file)
        ),
        call. = FALSE
      )
      
      next
    }
    
    ## Eliminar columnas completamente vacías
    df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
    
    ## Si no hay datos, pasar al siguiente archivo
    if (nrow(df) == 0) {
      
      warning(
        sprintf(
          "La consulta '%s' no devolvió filas. Se omite.",
          basename(source_file)
        ),
        call. = FALSE
      )
      
      next
    }
    
    target_file <- sub("0\\.csv$", ".csv", source_file)
    
    if (!file.exists(target_file)) {
      
      iteration <- 1
      
    } else {
      
      old <- tryCatch(
        read.csv(target_file, check.names = FALSE),
        error = function(e) NULL
      )
      
      if (is.null(old) ||
          !"iteration" %in% names(old) ||
          nrow(old) == 0) {
        
        iteration <- 1
        
      } else {
        
        iteration <- max(old$iteration, na.rm = TRUE) + 1
        
      }
    }
    
    df$iteration <- iteration
    
    write.table(
      df,
      file = target_file,
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(target_file),
      append = file.exists(target_file),
      quote = TRUE
    )
  }
  
  invisible(TRUE)
}


delete_iteration_csvs <- function(data_dir = file.path(getwd(), "Data")) {
  
  files <- list.files(
    data_dir,
    pattern = "0\\.csv$",
    full.names = TRUE
  )
  
  if (length(files) > 0) {
    file.remove(files)
  }
  
  invisible(NULL)
}

