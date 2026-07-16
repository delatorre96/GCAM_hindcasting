library(xml2)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

set_gcam_paths <- function(gcam_path) {
  #Exmple:
  #dir_gcamdata <- "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata"
  dir_gcam <<- gcam_path
  config_file <<-  paste0(gcam_path,'/exe/configuration.xml')
  run_gcam_file <<- paste0(gcam_path,'/exe/run-gcam.bat')
  dir_gcamdata <<- paste0(gcam_path,'/input/gcamdata')
  dir_xml <<- paste0(gcam_path,'/input/gcamdata/xml')

  print(dir_gcam)
  print(config_file)
  print(run_gcam_file)
  print(dir_gcamdata)
  print(dir_xml)
}

get_xml_files <- function(config_path) {
  
  # Leer el XML
  doc <- read_xml(config_path)
  
  # Extraer todos los Value de ScenarioComponents
  paths <- xml_text(xml_find_all(doc, "//ScenarioComponents/Value"))
  
  # Quedarse sólo con los archivos de gcamdata/xml
  paths <- paths[grepl("gcamdata/xml", paths)]
  
  # Extraer únicamente el nombre del archivo
  xml_files <- basename(paths)
  
  return(xml_files)
}

get_xmls_with_logit <- function(xml_files, xml_dir) {
  Filter(function(f) {
    doc <- read_xml(file.path(xml_dir, f))
    length(xml_find_all(doc, ".//logit-exponent")) > 0
  }, xml_files)
}


extraer_logits <- function(xml_file){
  
  library(xml2)
  
  doc <- read_xml(xml_file)
  
  logits <- xml_find_all(doc, ".//logit-exponent")
  
  salida <- vector("list", length(logits))
  
  for(i in seq_along(logits)){
    
    logit <- logits[[i]]
    
    padres <- xml_parents(logit)
    
    region <- NA
    supplysector <- NA
    subsector <- NA
    level <- NA
    
    for(p in padres){
      
      etiqueta <- xml_name(p)
      
      if(etiqueta == "region"){
        region <- xml_attr(p, "name")
      }
      
      if(etiqueta == "supplysector"){
        supplysector <- xml_attr(p, "name")
        level <- "supplysector"
      }
      
      if(etiqueta == "subsector"){
        subsector <- xml_attr(p, "name")
        level <- "subsector"
      }
      
    }
    
    salida[[i]] <- data.frame(
      
      id = i,
      
      region = region,
      
      supplysector = supplysector,
      
      subsector = subsector,
      
      level = level,
      
      fillout = xml_attr(logit,"fillout"),
      
      year = as.numeric(xml_attr(logit,"year")),
      
      logit = as.numeric(xml_text(logit)),
      
      xpath = xml_path(logit),
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  do.call(rbind, salida)
  
}




extraer_logits_anyXML<- function(xml_file){
  
  library(xml2)
  
  doc <- read_xml(xml_file)
  
  logits <- xml_find_all(doc, ".//logit-exponent")
  
  salida <- vector("list", length(logits))
  
  for(i in seq_along(logits)){
    
    logit <- logits[[i]]
    
    padres <- xml_parents(logit)
    
    region <- NA
    supplysector <- NA
    subsector <- NA
    level <- NA
    
    for(p in padres){
      
      etiqueta <- xml_name(p)
      
      if(etiqueta == "region"){
        region <- xml_attr(p,"name")
      }
      
      if(etiqueta == "supplysector"){
        supplysector <- xml_attr(p,"name")
        level <- "supplysector"
      }
      
      if(etiqueta == "subsector"){
        subsector <- xml_attr(p,"name")
        level <- "subsector"
      }
      
    }
    
    salida[[i]] <- data.frame(
      
      xml_file = basename(xml_file),
      
      id = i,
      
      region = region,
      
      supplysector = supplysector,
      
      subsector = subsector,
      
      level = level,
      
      fillout = xml_attr(logit,"fillout"),
      
      year = as.numeric(xml_attr(logit,"year")),
      
      logit = as.numeric(xml_text(logit)),
      
      xpath = xml_path(logit),
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  do.call(rbind,salida)
  
}





insertar_logits <- function(xml_entrada,
                            tabla_logits,
                            xml_salida){
  
  library(xml2)
  
  doc <- read_xml(xml_entrada)
  
  for(i in seq_len(nrow(tabla_logits))){
    
    nodo <- xml_find_first(doc,
                           tabla_logits$xpath[i])
    
    if(inherits(nodo,"xml_missing"))
      next
    
    nuevo <- xml_add_sibling(
      nodo,
      "logit-exponent",
      .where="after",
      as.character(tabla_logits$logit[i])
    )
    
    xml_set_attr(
      nuevo,
      "fillout",
      as.character(tabla_logits$fillout[i])
    )
    
    xml_set_attr(
      nuevo,
      "year",
      as.character(tabla_logits$year[i])
    )
    
  }
  
  write_xml(doc,
            xml_salida,
            options="format")
  
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




append_input <- function(df, output_file) {
  dir.create(
    file.path(thisScript_path, "Data", "inputs"),
    recursive = TRUE,
    showWarnings = FALSE
  )
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

