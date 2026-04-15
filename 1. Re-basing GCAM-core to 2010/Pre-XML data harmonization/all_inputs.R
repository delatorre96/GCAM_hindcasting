comparing_inputs <- function(gcam_paths, chunks_inputs, path2save, 
                             drake1 = FALSE, drake2 = FALSE, names_gcams ,final_name = 'inputs'){
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  all_inputs <- list()

  for (i in seq_along(gcam_paths)) {
    setwd(gcam_paths[i])
    
    devtools::load_all()
    
    if (i == 1 && drake1) driver_drake()
    if (i == 2 && drake2) driver_drake()
    
    setwd("R")
    
    inputs <- list()
    
    for (chunk in chunks_inputs) {
      fun_env <- new.env()
      source(chunk, local = fun_env)
      
      fn_name <- ls(fun_env)[sapply(ls(fun_env), function(x) is.function(fun_env[[x]]))]
      
      all_data <- load_from_cache(inputs_of(fn_name))
      
      inputs[[chunk]] <- all_data
    }

    all_inputs[[names_gcams[i]]] <- inputs
  }
  
  save(all_inputs, file = file.path(paste0(path2save,'/',final_name, ".RData")))
}



all_equal <- function(all_inputs, names_gcams){
  
  comparison_results <- data.frame(
    chunk = character(),
    dataframe = character(),
    identical = logical(),
    diff_columns = I(list()),   # lista para guardar vectores
    stringsAsFactors = FALSE
  )
  
  for (chunk_name in names(all_inputs[[names_gcams[1]]])) {
    
    df_names <- names(all_inputs[[names_gcams[1]]][[chunk_name]])
    
    for (df_name in df_names) {
      
      df_2010 <- all_inputs[[names_gcams[1]]][[chunk_name]][[df_name]]
      
      if (!df_name %in% names(all_inputs[[names_gcams[2]]][[chunk_name]])) {
        is_identical <- FALSE
        diff_cols <- colnames(df_2010)   # todas las columnas “diferentes” porque no existe en el otro año
      } else {
        df_2021 <- all_inputs[[names_gcams[2]]][[chunk_name]][[df_name]]
        is_identical <- isTRUE(all.equal(df_2010, df_2021, check.attributes = FALSE))
        
        if (is_identical) {
          diff_cols <- character(0)
        } else {
          # Comparar columna a columna
          common_cols <- intersect(colnames(df_2010), colnames(df_2021))
          diff_cols <- common_cols[!sapply(common_cols, function(col) {
            isTRUE(all.equal(df_2010[[col]], df_2021[[col]], check.attributes = FALSE))
          })]
        }
      }
      
      comparison_results <- rbind(comparison_results, data.frame(
        chunk = chunk_name,
        dataframe = df_name,
        identical = is_identical,
        diff_columns = I(list(diff_cols)),  # envolver en I(list()) para mantener vector en celda
        stringsAsFactors = FALSE
      ))
    }
  }
  comparison_results$diff_columns_str <- sapply(comparison_results$diff_columns, function(x) paste(x, collapse = ", "))
  comparison_results$diff_columns <- NULL
  return(comparison_results)
}

get_diffs <- function(all_inputs, names_gcams){
  
  comparison_results <- data.frame(
    chunk = character(),
    dataframe = character(),
    identical = logical(),
    diff_columns = character(),
    n_diff_columns = integer(),
    single_numeric_diff_zero = logical(),
    stringsAsFactors = FALSE
  )
  
  for (chunk_name in names(all_inputs[[names_gcams[1]]])) {
    
    df_names <- names(all_inputs[[names_gcams[1]]][[chunk_name]])
    
    for (df_name in df_names) {
      
      df_2010 <- all_inputs[[names_gcams[1]]][[chunk_name]][[df_name]]
      
      if (!df_name %in% names(all_inputs[[names_gcams[2]]][[chunk_name]])) {
        is_identical <- FALSE
        diff_cols <- colnames(df_2010)
      } else {
        df_2021 <- all_inputs[[names_gcams[2]]][[chunk_name]][[df_name]]
        is_identical <- isTRUE(all.equal(df_2010, df_2021, check.attributes = FALSE))
        
        if (is_identical) {
          diff_cols <- character(0)
        } else {
          common_cols <- intersect(colnames(df_2010), colnames(df_2021))
          diff_cols <- common_cols[!sapply(common_cols, function(col) {
            isTRUE(all.equal(df_2010[[col]], df_2021[[col]], check.attributes = FALSE))
          })]
        }
      }
      
      n_diff <- length(diff_cols)
      diff_cols_str <- if (n_diff > 0) paste(diff_cols, collapse = ", ") else ""
      
      single_numeric_zero <- NA
      if (n_diff == 1) {
        col_name <- diff_cols[1]
        # Comprobar si es numérica
        if (is.numeric(df_2010[[col_name]]) && is.numeric(df_2021[[col_name]])) {
          diff_vals <- df_2010[[col_name]] - df_2021[[col_name]]
          # Determinar si alguno de los dos números es 0
          single_numeric_zero <- any(df_2010[[col_name]] == 0 | df_2021[[col_name]] == 0)
        }
      }
      
      comparison_results <- rbind(comparison_results, data.frame(
        chunk = chunk_name,
        dataframe = df_name,
        identical = is_identical,
        diff_columns = diff_cols_str,
        n_diff_columns = n_diff,
        single_numeric_diff_zero = single_numeric_zero,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(comparison_results)
}


compute_row_diffs <- function(all_inputs, names_gcams, comparison_results_false) {
  
  # Lista para guardar resultados por data frame
  row_diffs_list <- list()
  
  for (i in seq_len(nrow(comparison_results_false))) {
    
    chunk_name <- comparison_results_false$chunk[i]
    df_name    <- comparison_results_false$dataframe[i]
    
    # Extraer la columna que difiere
    diff_col <- comparison_results_false$diff_columns[i]
    
    df_2010 <- all_inputs[[names_gcams[1]]][[chunk_name]][[df_name]]
    df_2021 <- all_inputs[[names_gcams[2]]][[chunk_name]][[df_name]]
    
    # Calcular diferencia fila por fila
    if (diff_col %in% colnames(df_2010) && diff_col %in% colnames(df_2021)) {
      diff_values <- df_2010[[diff_col]] - df_2021[[diff_col]]
      
      # Guardar en un data frame con referencia
      row_diffs <- data.frame(
        chunk = chunk_name,
        dataframe = df_name,
        row_index = seq_len(nrow(df_2010)),
        col_name = diff_col,
        value_2010 = df_2010[[diff_col]],
        value_2021 = df_2021[[diff_col]],
        diff = diff_values,
        zero_in_either = df_2010[[diff_col]] == 0 | df_2021[[diff_col]] == 0,
        stringsAsFactors = FALSE
      )
      
      # Guardar en lista
      row_diffs_list[[paste0(chunk_name, "_", df_name)]] <- row_diffs
    }
  }
  
  # Combinar todos los data frames en uno solo
  do.call(rbind, row_diffs_list)
}

compute_row_values_all <- function(all_inputs, names_gcams, comparison_results_false) {
  
  # Lista para guardar resultados por data frame
  row_values_list <- list()
  
  for (i in seq_len(nrow(comparison_results_false))) {
    
    chunk_name <- comparison_results_false$chunk[i]
    df_name    <- comparison_results_false$dataframe[i]
    
    df_2010 <- all_inputs[[names_gcams[1]]][[chunk_name]][[df_name]]
    df_2021 <- all_inputs[[names_gcams[2]]][[chunk_name]][[df_name]]
    
    # Revisar que los data frames tengan el mismo número de filas
    n_rows <- max(nrow(df_2010), nrow(df_2021))
    
    # Extender con NA si algún df tiene menos filas
    if (nrow(df_2010) < n_rows) df_2010[(nrow(df_2010)+1):n_rows, ] <- NA
    if (nrow(df_2021) < n_rows) df_2021[(nrow(df_2021)+1):n_rows, ] <- NA
    
    # Para cada columna, crear un data frame combinando valores
    for (col_name in union(colnames(df_2010), colnames(df_2021))) {
      
      value_2010 <- if (col_name %in% colnames(df_2010)) df_2010[[col_name]] else rep(NA, n_rows)
      value_2021 <- if (col_name %in% colnames(df_2021)) df_2021[[col_name]] else rep(NA, n_rows)
      
      row_values <- data.frame(
        chunk = chunk_name,
        dataframe = df_name,
        row_index = seq_len(n_rows),
        col_name = col_name,
        value_2010 = value_2010,
        value_2021 = value_2021,
        stringsAsFactors = FALSE
      )
      
      row_values_list[[paste0(chunk_name, "_", df_name, "_", col_name)]] <- row_values
    }
  }
  
  # Combinar todos los data frames en uno solo
  do.call(rbind, row_values_list)
}


comparing_outputs <- function(gcam_paths, chunks_inputs, path2save, 
                             drake1 = FALSE, drake2 = FALSE, names_gcams ,final_name = 'outputs'){
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  all_outputs <- list()

  for (i in seq_along(gcam_paths)) {
    setwd(gcam_paths[i])
    
    devtools::load_all()
    
    if (i == 1 && drake1) driver_drake()
    if (i == 2 && drake2) driver_drake()
    
    setwd("R")
    
    outputs <- list()
    
    for (chunk in chunks_inputs) {
      fun_env <- new.env()
      source(chunk, local = fun_env)
      
      fn_name <- ls(fun_env)[sapply(ls(fun_env), function(x) is.function(fun_env[[x]]))]
      
      all_data <- load_from_cache(outputs_of(fn_name))
      
      outputs[[chunk]] <- all_data
    }

    all_outputs[[names_gcams[i]]] <- outputs
  }
  
  save(all_outputs, file = file.path(paste0(path2save,'/',final_name, ".RData")))
}


get_allData <- function(gcam_path, do_driver = FALSE, save_input_data = FALSE, path2save, files_xml,csvs_to_xml_name = 'csvs_to_xml'){
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(gcam_path)
  devtools::load_all()
  if (do_driver){
    driver_drake()
  }
  
  setwd('R')
  csvs_to_xml <- list()
  files_xml <- files_xml[grepl("\\.R$", files_xml)]
  for (file in files_xml){
    if (!(file %in% names(csvs_to_xml))){
      env <- new.env()
      source(file, local = env)
      r_objects <- ls(env)
      module <- r_objects[grep("module", r_objects)]
      all_data <- load_from_cache(inputs_of(module))
      csvs_to_xml[[file]] <- all_data
    }
  }
  if (save_input_data){
    save(csvs_to_xml, file = paste0(path2save,"/",csvs_to_xml_name,".RData"))
  }
  return(csvs_to_xml)
}

# 
# analyze_year_columns <- function(csvs_to_xml) {
#   
#   results_list <- list()
#   idx <- 1
#   
#   for (chunk_name in names(csvs_to_xml)) {
#     
#     chunk <- csvs_to_xml[[chunk_name]]
#     
#     for (df_name in names(chunk)) {
#       
#       df <- chunk[[df_name]]
#       col_names <- names(df)
#       
#       # columna exacta "year"
#       has_year <- "year" %in% col_names
#       
#       if (has_year) {
#         year_vals <- df[["year"]]
#         min_year <- suppressWarnings(min(year_vals, na.rm = TRUE))
#         max_year <- suppressWarnings(max(year_vals, na.rm = TRUE))
#         
#         if (is.infinite(min_year)) min_year <- NA
#         if (is.infinite(max_year)) max_year <- NA
#       } else {
#         min_year <- NA
#         max_year <- NA
#       }
#       
#       # columnas que contienen "year" pero no exactamente "year"
#       contains_year_cols <- col_names[grepl("year", col_names, ignore.case = TRUE) & col_names != "year"]
#       has_contains_year <- length(contains_year_cols) > 0
#       
#       if (has_contains_year) {
#         names_col_contains_year <- paste(contains_year_cols, collapse = ", ")
#         
#         # para simplificar asumimos que hay una sola columna principal, si hay más, tomamos el primer conjunto
#         first_col <- contains_year_cols[1]
#         year_vals_contains <- df[[first_col]]
#         min_contains_year <- suppressWarnings(min(year_vals_contains, na.rm = TRUE))
#         max_contains_year <- suppressWarnings(max(year_vals_contains, na.rm = TRUE))
#         
#         if (is.infinite(min_contains_year)) min_contains_year <- NA
#         if (is.infinite(max_contains_year)) max_contains_year <- NA
#         
#       } else {
#         names_col_contains_year <- NA
#         min_contains_year <- NA
#         max_contains_year <- NA
#       }
#       
#       # guardar resultado
#       results_list[[idx]] <- data.frame(
#         chunk = chunk_name,
#         dataframe = df_name,
#         col_year = has_year,
#         min_year = min_year,
#         max_year = max_year,
#         contains_year = has_contains_year,
#         names_col_contains_year = names_col_contains_year,
#         min_contains_year = min_contains_year,
#         max_contains_year = max_contains_year,
#         stringsAsFactors = FALSE
#       )
#       
#       idx <- idx + 1
#     }
#   }
#   
#   do.call(rbind, results_list)
# }

analyze_exact_year_columns <- function(csvs_to_xml) {
  
  results_list <- list()
  idx <- 1
  
  for (chunk_name in names(csvs_to_xml)) {
    
    chunk <- csvs_to_xml[[chunk_name]]
    
    for (df_name in names(chunk)) {
      
      df <- chunk[[df_name]]
      
      # solo si existe la columna exacta "year"
      if ("year" %in% names(df)) {
        year_vals <- df[["year"]]
        
        min_y <- suppressWarnings(min(year_vals, na.rm = TRUE))
        max_y <- suppressWarnings(max(year_vals, na.rm = TRUE))
        
        if (is.infinite(min_y)) min_y <- NA
        if (is.infinite(max_y)) max_y <- NA
        
        all_years <- paste(sort(unique(year_vals)), collapse = ", ")
        
        results_list[[idx]] <- data.frame(
          chunk = chunk_name,
          dataframe = df_name,
          min_year = min_y,
          max_year = max_y,
          all_years = all_years,
          stringsAsFactors = FALSE
        )
        
        idx <- idx + 1
      }
    }
  }
  
  do.call(rbind, results_list)
}



analyze_year_columns_contains <- function(csvs_to_xml) {
  
  results_list <- list()
  idx <- 1
  
  for (chunk_name in names(csvs_to_xml)) {
    
    chunk <- csvs_to_xml[[chunk_name]]
    
    for (df_name in names(chunk)) {
      
      df <- chunk[[df_name]]
      col_names <- names(df)
      
      # columnas que contienen "year"
      year_cols <- col_names[grepl("year", col_names, ignore.case = TRUE)]
      
      # solo procesar si hay alguna
      if (length(year_cols) > 0) {
        for (col in year_cols) {
          year_vals <- df[[col]]
          min_y <- suppressWarnings(min(year_vals, na.rm = TRUE))
          max_y <- suppressWarnings(max(year_vals, na.rm = TRUE))
          
          if (is.infinite(min_y)) min_y <- NA
          if (is.infinite(max_y)) max_y <- NA
          
          all_years <- paste(sort(unique(year_vals)), collapse = ", ")
          
          results_list[[idx]] <- data.frame(
            chunk = chunk_name,
            dataframe = df_name,
            name_col_year = col,
            min_year = min_y,
            max_year = max_y,
            all_years = all_years,
            stringsAsFactors = FALSE
          )
          
          idx <- idx + 1
        }
      }
    }
  }
  
  do.call(rbind, results_list)
}
