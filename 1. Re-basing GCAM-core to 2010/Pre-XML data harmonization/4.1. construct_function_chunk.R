hindcasting_2010 <- function() {
  cat(">> [Hindcasting] Iniciando función...\n")
  
  # mapping de módulos a chunks
  module_mapping <- read.csv(
    'C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis/module_mapping.csv'
  )
  
  # new_chunks_2010 como variable global
  if (!exists("new_chunks_2010")) {
    cat(">> [Hindcasting] Cargando new_chunks_2010.RData...\n")
    load('C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/1. Re-basing GCAM-core to 2010/Pre-XML data harmonization/new_chunks_2010.RData')
    cat(">> [Hindcasting] new_chunks_2010 cargado, chunks:", length(new_chunks_2010), "\n")
  }
  
  # nombre del módulo
  module_fn <- as.character(sys.call(-1)[[1]])
  module_fn <- gsub(".*:::", "", module_fn)
  module_fn <- trimws(module_fn) 
  cat(">> [Hindcasting] Módulo que llama:", module_fn, "\n")
  
  # obtenemos el chunk correspondiente
  chunk_i <- module_mapping$file[module_mapping$module == module_fn]
  if (length(chunk_i) != 1) {
    warning("No se encontró o hay más de un chunk para este módulo: ", module_fn)
    return(NULL)
  } else {
    cat(">> [Hindcasting] Chunk asignado:", chunk_i, "\n")
  }
  
  # inputs reales del módulo
  real_inputs <- get(module_fn)(driver.DECLARE_INPUTS)
  cat(">> [Hindcasting] Inputs declarados por el módulo:", paste(real_inputs, collapse = ", "), "\n")
  
  # entorno del módulo actual
  mod_env <- parent.frame()
  
  # función para convertir tibbles vacíos a NULL
  empty_to_null <- function(df) {
  if (is.data.frame(df) && nrow(df) > 0 && all(sapply(df, function(x) all(is.na(x))))) {
    return(NULL)
  }
  df
}
  
  # sobrescribir solo los inputs que existan en el chunk
  for (df_i in intersect(real_inputs, names(new_chunks_2010[[chunk_i]]))) {
    df_ <- new_chunks_2010[[chunk_i]][[df_i]]
    df_ <- empty_to_null(df_)  # <-- aquí convertimos NA a NULL si corresponde
    assign(df_i, df_, envir = mod_env)
    cat("   [Hindcasting] Input sobrescrito:", df_i, "\n")
  }
  
  cat(">> [Hindcasting] Función completada para módulo:", module_fn, "\n\n")
  
  # =========================================================
  # Hindcasting constants injected into module environment
  # =========================================================
  
  HISTORICAL_YEARS        <- 1971:2010
  
  FUTURE_YEARS            <- (max(HISTORICAL_YEARS)+1):2100
  FUTURE_YEARS[FUTURE_YEARS == 2020] <- 2021
  FUTURE_YEARS <- unique(FUTURE_YEARS)
  
  MODEL_BASE_YEARS        <- unique(c(1975, 1990, 2005, 2010, max(HISTORICAL_YEARS)))
  MODEL_FINAL_BASE_YEAR   <- max(MODEL_BASE_YEARS)
  
  MODEL_FUTURE_YEARS      <- seq(2015, 2100, 5)
  MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS == 2020] <- 2021
  
  if (min(MODEL_FUTURE_YEARS) <= max(HISTORICAL_YEARS)) {
    stop("ERROR: Model future years overlap historial years in constants")
  }
  
  if (!(all(MODEL_FUTURE_YEARS %in% FUTURE_YEARS))) {
    stop("ERROR: Model future years not present in future years")
  }
  
  MODEL_YEARS <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)
  
  YEAR_RECODE <- c(
    "start-year" = min(MODEL_BASE_YEARS),
    "final-calibration-year" = MODEL_FINAL_BASE_YEAR,
    "final-historical-year" = as.numeric(max(HISTORICAL_YEARS)),
    "initial-future-year" = min(MODEL_FUTURE_YEARS),
    "initial-nonhistorical-year" = min(MODEL_YEARS[MODEL_YEARS > max(HISTORICAL_YEARS)]),
    "end-year" = max(MODEL_FUTURE_YEARS)
  )
  
  # inject into module environment (NOT global)
  const_list <- list(
    HISTORICAL_YEARS = HISTORICAL_YEARS,
    FUTURE_YEARS = FUTURE_YEARS,
    MODEL_BASE_YEARS = MODEL_BASE_YEARS,
    MODEL_FINAL_BASE_YEAR = MODEL_FINAL_BASE_YEAR,
    MODEL_FUTURE_YEARS = MODEL_FUTURE_YEARS,
    MODEL_YEARS = MODEL_YEARS,
    YEAR_RECODE = YEAR_RECODE
  )
  
  for (nm in names(const_list)) {
    assign(nm, const_list[[nm]], envir = mod_env)
  }
  
}


