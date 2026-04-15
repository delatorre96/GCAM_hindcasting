hindcasting_2010 <- function() {
  cat(">> [Hindcasting] Iniciando función...\n")
  
  # mapping de módulos a chunks
  module_mapping <- read.csv(
    'C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis/module_mapping.csv'
  )
  
  # new_chunks_2010 como variable global
  if (!exists("new_chunks_2010")) {
    cat(">> [Hindcasting] Cargando new_chunks_2010.RData...\n")
    load('C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis/new_chunks_2010.RData')
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
}
