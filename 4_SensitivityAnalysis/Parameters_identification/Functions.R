search_string <- function(
    content_patterns = NULL,
    filename_patterns = NULL,
    dir_chunks = "C:/GCAM/Nacho/gcam_europe/input/gcamdata/R",
    and = TRUE
) {
  
  files <- list.files(
    path = dir_chunks,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  ## Filtrar por nombre de archivo
  if (!is.null(filename_patterns)) {
    
    keep <- sapply(files, function(f) {
      x <- basename(f)
      
      if (and) {
        all(sapply(filename_patterns, grepl, x))
      } else {
        any(sapply(filename_patterns, grepl, x))
      }
    })
    
    files <- files[keep]
  }
  
  ## Filtrar por contenido
  if (!is.null(content_patterns)) {
    
    keep <- sapply(files, function(f) {
      
      txt <- readLines(f, warn = FALSE)
      
      if (and) {
        all(sapply(content_patterns, function(p) any(grepl(p, txt))))
      } else {
        any(sapply(content_patterns, function(p) any(grepl(p, txt))))
      }
      
    })
    
    files <- files[keep]
  }
  
  return(files)
}


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