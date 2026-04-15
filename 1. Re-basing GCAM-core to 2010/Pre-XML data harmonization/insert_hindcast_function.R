insert_hindcast_data <- function(){
if (!exists("mapping_treatment_df")) {
  mapping_treatment_df <<- read.csv('C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis/mapping_dataframes_years.csv')
}
for (file in module_mapping$file) {
  lines <- readLines(file)
  
  # Buscar todas las líneas que contienen get_data
  pos_get <- grep("get_data\\s*\\(", lines)
  
  if (length(pos_get) == 0) {
    warning("No se encontró ninguna llamada a get_data en ", file, "; se salta.")
    next
  }
  
  # Tomar la última ocurrencia
  last_get <- max(pos_get)
  
  # Insertar hindcasting_2010() después de esa línea
  lines <- append(lines, "hindcasting_2010()", after = last_get)
  
  # Sobrescribir el archivo
  writeLines(lines, file)
  
  #cat(">> [Script] hindcasting_2010() insertado en", file, "después de la línea", last_get, "\n")
}
}
