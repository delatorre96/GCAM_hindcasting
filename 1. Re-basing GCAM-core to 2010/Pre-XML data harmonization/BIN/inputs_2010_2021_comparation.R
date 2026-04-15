inputs_2010 <- load('inputs_2010.RData')
inputs_2010 <- inputs
inputs_2021 <- load('inputs_2021.RData')
inputs_2021 <- inputs



results <- data.frame(
  chunk = character(),
  df_name = character(),
  nrow_diff = integer(),
  enYear_2010 = numeric(),
  enYear_2021 = numeric(),
  stringsAsFactors = FALSE
)

for (chunk in names(inputs_2010)) {
  chunk_2010 <- inputs_2010[[chunk]]
  chunk_2021 <- inputs_2021[[chunk]]
  
  for (df_i in names(chunk_2010)) {   
    df_2010 <- chunk_2010[[df_i]]
    df_2021 <- chunk_2021[[df_i]]
    
    # Comparar data frames
    if (!isTRUE(all.equal(df_2010, df_2021))) {
      
      # Diferencia de filas
      nrow_diff <- nrow(df_2021) - nrow(df_2010)
      
      # Buscar la columna de año si existe
      enYear_2010 <- if ("year" %in% names(df_2010)) max(df_2010$year, na.rm = TRUE) else NA
      enYear_2021 <- if ("year" %in% names(df_2021)) max(df_2021$year, na.rm = TRUE) else NA
      
      
      results <- rbind(
        results,
        data.frame(
          chunk = chunk,
          df_name = df_i,
          equal = FALSE,
          nrow_diff = nrow_diff,
          enYear_2010 = enYear_2010,
          enYear_2021 = enYear_2021,
          stringsAsFactors = FALSE
        )
      )
    }else{
      results <- rbind(
        results,
        data.frame(
          chunk = chunk,
          df_name = df_i,
          equal = TRUE,
          nrow_diff = NA,
          enYear_2010 = NA,
          enYear_2021 = NA,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}