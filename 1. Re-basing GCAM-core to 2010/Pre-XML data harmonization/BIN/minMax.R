inputs_2021_xml <- load('inputs_xml_2021.RData')
inputs_2021_xml <- inputs


results <- data.frame(
  chunk = character(),
  df_name = character(),
  nrow_diff = integer(),
  startYear = numeric(),
  endYear = numeric(),
  stringsAsFactors = FALSE
)

for (chunk in names(inputs_2021_xml)) {
  chunk_i <- inputs_2021_xml[[chunk]]
  
  for (df in names(chunk_i)) {  
    df_i <- chunk_i[[df]]
    startYear <- if ("year" %in% names(df_i)) min(df_i$year, na.rm = TRUE) else NA
    endYear<- if ("year" %in% names(df_i)) max(df_i$year, na.rm = TRUE) else NA
      
      
      results <- rbind(
        results,
        data.frame(
          chunk = chunk,
          df_name = df,
          startYear = startYear,
          endYear = endYear,
          stringsAsFactors = FALSE
        )
      )
    }
  }
