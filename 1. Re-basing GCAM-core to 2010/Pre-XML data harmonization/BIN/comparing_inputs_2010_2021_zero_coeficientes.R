load('C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/1. Re-basing GCAM-core to 2010/Pre-XML data harmonization/csvs_to_xml_2010.RData')
csvs_to_xml_2010 <- csvs_to_xml
load('C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/1. Re-basing GCAM-core to 2010/Pre-XML data harmonization/csvs_to_xml_2021.RData')
csvs_to_xml_2021 <- csvs_to_xml
rm(csvs_to_xml)


library(dplyr)
library(purrr)

compare_zero_patterns <- function(list_2010, list_2021) {
  
  map_dfr(names(list_2010), function(chunk_name) {
    
    chunk_2010 <- list_2010[[chunk_name]]
    chunk_2021 <- list_2021[[chunk_name]]
    
    map_dfr(names(chunk_2010), function(df_name) {
      
      df_2010 <- chunk_2010[[df_name]]
      df_2021 <- chunk_2021[[df_name]]
      
      # columnas numéricas comunes
      num_cols <- intersect(
        names(df_2010)[sapply(df_2010, is.numeric)],
        names(df_2021)[sapply(df_2021, is.numeric)]
      )
      
      # caso sin columnas numéricas
      if (length(num_cols) == 0) {
        return(tibble(
          chunk = chunk_name,
          data_frame = df_name,
          if_zeros = FALSE,
          amountZeros = 0,
          colWithZeros = NA_character_
        ))
      }
      
      # detectar columnas que cumplen condición estricta
      cols_with_zeros <- num_cols[
        map_lgl(num_cols, function(col) {
          has_zero_2021 <- any(df_2021[[col]] == 0, na.rm = TRUE)
          has_zero_2010 <- any(df_2010[[col]] == 0, na.rm = TRUE)
          
          (!has_zero_2021) & has_zero_2010
        })
      ]
      
      # contar ceros en 2010 solo en esas columnas
      amount <- if (length(cols_with_zeros) > 0) {
        sum(map_int(cols_with_zeros, function(col) {
          sum(df_2010[[col]] == 0, na.rm = TRUE)
        }))
      } else {
        0
      }
      
      tibble(
        chunk = chunk_name,
        data_frame = df_name,
        if_zeros = length(cols_with_zeros) > 0,
        amountZeros = amount,
        colWithZeros = if (length(cols_with_zeros) > 0) {
          paste(cols_with_zeros, collapse = ", ")
        } else {
          NA_character_
        }
      )
      
    })
    
  })
}
result <- compare_zero_patterns(csvs_to_xml_2010, csvs_to_xml_2021)
View(result)


compare_na_patterns <- function(list_2010, list_2021) {
  
  map_dfr(names(list_2010), function(chunk_name) {
    
    chunk_2010 <- list_2010[[chunk_name]]
    chunk_2021 <- list_2021[[chunk_name]]
    
    map_dfr(names(chunk_2010), function(df_name) {
      
      df_2010 <- chunk_2010[[df_name]]
      df_2021 <- chunk_2021[[df_name]]
      
      # columnas numéricas comunes
      num_cols <- intersect(
        names(df_2010)[sapply(df_2010, is.numeric)],
        names(df_2021)[sapply(df_2021, is.numeric)]
      )
      
      # caso sin columnas numéricas
      if (length(num_cols) == 0) {
        return(tibble(
          chunk = chunk_name,
          data_frame = df_name,
          if_NA = FALSE,
          amountNA = 0,
          colWithNA = NA_character_
        ))
      }
      
      # columnas donde:
      # 2010 tiene NA en alguna observación
      # y 2021 no tiene ningún NA en esa columna
      cols_with_na <- num_cols[
        map_lgl(num_cols, function(col) {
          has_na_2010 <- any(is.na(df_2010[[col]]))
          has_na_2021 <- any(is.na(df_2021[[col]]))
          
          has_na_2010 & (!has_na_2021)
        })
      ]
      
      # contar NAs en 2010 solo en esas columnas
      amount <- if (length(cols_with_na) > 0) {
        sum(map_int(cols_with_na, function(col) {
          sum(is.na(df_2010[[col]]))
        }))
      } else {
        0
      }
      
      tibble(
        chunk = chunk_name,
        data_frame = df_name,
        if_NA = length(cols_with_na) > 0,
        amountNA = amount,
        colWithNA = if (length(cols_with_na) > 0) {
          paste(cols_with_na, collapse = ", ")
        } else {
          NA_character_
        }
      )
      
    })
    
  })
}

result2 <- compare_na_patterns(csvs_to_xml_2010, csvs_to_xml_2021)
View(result2)
