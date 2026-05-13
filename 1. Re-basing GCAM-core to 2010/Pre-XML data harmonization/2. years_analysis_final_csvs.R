load('csvs_to_xml_2021.RData')
csvs_to_xml_2021 <- csvs_to_xml
load('csvs_to_xml_2010.RData')
csvs_to_xml_2010 <- csvs_to_xml
rm(csvs_to_xml)

library(dplyr)
library(purrr)

analyze_columns_with_year <- function(list_2010, list_2021) {
  
  results_list <- list()
  idx <- 1
  
  # asumimos que las listas tienen los mismos nombres de chunk y dataframes
  for (chunk_name in names(list_2010)) {
    
    chunk_2010 <- list_2010[[chunk_name]]
    chunk_2021 <- list_2021[[chunk_name]]
    
    for (df_name in names(chunk_2010)) {
      
      df_2010 <- chunk_2010[[df_name]]
      df_2021 <- chunk_2021[[df_name]]
      
      # columnas que contienen "year" en ambos
      year_cols <- names(df_2010)[grepl("year", names(df_2010), ignore.case = TRUE)]
      
      if (length(year_cols) > 0) {
        for (col in year_cols) {
          vals_2010 <- df_2010[[col]]
          vals_2021 <- df_2021[[col]]
          
          min_2010 <- suppressWarnings(min(vals_2010, na.rm = TRUE))
          max_2010 <- suppressWarnings(max(vals_2010, na.rm = TRUE))
          all_2010 <- paste(sort(unique(vals_2010)), collapse = ", ")
          
          min_2021 <- suppressWarnings(min(vals_2021, na.rm = TRUE))
          max_2021 <- suppressWarnings(max(vals_2021, na.rm = TRUE))
          all_2021 <- paste(sort(unique(vals_2021)), collapse = ", ")
          
          results_list[[idx]] <- data.frame(
            chunk = chunk_name,
            dataframe = df_name,
            name_col_year = col,
            min_year_2010 = ifelse(is.infinite(min_2010), NA, min_2010),
            max_year_2010 = ifelse(is.infinite(max_2010), NA, max_2010),
            all_years_2010 = all_2010,
            min_year_2021 = ifelse(is.infinite(min_2021), NA, min_2021),
            max_year_2021 = ifelse(is.infinite(max_2021), NA, max_2021),
            all_years_2021 = all_2021,
            stringsAsFactors = FALSE
          )
          
          idx <- idx + 1
        }
      }
    }
  }
  
  do.call(rbind, results_list)
}

results <- analyze_columns_with_year (csvs_to_xml_2010, csvs_to_xml_2021)

######################### gcam_2010 and gcam_2021 equal #########################

results <- results %>% mutate (are_equal = if_else(all_years_2010 == all_years_2021,TRUE, FALSE))

######################### Mapedo YEARS_RECODE #########################

important_years_2010 <- c(
  start_year = 1975,
  final_calibration_year = 2010,
  final_historical_year = 2010,
  initial_future_year = 2015,
  initial_nonhistorical_year = 2015,
  end_year = 2100
)

important_years_2021 <- c(
  start_year = 1975,
  final_calibration_year = 2021,
  final_historical_year = 2021,
  initial_future_year = 2025,
  initial_nonhistorical_year = 2025,
  end_year = 2100
)

map_gcam_year <- function(year_2010, year_2021) {
  
  # combinamos ambos
  if (!is.na(year_2010) & !is.na(year_2021)) {
    
    # start-year
    if (year_2010 == important_years_2010["start_year"] & year_2021 == important_years_2021["start_year"]) return("start-year")
    
    # final-calibration-year / final-historical-year
    if (year_2010 == important_years_2010["final_calibration_year"] & year_2021 == important_years_2021["final_calibration_year"]) return("final-calibration-year")
    
    # initial-future-year / initial-nonhistorical-year (preferimos initial-future-year)
    if (year_2010 == important_years_2010["initial_future_year"] & year_2021 == important_years_2021["initial_future_year"]) return("initial-future-year")
    
    # end-year
    if (year_2010 == important_years_2010["end_year"] & year_2021 == important_years_2021["end_year"]) return("end-year")
    
    # en caso de que sea final-historical-year o final-calibration-year mezclados
    if (year_2010 == important_years_2010["final_historical_year"] & year_2021 == important_years_2021["final_historical_year"]) return("final-calibration-year")
    
    # fallback
    return(NA)
  } else {
    return(NA)
  }
}


results$mapped_min_year <- mapply(map_gcam_year, 
                                  as.numeric(results$min_year_2010), 
                                  as.numeric(results$min_year_2021))

results$mapped_max_year <- mapply(map_gcam_year, 
                                  as.numeric(results$max_year_2010), 
                                  as.numeric(results$max_year_2021))



######################### same min and max year #########################
results <- results %>% mutate(same_min_max = if_else(min_year_2010 == max_year_2010 &
                                                       min_year_2021 == max_year_2021 &
                                                       mapped_min_year == mapped_max_year, TRUE, FALSE))
######################### serie from start-year to final-calibration-year #########################

results <- results %>%
  mutate(
    startYear_TO_finalCalibrationYear = !is.na(mapped_min_year) & !is.na(mapped_max_year) &
      mapped_min_year == 'start-year' & mapped_max_year == 'final-calibration-year'
  )

######################### serie from initial-future-year to end-year #########################

results <- results %>%
  mutate(
    intialFutureYear_TO_endYear = !is.na(mapped_min_year) & !is.na(mapped_max_year) &
      mapped_min_year == 'initial-future-year' & mapped_max_year == 'end-year'
  )

######################### serie from final-calibration-year to end-year #########################

results <- results %>%
  mutate(
    finalCalibrationYear_TO_endYear = !is.na(mapped_min_year) & !is.na(mapped_max_year) &
      mapped_min_year == 'final-calibration-year' & mapped_max_year == 'end-year'
  )

######################### Continuity #########################
parse_years <- function(x) {
  as.numeric(strsplit(x, ",\\s*")[[1]])
}
gcam_years <- c(
  1975, 1990, 2005, 2010, 2015, 2021,
  2025, 2030, 2035, 2040, 2045, 2050,
  2055, 2060, 2065, 2070, 2075, 2080,
  2085, 2090, 2095, 2100
)
is_continuous_gcam <- function(years) {
  # ordena la serie y filtra NAs por si acaso
  years <- sort(years)
  
  # encuentra posiciones en gcam_years
  idx <- match(years, gcam_years)
  
  # si alguno no está en gcam_years → no es válido
  if (any(is.na(idx))) return(FALSE)
  
  # verificar que los índices sean consecutivos
  all(diff(idx) == 1)
}
results <- results %>%
  rowwise() %>%
  mutate(
    continuousSeries_2021 = is_continuous_gcam(parse_years(all_years_2021)),
    continuousSeries_2010 = is_continuous_gcam(parse_years(all_years_2010)),
    continuousSeries = continuousSeries_2021 & continuousSeries_2010
  ) %>% select(-continuousSeries_2021, -continuousSeries_2010) %>%
  ungroup()

######################### Not_matching years #########################
results <- results %>%
  rowwise() %>%
  mutate(
    years_2010 = list(parse_years(all_years_2010)),
    years_2021 = list(parse_years(all_years_2021)),
    missing_in_2010 = paste(setdiff(years_2021, years_2010), collapse = ", "),
    missing_in_2021  = paste(setdiff(years_2010, years_2021), collapse = ", ")
  ) %>%
  ungroup() %>% select(-years_2010, -years_2021)


######################### Map treatments #########################
results <- results %>%
  mutate(
    treatment = case_when(
      # 1) Nada que hacer: series iguales o min/max coinciden
      are_equal ~ "copy_df",
      
      # 2) Recortar años sobrantes: missing en 2010 pero no en 2021
      missing_in_2021 == "" & missing_in_2010 != "" ~ "trim_years",
      
      # 3.1) Rellenar hacia atrás: missing en 2021 pero no en 2010
      missing_in_2021 != "" & missing_in_2010 == "" ~ "fill_missing_years",
      
      # 3.2) Intercambiar años: missing en ambas
      missing_in_2021 != "" & missing_in_2010 != "" ~ "swap_years",
      
      # Por si acaso algo queda sin cubrir
      TRUE ~ "unknown"
    )
  )

mapping_treatment_df <- results[c('chunk','dataframe','name_col_year','treatment','missing_in_2010','missing_in_2021')]
check <- mapping_treatment_df %>%
  group_by(chunk, dataframe) %>%
  summarise(
    other_treatments = setdiff(unique(treatment), "copy_df"),
    n_other = length(other_treatments),
    .groups = "drop"
  )
diferentTreatmentsInDF <- check %>% filter(n_other > 1) ##Para verificar que solo convive copy_df con cualquier otro tratamiento pero no dos tratamientos distintos a copy_df

if (nrow(diferentTreatmentsInDF) == 0){
  mapping_treatment_df<- mapping_treatment_df %>%
    group_by(chunk, dataframe) %>%
    filter(
      !(treatment == "copy_df" & n_distinct(treatment) > 1)
    ) %>%
    ungroup()
}


######################### analyse whether each non number columns group has a single year (if this is the case, the treatment is always substitution) #########################
mapping_treatment_df$distinct_years <- map_lgl(seq_len(nrow(mapping_treatment_df)), function(i) {
  row <- mapping_treatment_df[i, ]
  chunk_i <- row$chunk
  df_i <- row$dataframe
  col_year <- row$name_col_year
  
  df_2010 <- csvs_to_xml_2010[[chunk_i]][[df_i]]
  df_2021 <- csvs_to_xml_2021[[chunk_i]][[df_i]]
  
  num_cols_2010 <- setdiff(names(df_2010)[sapply(df_2010, is.numeric)], col_year)
  non_num_cols_2010 <- names(df_2010)[sapply(df_2010, is.character)]
  
  num_cols_2021 <- setdiff(names(df_2021)[sapply(df_2021, is.numeric)], col_year)
  non_num_cols_2021 <- names(df_2021)[sapply(df_2021, is.character)]
  
  # chequeamos 2010
  all_2010 <- df_2010 %>%
    select(-all_of(num_cols_2010)) %>%
    group_by(across(all_of(non_num_cols_2010))) %>%
    summarise(n_years = n_distinct(.data[[col_year]]), .groups = "drop") %>%
    pull(n_years) %>%
    all(. == 1)
  
  # chequeamos 2021
  all_2021 <- df_2021 %>%
    select(-all_of(num_cols_2021)) %>%
    group_by(across(all_of(non_num_cols_2021))) %>%
    summarise(n_years = n_distinct(.data[[col_year]]), .groups = "drop") %>%
    pull(n_years) %>%
    all(. == 1)
  
  # TRUE solo si ambos cumplen
  all_2010 && all_2021
})

mapping_treatment_df <- mapping_treatment_df %>% 
  mutate(treatment = if_else(distinct_years == TRUE & treatment == 'fill_missing_years', 'swap_years',treatment)) 

idx <- mapping_treatment_df$treatment == "swap_years" & mapping_treatment_df$missing_in_2010 == ""
mapping_treatment_df$missing_in_2010[idx] <- "2025"        
mapping_treatment_df$missing_in_2021[idx] <- "2015" 


write.csv(mapping_treatment_df, 'mapping_treatment_df.csv', row.names = FALSE)
  



######################### analyse distinct df ######################### 
results_unique <- results %>% 
  # filter(are_equal == FALSE, 
  #        same_min_max == FALSE
  #        ) %>% 
  select(-chunk,-dataframe,-name_col_year) %>% 
  distinct()









