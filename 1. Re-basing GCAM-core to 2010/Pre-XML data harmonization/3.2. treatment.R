load('C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis/csvs_to_xml_2021.RData')
csvs_to_xml_2021 <- csvs_to_xml
mapping_treatment_df <- read.csv('C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis/mapping_treatment_df.csv')

library(dplyr)
library(purrr)
library(stringr)

summary_treatments <- mapping_treatment_df %>%
  group_by(chunk, dataframe) %>%
  summarise(
    treatments = paste(unique(treatment), collapse = ", "),
    n_columns = n(),
    .groups = "drop"
  ) %>%
  arrange(chunk, dataframe)

##################### Adjust #####################
choose_year_col <- function(df_, cols) {
  if (length(cols) == 1) {
    return(cols[1])
  } else {
    # revisamos cuál varía realmente
    varying <- sapply(cols, function(col) length(unique(df_[[col]])) > 1)
    if (any(varying)) {
      return(cols[varying][1])  # si varias varían, tomamos la primera
    } else {
      return(cols[1])  # si ninguna varía, tomamos la primera
    }
  }
}

replace_missing_values <- function(df_, missing_in_2010, missing_in_2021, name_col_treatment) {
  library(dplyr)
  missing_in_2010_i <- lapply(missing_in_2010, function(x) as.numeric(strsplit(as.character(x), ",\\s*")[[1]]))[[1]]
  missing_in_2021_i <- lapply(missing_in_2021, function(x) as.numeric(strsplit(as.character(x), ",\\s*")[[1]]))[[1]]
  
  if (length(missing_in_2010_i) == length(missing_in_2021_i)){
    for (i in seq_along(missing_in_2010)) {
      for (j in seq_along(name_col_treatment)) {
        df_ <- df_ %>%
          mutate(
            !!name_col_treatment[j] := {
              # Convertir columna a numérico
              col_numeric <- as.numeric(.data[[name_col_treatment[j]]])
              # Reemplazo con if_else
              if_else(
                col_numeric %in% as.numeric(missing_in_2010[i]),
                as.numeric(missing_in_2021[i]),
                col_numeric
              )
            }
          )
      }
    }
  } else if (length(missing_in_2010_i) < length(missing_in_2021_i)) {
    
    # Caso extendido: duplicar filas por cada año nuevo
    rows_to_replace <- df_ %>%
      filter(.data[[name_col_treatment]] %in% missing_in_2010_i)
    
    # duplicar filas: crossing cada fila con los nuevos años
    new_rows <- tidyr::crossing(rows_to_replace, new_year = missing_in_2021_i) %>%
      mutate(!!name_col_treatment := new_year) %>%
      select(-new_year)
    
    # eliminar filas originales y añadir las nuevas
    df_ <- df_ %>%
      filter(!(.data[[year_col]] %in% missing_in_2010_i)) %>%
      bind_rows(new_rows) %>%
      arrange(.data[[year_col]])
  }
  
  return(df_)
}


trim_years_in_df <- function(df_, missing_in_2010, name_col_treatment) {
  library(dplyr)
  
  for (i in seq_along(name_col_treatment)) {
    # Convertir la entrada en vector, en caso de que venga como string con comas
    years_to_remove <- strsplit(missing_in_2010[i], ",\\s*")[[1]]
    
    df_ <- df_ %>%
      filter(!(.data[[name_col_treatment[i]]] %in% years_to_remove))
  }
  
  return(df_)
}


fill_missing_years <- function(df_, missing_in_2021, name_col_treatment) {
  library(dplyr)
  library(tidyr)
  
  # Convertir missing_in_2021 a vector numérico
  new_years <- as.numeric(unlist(strsplit(missing_in_2021, ",\\s*")))
  
  # Columnas numéricas a interpolar
  num_cols <- names(df_)[sapply(df_, is.numeric) & !names(df_) %in% name_col_treatment]
  
  # Columnas “clave” para agrupar
  key_cols <- setdiff(names(df_), c(name_col_treatment, num_cols))
  
  if (length(missing_in_2021) == 1 || length(name_col_treatment) == 1) {
    year_col <- choose_year_col(df_, name_col_treatment)
    other_col <- setdiff(name_col_treatment, year_col)
    if (length(other_col) > 0) key_cols <- c(key_cols, other_col)
    
    df_extended <- df_ %>%
      group_by(across(all_of(key_cols))) %>%
      tidyr::complete(
        !!sym(year_col) := sort(unique(c(.data[[year_col]], new_years)))
      ) %>%
      ungroup()
    
    df_extended <- df_extended %>%
      group_by(across(all_of(key_cols))) %>%
      arrange(.data[[year_col]], .by_group = TRUE) %>%
      mutate(across(all_of(num_cols), ~ {
        n_val <- sum(!is.na(.))
        if(n_val == 0) return(.)
        if(n_val == 1) return(rep(.[!is.na(.)], length(.)))  # <--- si solo hay un valor
        approx(
          x = .data[[year_col]][!is.na(.)],
          y = .[!is.na(.)],
          xout = .data[[year_col]],
          rule = 2
        )$y
      })) %>%
      ungroup()
    
    df_ <- df_extended
    
  } else {
    df_1 <- df_ %>%
      mutate(year_i = .data[[name_col_treatment[1]]])
    
    df_extended <- df_1 %>%
      group_by(across(all_of(key_cols))) %>%
      tidyr::complete(year_i = sort(unique(c(year_i, new_years)))) %>%
      ungroup()
    
    df_extended <- df_extended %>%
      group_by(across(all_of(key_cols))) %>%
      arrange(year_i, .by_group = TRUE) %>%
      mutate(across(all_of(num_cols), ~ {
        n_val <- sum(!is.na(.))
        if(n_val == 0) return(.)
        if(n_val == 1) return(rep(.[!is.na(.)], length(.)))
        approx(
          x = year_i[!is.na(.)],
          y = .[!is.na(.)],
          xout = year_i,
          rule = 2
        )$y
      })) %>%
      ungroup() %>%
      mutate(!!name_col_treatment[1] := year_i,
             !!name_col_treatment[2] := year_i)
    
    df_final <- df_ %>%
      full_join(df_extended, 
                by = c(key_cols, all_of(name_col_treatment)),
                suffix = c("", "_new"))
    
    for (col in num_cols) {
      new_col <- paste0(col, "_new")
      if (new_col %in% names(df_final)) {
        df_final[[col]] <- coalesce(df_final[[new_col]], df_final[[col]])
      }
    }
    
    df_ <- df_final %>%
      select(-ends_with("_new"), -year_i)
  }
  
  return(df_)
}


new_chunks_2010 <- list()
for (chunk_i in names(csvs_to_xml_2021)){
  chunk <- csvs_to_xml_2021[[chunk_i]]
  new_chunks_2010[[chunk_i]] <- list()
  for (df_i in names(chunk)){
    df_ <- csvs_to_xml_2021[[chunk_i]][[df_i]]
    if (df_i %in% mapping_treatment_df$dataframe){
    treatment_df <- mapping_treatment_df[mapping_treatment_df$dataframe == df_i & 
                                           mapping_treatment_df$chunk == chunk_i,]
    v <- unique(trimws(treatment_df$treatment))
    if ("copy_df" %in% v & length(v) > 1) {
      other <- setdiff(v, "copy_df")
      type_of_treatment <- if (length(other) > 0) other[1] else "copy_df"
      treatment_df <- treatment_df[treatment_df$treatment != "copy_df",]
    } else if (length(v) > 1) {
      type_of_treatment <- v
    } else {
      type_of_treatment <- v[1]
    }
    
    
    if (is.character(type_of_treatment) && length(type_of_treatment) == 1) {
      name_col_treatment <- treatment_df$name_col_year
      missing_in_2010 <- treatment_df$missing_in_2010 
      missing_in_2021 <- treatment_df$missing_in_2021
      missing_in_2010 <- missing_in_2010[missing_in_2010 != ""]
      missing_in_2021 <- missing_in_2021[missing_in_2021 != ""]
      
      if (type_of_treatment == 'copy_df'){
        new_chunks_2010[[chunk_i]][[df_i]] <- df_
      }
      else if(type_of_treatment == 'swap_years'){
        #Intercambiar las filas con los años de missing_in_2021 en el data frame df_ en name_col_treatment por las filas con los años de missing_in_2010
        #si en las nuevas filas hay valores numéricos, habrá que intepolar
        df_ <- replace_missing_values(df_, missing_in_2010, missing_in_2021, name_col_treatment)
        new_chunks_2010[[chunk_i]][[df_i]] <- df_
        
      }
      else if(type_of_treatment == 'trim_years'){
        #Recortar. Eliminar las filas con los años que aparecen en missing_in_2010 en nuestro en el data frame df_
        df_ <- trim_years_in_df(df_, missing_in_2010, name_col_treatment) 
        new_chunks_2010[[chunk_i]][[df_i]] <- df_
        
      }
      else if(type_of_treatment == 'fill_missing_years'){
        # Añadir filas en nuestro dataframe df_ para que incluya los años que aparecen en missing_in_2021. Todo idéntico, varaibles no numéricos iguales y los numéricos elaborar interpolación 
        df_ <- fill_missing_years(df_, missing_in_2021, name_col_treatment)
        new_chunks_2010[[chunk_i]][[df_i]] <- df_
        
      }
    }else if (is.character(type_of_treatment) && length(type_of_treatment) == 2){
      if (identical(sort(type_of_treatment), c("fill_missing_years", "swap_years"))) {
        treatment_df_swap <- treatment_df[treatment_df$treatment == 'swap_years',]
        name_col_treatment <- treatment_df_swap$name_col_year
        missing_in_2010 <- treatment_df_swap$missing_in_2010 
        missing_in_2021 <- treatment_df_swap$missing_in_2021
        df_ <- replace_missing_values(df_, missing_in_2010, missing_in_2021, name_col_treatment)
        # treatment_df_fill <- treatment_df[treatment_df$treatment == 'fill_missing_years',]
        # name_col_treatment <- treatment_df_fill$name_col_year
        # missing_in_2010 <- treatment_df_fill$missing_in_2010 
        # missing_in_2021 <- treatment_df_fill$missing_in_2021
        # df_ <- fill_missing_years(df_, missing_in_2021, name_col_treatment)
        # new_chunks_2010[[chunk_i]][[df_i]] <- df_
      }
    }
    
    
    }else{
      #Meter el data frame tal cual en item chunk en item dataframe en la lista new_chunks_2010
      new_chunks_2010[[chunk_i]][[df_i]] <- df_
    }
    
  }
}

######################## Check whether in 2010 are NaN while not in 2021 ######################## 
has_bad_values <- function(df) {
  if (!is.data.frame(df)) return(FALSE)
  any(sapply(df, function(col) any(is.na(col) | is.infinite(col))))
}

problematic_chunks <- list()

for (chunk in names(new_chunks_2010)) {
  lista1 <- new_chunks_2010[[chunk]]
  lista2 <- csvs_to_xml_2021[[chunk]]
  
  bad_dfs <- lapply(names(lista1), function(nm) {
    df1 <- lista1[[nm]]
    df2 <- lista2[[nm]]
    
    if (has_bad_values(df1) && !has_bad_values(df2)) {
      return(nm)
    } else {
      return(NULL)
    }
  })
  
  bad_dfs <- unlist(bad_dfs)
  
  if (length(bad_dfs) > 0) {
    problematic_chunks[[chunk]] <- bad_dfs
  }
}


mapping_treatment_df_promChunks_new <- mapping_treatment_df %>% filter(chunk %in% names(problematic_chunks) & dataframe %in% unlist(problematic_chunks, use.names = FALSE))


# df <- new_chunks_2010$zenergy_xml_electricity.R$L223.GlobalTechProfitShutdown_elec
# 
# bad_rows <- df %>%
#   filter(if_any(everything(), ~ is.na(.) | is.infinite(.) | is.nan(.)))





############## Save results ###############

save(new_chunks_2010, file = "new_chunks_2010.RData")


