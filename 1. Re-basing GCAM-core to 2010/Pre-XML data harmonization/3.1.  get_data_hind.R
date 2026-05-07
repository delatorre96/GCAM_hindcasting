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


treatment_hind <- function(df_i, df_){
  if (!exists("mapping_treatment_df")) {
    mapping_treatment_df <<- read.csv('C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis/mapping_dataframes_years.csv')
  }
  
  if (df_i %in% mapping_treatment_df$dataframe){
    treatment_df <- mapping_treatment_df[mapping_treatment_df$dataframe == df_i,]
    v <- unique(trimws(treatment_df$treatment))
    if ("copy_df" %in% v) {
      other <- setdiff(v, "copy_df")
      type_of_treatment <- if (length(other) > 0) other[1] else "copy_df"
    } else if (length(v) >= 2) {
      type_of_treatment <- "swap_years"
    } else {
      type_of_treatment <- v[1]
    }
    name_col_treatment <- treatment_df$name_col_year
    missing_in_2010 <- treatment_df$missing_in_2010 
    missing_in_2021 <- treatment_df$missing_in_2021
    missing_in_2010 <- missing_in_2010[missing_in_2010 != ""]
    missing_in_2021 <- missing_in_2021[missing_in_2021 != ""]
    
    if (type_of_treatment == 'copy_df'){
      return (df_)
    }
    else if(type_of_treatment == 'swap_years'){
      # orig_attrs <- attributes(df_)
      #Intercambiar las filas con los años de missing_in_2021 en el data frame df_ en name_col_treatment por las filas con los años de missing_in_2010
      #si en las nuevas filas hay valores numéricos, habrá que intepolar
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
      # attributes(df_) <- orig_attrs
      return (df_)
      
    }
    else if(type_of_treatment == 'trim_years'){
      #Recortar. Eliminar las filas con los años que aparecen en missing_in_2010 en nuestro en el data frame df_
      # orig_attrs <- attributes(df_)
      for (i in seq_along(name_col_treatment)){
        years_to_remove <- strsplit(missing_in_2010[i], ",\\s*")[[1]]
        df_ <- df_ %>%
          filter(!(.data[[name_col_treatment[i]]] %in% years_to_remove))
      }
      # attributes(df_) <- orig_attrs
      return (df_)
      
    }
    else if(type_of_treatment == 'fill_missing_years'){
      # Añadir filas en nuestro dataframe df_ para que incluya los años que aparecen en missing_in_2021. Todo idéntico, varaibles no numéricos iguales y los numéricos elaborar interpolación 
      if (length(missing_in_2021) == 1){
        new_years <- as.numeric(strsplit(missing_in_2021, ",\\s*")[[1]])
        num_cols <- names(df_)[sapply(df_, is.numeric) & names(df_) != name_col_treatment]
        key_cols <- setdiff(names(df_), c(name_col_treatment, num_cols))
        year_col <- choose_year_col(df_, name_col_treatment)
        other_col <- setdiff(name_col_treatment, year_col)
        if (length(other_col) > 0) {
          key_cols <- c(key_cols, other_col)
        }
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
            # si hay menos de 2 puntos no se interpola
            if(sum(!is.na(.)) < 2) return(.)
            approx(
              x = .data[[year_col]][!is.na(.)],
              y = .[!is.na(.)],
              xout = .data[[year_col]],
              rule = 2
            )$y
          })) %>%
          ungroup()
        # orig_attrs <- attributes(df_)
        df_ <- df_extended
        # attributes(df_) <- orig_attrs
        
      }else{
        new_years <- as.numeric(strsplit(missing_in_2021, ",\\s*")[[1]])
        num_cols <- names(df_)[sapply(df_, is.numeric) & !names(df_) %in% name_col_treatment]
        key_cols <- setdiff(names(df_), c(name_col_treatment, num_cols))
        df_1 <- df_ %>%
          mutate(year_i = get(name_col_treatment[1])) 
        df_extended <- df_1 %>%
          group_by(across(all_of(key_cols))) %>%
          tidyr::complete(year_i = sort(unique(c(year_i, new_years)))) %>%
          ungroup()
        
        df_extended <- df_extended %>%
          group_by(across(all_of(key_cols))) %>%
          arrange(year_i, .by_group = TRUE) %>%
          mutate(across(all_of(num_cols), ~ {
            # interpolación segura
            if(sum(!is.na(.)) < 2) return(.)
            approx(
              x = year_i[!is.na(.)],
              y = .[!is.na(.)],
              xout = year_i,
              rule = 2
            )$y
          })) %>%
          ungroup() %>%
          mutate(!!name_col_treatment[1] := year_i) %>%
          mutate(!!name_col_treatment[2] := year_i)
        # 3. Join con el original
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
        # orig_attrs <- attributes(df_)
        df_ <- df_final %>%
          select(-ends_with("_new"),-year_i)
        # attributes(df_) <- orig_attrs
      }
      return (df_)
      
    }
  }else{
    #Meter el data frame tal cual en item chunk en item dataframe en la lista new_chunks_2010
    return (df_)
  }
}   


get_data_hindcasting <- function(all_data, name, strip_attributes = FALSE) {
  assertthat::assert_that(is_data_list(all_data))
  
  names(all_data) <- gsub(data.USER_MOD_POSTFIX, '', names(all_data))
  if(is.null(all_data[[name]])) {
    stop("Data system: couldn't find ", name)
  }
  
  # If a chunk's output is missing, it returns a tibble with all NA values
  # In this case we don't want to copy it to main data list, so that subsequent
  # chunks an easily check for its status via is.null()
  if(nrow(all_data[[name]]) > 0 && all(is.na(all_data[[name]]))) {
    return(NULL)
  }
  
  df_ <- all_data[[name]]
  # If strip_attributes == TRUE, remove all attributes.
  # As of dplyr 1.0.0, these can no longer be easily overwritten, so we remove them
  if(strip_attributes) {
    attr(df_, ATTR_TITLE) <- NULL
    attr(df_, ATTR_UNITS) <- NULL
    attr(df_, ATTR_COMMENTS) <- NULL
    attr(df_, ATTR_PRECURSORS) <- NULL
    attr(df_, ATTR_LEGACY_NAME) <- NULL
    attr(df_, ATTR_REFERENCE) <- NULL
  } else {
    df_ <- all_data[[name]]
  }
  df_hind <- treatment_hind(df_i = name, df_ = df_)
  return(df_hind)
}


