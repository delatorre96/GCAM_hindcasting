
initial_correlation_screeming <- function(experiment_id, con, param = 'logit'){
  #param could be "logit","satiation_level" or "price_elasticity"
  
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")
  
  query_name <- "outputs_by_tech"
  
  
  
  
  inputs <- read_experiment_inputs(
    con = con,
    execution_errors = TRUE,
    experiment_id
  )  %>% 
    select( -year)
  
  
  outputs_by_tech <- read_experiment_output(
    con = con,
    query_name = query_name,
    execution_errors = TRUE,
    experiment_id
  )
  
  
  ####### Reference #######
  ref_values <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
    select(where(~ !all(is.na(.)))) %>% 
    select(-query, -year, -rel_error, -value_chY, -error, -abs_error)
  
  all_errors_output_by_tech <- outputs_by_tech %>% 
    left_join(ref_values, by = c('region', 'technology', 'subsector', 'output', 'sector'))  %>% 
    filter(!is.na(value_ref) | !is.na(`2021`) ) %>%
    mutate(error_abs = abs(value_ref -`2021`))%>%
    select(-`2021`, -value_ref)
  
  # 1. Inputs únicos
  
  
  input_keys <- inputs %>%
    select(
      xpath,
      xml_file
    ) %>%
    distinct() %>%
    mutate(input_id = row_number())
  
  
  inputs <- inputs %>%
    left_join(
      input_keys,
      by = c(
        "xml_file",
        'xpath'
      )
    )
  
  
  
  # 2. Preparar outputs
  
  
  
  
  all_errors <- all_errors_output_by_tech %>%
    rename(
      output_sector = sector,
      output_subsector = subsector,
      output_output = output,
      output_technology = technology
    )
  
  
  all_errors_keys <- all_errors %>%
    select(
      region, 
      output_sector, 
      output_subsector, 
      output_output, 
      output_technology
    ) %>%
    distinct() %>%
    mutate(output_id = row_number())
  
  
  all_errors <- all_errors %>%
    left_join(
      all_errors_keys,
      by = c(
        "region", 
        "output_sector", 
        "output_subsector", 
        "output_output", 
        "output_technology"
      )
    )
  
  # CORRELACIONES INPUT -> OUTPUT
  
  inputs_cor <- inputs %>% 
    select(
      run_id,
      input_id,
      input_value = all_of(param)
    )
  
  
  
  correlation_list <- vector(
    "list",
    length(
      all_errors_keys$output_id
    )
  )
  
  
  
  for (i in seq_along(all_errors_keys$output_id)) {
    
    output_id_i <-
      all_errors_keys$output_id[i]
    
    
    # ==========================================================
    # 3.1. Seleccionar output
    # ==========================================================
    
    output_data <- all_errors %>%
      filter(
        output_id == output_id_i
      ) %>%
      select(
        run_id,
        error_abs
      )
    
    
    
    # ==========================================================
    # 3.2. Join output con inputs
    # ==========================================================
    
    correlation_data <-
      output_data %>%
      inner_join(
        inputs_cor,
        by = "run_id"
      )
    
    
    # ==========================================================
    # 3.3. Calcular Spearman para cada input
    # ==========================================================
    
    correlation_results <-
      correlation_data %>%
      
      group_by(
        input_id
      ) %>%
      
      summarise(
        
        # ------------------------------------------------------
        # Número de observaciones completas
        # ------------------------------------------------------
        
        n =
          sum(
            complete.cases(
              input_value,
              error_abs
            )
          ),
        
        
        # ------------------------------------------------------
        # Correlación Spearman
        # ------------------------------------------------------
        
        correlation = {
          
          complete <-
            complete.cases(
              input_value,
              error_abs
            )
          
          
          x <-
            input_value[
              complete
            ]
          
          
          y <-
            error_abs[
              complete
            ]
          
          
          if (
            length(x) >= 3 &&
            sd(x) > 0 &&
            sd(y) > 0
          ) {
            
            cor(
              x,
              y,
              method = "spearman"
            )
            
          } else {
            
            NA_real_
            
          }
          
        },
        
        .groups = "drop"
        
      ) %>%
      
      mutate(
        output_id =
          output_id_i
      ) %>%
      
      select(
        output_id,
        input_id,
        n,
        correlation
      )
    
    
    # ==========================================================
    # 3.4. Guardar resultado
    # ==========================================================
    
    correlation_list[[i]] <-
      correlation_results
    
    
    # ==========================================================
    # 3.5. Progreso
    # ==========================================================
    
    if (
      i %% 100 == 0 ||
      i ==
      length(
        all_errors_keys$output_id
      )
    ) {
      
      cat(
        
        "Output:",
        i,
        "/",
        length(
          all_errors_keys$output_id
        ),
        "| Output ID:",
        output_id_i,
        "\n"
        
      )
      
    }
    
  }
  
  
  # ============================================================
  # 4. Construir dataframe final
  # ============================================================
  
  correlation_all <-
    bind_rows(
      correlation_list
    ) %>% mutate (corr_abs = abs(correlation))
  
  
  # ============================================================
  # 5. Ordenar
  # ============================================================
  
  correlation_all <-
    correlation_all %>%
    
    arrange(
      output_id,
      desc(
        corr_abs
      )
    )
  
  write.csv (correlation_all,  paste0("correlation_all_",param,".csv"), row.names = FALSE)
  return (correlation_all)
}

summary_corr_df <- function(correlation_all, corr_threshold = 0.1){
    
  input_correlation_summary <- correlation_all %>%
    group_by(input_id) %>%
    summarise(
      max_abs_correlation =
        max(
          abs(correlation),
          na.rm = TRUE
        ),
      
      n_outputs_correlated =
        sum(
          abs(correlation) >= corr_threshold,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
  return(input_correlation_summary)
}






