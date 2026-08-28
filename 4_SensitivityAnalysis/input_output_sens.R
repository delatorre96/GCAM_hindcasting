library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis/gcam_sensitivity.sqlite"
)

#See interested experiments:
experiments <- dbReadTable(con, "Experiments")
experiment_id <- "EXP_2e96286f"

runs <- dbReadTable(con, "Runs")

datasets <- dbReadTable(con, "Datasets")

query_name <- "outputs_by_tech"



source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

inputs <- read_experiment_inputs(
  con = con,
  experiment_id
)  %>% 
  select(-xml_file, -xpath, -fillout, -year)


outputs_by_tech <- read_experiment_output(
  con = con,
  query_name = query_name,
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



library(dplyr)
library(tidyr)
library(ranger)


#### logits #####
## How many distinct input logits are there

inputs_logits <- inputs 
# %>%
#   filter(
#     is.na(price_elasticity),
#     is.na(gcam_consumer),
#     is.na(satiation_level)
#   ) %>%
#   select(
#     -price_elasticity,
#     -gcam_consumer,
#     -satiation_level
#   )


# ============================================================
# 1. Inputs únicos
# ============================================================

input_keys <- inputs_logits %>%
  select(
    region,
    supplysector,
    subsector,
    nesting_subsector
  ) %>%
  distinct() %>%
  mutate(input_id = row_number())


inputs_logits <- inputs_logits %>%
  left_join(
    input_keys,
    by = c(
      "region", 
      "supplysector", 
      "subsector", 
      "nesting_subsector"
    )
  )


# ============================================================
# 2. Preparar outputs
# ============================================================

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


# ============================================================
# 3. Threshold de correlación
# ============================================================

correlation_threshold <- 0.1


# ============================================================
# 4. Parámetros Random Forest
# ============================================================

set.seed(123)

# Proporción train/test
train_fraction <- 0.80

# Número de folds dentro del training set
n_folds <- 5

# Número de árboles
n_trees <- 1000


# ============================================================
# 5. Lista donde guardaremos los resultados
# ============================================================

# IMPORTANTE:
# El loop recorre output_id, por lo que la longitud debe
# corresponder al número de outputs, no al número de inputs.

input_output_list <- vector(
  "list", 
  length(all_errors_keys$output_id)
)


# Lista adicional para guardar métricas de cada Random Forest
rf_model_results_list <- vector(
  "list",
  length(all_errors_keys$output_id)
)


# ============================================================
# 6. Loop
# ============================================================

for (i in seq_along(all_errors_keys$output_id)) {
  
  line <- all_errors_keys[i, ]
  
  output_id_i <- line$output_id
  
  
  # ----------------------------------------------------------
  # 6.1. Seleccionar ESTE output
  # ----------------------------------------------------------
  
  outputs_sub <- all_errors %>%
    filter(output_id == output_id_i)
  
  
  # ----------------------------------------------------------
  # 6.2. Join con TODOS los inputs de la misma
  #     región + run
  # ----------------------------------------------------------
  
  input_output_merge <- outputs_sub %>%
    left_join(
      inputs_logits,
      by = c("region", "run_id")
    )
  
  
  # ----------------------------------------------------------
  # 6.3. Calcular correlación input -> output
  # ----------------------------------------------------------
  
  correlation_results <- input_output_merge %>%
    group_by(input_id) %>%
    summarise(
      
      n = sum(
        complete.cases(logit, error_abs)
      ),
      
      correlation = if (n >= 3) {
        cor(
          logit,
          error_abs,
          use = "complete.obs",
          method = "spearman"
        )
      } else {
        NA_real_
      },
      
      .groups = "drop"
    ) %>%
    mutate(
      output_id = output_id_i
    )
  
  
  # ----------------------------------------------------------
  # 6.4. Seleccionar inputs con correlación suficiente
  # ----------------------------------------------------------
  
  selected_inputs <- correlation_results %>%
    filter(
      !is.na(correlation),
      abs(correlation) >= correlation_threshold
    ) %>%
    arrange(desc(abs(correlation)))
  
  
  # ----------------------------------------------------------
  # 6.5. Si no hay inputs seleccionados, guardar y continuar
  # ----------------------------------------------------------
  
  if (nrow(selected_inputs) == 0) {
    
    input_output_list[[i]] <- correlation_results %>%
      filter(
        !is.na(correlation),
        abs(correlation) >= correlation_threshold
      )
    
    rf_model_results_list[[i]] <- tibble(
      output_id = output_id_i,
      n_inputs_rf = 0,
      n_runs_rf = 0,
      cv_rmse = NA_real_,
      test_rmse = NA_real_,
      test_r2 = NA_real_,
      best_mtry = NA_integer_,
      best_min_node_size = NA_integer_
    )
    
    next
  }
  
  
  # ----------------------------------------------------------
  # 6.6. IDs de los inputs seleccionados
  # ----------------------------------------------------------
  
  selected_input_ids <- selected_inputs$input_id
  
  

  # ----------------------------------------------------------
  # 6.7. Preparar dataset para Random Forest
  #
  # Cada fila = un run
  # Cada columna = un input
  # Target = error_abs
  # ----------------------------------------------------------
  
  rf_long <- input_output_merge %>%
    filter(
      input_id %in% selected_input_ids
    ) %>%
    select(
      run_id,
      input_id,
      logit,
      error_abs
    )
  
  
  # ----------------------------------------------------------
  # Comprobar si existe más de un valor por
  # run_id + input_id
  # ----------------------------------------------------------
  
  duplicated_input_runs <- rf_long %>%
    group_by(
      run_id,
      input_id
    ) %>%
    summarise(
      n_values = n(),
      n_logit_unique = n_distinct(logit),
      .groups = "drop"
    ) %>%
    filter(
      n_values > 1
    )
  
  
  # ----------------------------------------------------------
  # Si hay duplicados, comprobar si realmente tienen
  # diferentes valores de logit
  # ----------------------------------------------------------
  
  if (nrow(duplicated_input_runs) > 0) {
    
    duplicated_with_different_logits <- duplicated_input_runs %>%
      filter(
        n_logit_unique > 1
      )
    
    if (nrow(duplicated_with_different_logits) > 0) {
      
      warning(
        paste0(
          "Output ", output_id_i,
          ": existen múltiples valores de logit ",
          "para algunos pares run_id + input_id."
        )
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # Reducir a un único valor por run_id + input_id
  #
  # Si existen duplicados con el mismo logit, first() no
  # cambia el resultado.
  #
  # Si existen duplicados con valores diferentes, usamos
  # mean() como agregación.
  # ----------------------------------------------------------
  
  rf_long <- rf_long %>%
    group_by(
      run_id,
      input_id
    ) %>%
    summarise(
      logit = mean(logit, na.rm = TRUE),
      error_abs = first(error_abs),
      .groups = "drop"
    )
  
  
  # ----------------------------------------------------------
  # 6.8. Pasar a formato ancho
  # ----------------------------------------------------------
  
  rf_data <- rf_long %>%
    pivot_wider(
      names_from = input_id,
      values_from = logit,
      names_prefix = "input_"
    )
  
  
  # ----------------------------------------------------------
  # Comprobar que NO quedan columnas tipo list
  # ----------------------------------------------------------
  
  list_columns <- names(
    rf_data
  )[sapply(
    rf_data,
    is.list
  )]
  
  
  if (length(list_columns) > 0) {
    
    stop(
      paste0(
        "El dataframe RF todavía contiene columnas tipo list: ",
        paste(list_columns, collapse = ", ")
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # 6.9. Eliminar runs con NA
  # ----------------------------------------------------------
  
  rf_data <- rf_data %>%
    filter(
      complete.cases(.)
    )
  
  
  # ----------------------------------------------------------
  # 6.9. Comprobar que hay suficientes runs
  # ----------------------------------------------------------
  
  n_runs <- nrow(rf_data)
  n_inputs_rf <- length(selected_input_ids)
  
  
  if (n_runs < 10 || n_inputs_rf < 1) {
    
    input_output_list[[i]] <- selected_inputs
    
    rf_model_results_list[[i]] <- tibble(
      output_id = output_id_i,
      n_inputs_rf = n_inputs_rf,
      n_runs_rf = n_runs,
      cv_rmse = NA_real_,
      test_rmse = NA_real_,
      test_r2 = NA_real_,
      best_mtry = NA_integer_,
      best_min_node_size = NA_integer_
    )
    
    next
  }
  
  
  # ==========================================================
  # 6.10. Train / Test split POR RUN
  # ==========================================================
  
  set.seed(123 + output_id_i)
  
  train_ids <- sample(
    rf_data$run_id,
    size = floor(train_fraction * n_runs)
  )
  
  train_data <- rf_data %>%
    filter(
      run_id %in% train_ids
    )
  
  test_data <- rf_data %>%
    filter(
      !run_id %in% train_ids
    )
  
  
  # ----------------------------------------------------------
  # 6.11. Preparar folds de Cross Validation
  #       solamente dentro del TRAIN
  # ----------------------------------------------------------
  
  n_folds_actual <- min(
    n_folds,
    nrow(train_data)
  )
  
  set.seed(456 + output_id_i)
  
  fold_id <- sample(
    rep(
      1:n_folds_actual,
      length.out = nrow(train_data)
    )
  )
  
  
  # ==========================================================
  # 6.12. Grid Search
  # ==========================================================
  
  # Valores de mtry razonables dependiendo del número de inputs
  
  mtry_values <- unique(
    pmax(
      1,
      pmin(
        n_inputs_rf,
        c(
          floor(sqrt(n_inputs_rf)),
          floor(n_inputs_rf / 3),
          floor(n_inputs_rf / 2),
          n_inputs_rf
        )
      )
    )
  )
  
  
  # Valores de min.node.size
  
  min_node_values <- c(
    3,
    5,
    10,
    20
  )
  
  
  rf_grid <- expand.grid(
    mtry = mtry_values,
    min.node.size = min_node_values
  )
  
  
  # ----------------------------------------------------------
  # Función RMSE
  # ----------------------------------------------------------
  
  calculate_rmse <- function(observed, predicted) {
    sqrt(
      mean(
        (observed - predicted)^2
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Grid search + 5-fold CV
  # ----------------------------------------------------------
  
  cv_results <- vector(
    "list",
    nrow(rf_grid)
  )
  
  
  for (g in seq_len(nrow(rf_grid))) {
    
    current_mtry <- rf_grid$mtry[g]
    current_min_node <- rf_grid$min.node.size[g]
    
    fold_rmse <- numeric(n_folds_actual)
    
    
    for (fold in seq_len(n_folds_actual)) {
      
      cv_train <- train_data[
        fold_id != fold,
        ,
        drop = FALSE
      ]
      
      cv_valid <- train_data[
        fold_id == fold,
        ,
        drop = FALSE
      ]
      
      
      # Quitamos run_id porque no es predictor
      cv_train_model <- cv_train %>%
        select(-run_id)
      
      cv_valid_model <- cv_valid %>%
        select(-run_id)
      
      
      # Random Forest
      rf_cv <- ranger(
        error_abs ~ .,
        data = cv_train_model,
        num.trees = n_trees,
        mtry = current_mtry,
        min.node.size = current_min_node,
        importance = "none",
        seed = 1000 + output_id_i + fold
      )
      
      
      # Predicción
      predictions <- predict(
        rf_cv,
        data = cv_valid_model
      )$predictions
      
      
      # RMSE
      fold_rmse[fold] <- calculate_rmse(
        cv_valid_model$error_abs,
        predictions
      )
    }
    
    
    cv_results[[g]] <- tibble(
      mtry = current_mtry,
      min.node.size = current_min_node,
      cv_rmse = mean(
        fold_rmse,
        na.rm = TRUE
      )
    )
  }
  
  
  cv_results <- bind_rows(
    cv_results
  ) %>%
    arrange(cv_rmse)
  
  
  # ----------------------------------------------------------
  # Mejor combinación de hiperparámetros
  # ----------------------------------------------------------
  
  best_model_parameters <- cv_results %>%
    slice(1)
  
  
  best_mtry <- best_model_parameters$mtry
  best_min_node_size <- best_model_parameters$min.node.size
  best_cv_rmse <- best_model_parameters$cv_rmse
  
  
  # ==========================================================
  # 6.13. Entrenar modelo final sobre TODO el TRAIN
  # ==========================================================
  
  final_train_data <- train_data %>%
    select(-run_id)
  
  
  final_test_data <- test_data %>%
    select(-run_id)
  
  
  set.seed(789 + output_id_i)
  
  rf_final <- ranger(
    error_abs ~ .,
    data = final_train_data,
    num.trees = n_trees,
    mtry = best_mtry,
    min.node.size = best_min_node_size,
    
    # IMPORTANTE:
    # permutation importance
    importance = "permutation",
    
    seed = 2000 + output_id_i
  )
  
  
  # ==========================================================
  # 6.14. Evaluación sobre TEST
  # ==========================================================
  
  test_predictions <- predict(
    rf_final,
    data = final_test_data
  )$predictions
  
  
  test_rmse <- calculate_rmse(
    final_test_data$error_abs,
    test_predictions
  )
  
  
  # R² sobre test
  
  test_r2 <- 1 -
    sum(
      (final_test_data$error_abs - test_predictions)^2
    ) /
    sum(
      (final_test_data$error_abs -
         mean(final_test_data$error_abs))^2
    )
  
  
  # ==========================================================
  # 6.15. Extraer importancia de variables
  # ==========================================================
  
  importance_results <- tibble(
    variable = names(
      rf_final$variable.importance
    ),
    
    importance = as.numeric(
      rf_final$variable.importance
    )
  ) %>%
    mutate(
      input_id = as.integer(
        sub(
          "input_",
          "",
          variable
        )
      )
    ) %>%
    mutate(
      importance_relative = importance /
        sum(
          abs(importance),
          na.rm = TRUE
        )
    )
  
  
  # ==========================================================
  # 6.16. Añadir importancia al dataframe de correlaciones
  # ==========================================================
  
  correlation_results_final <- correlation_results %>%
    filter(
      !is.na(correlation),
      abs(correlation) >= correlation_threshold
    ) %>%
    left_join(
      importance_results %>%
        select(
          input_id,
          importance,
          importance_relative
        ),
      by = "input_id"
    ) %>%
    mutate(
      cv_rmse = best_cv_rmse,
      test_rmse = test_rmse,
      test_r2 = test_r2,
      best_mtry = best_mtry,
      best_min_node_size = best_min_node_size,
      n_runs_rf = n_runs,
      n_inputs_rf = n_inputs_rf
    ) %>%
    arrange(
      desc(abs(correlation))
    )
  
  
  # ----------------------------------------------------------
  # 6.17. Guardar resultados del output
  # ----------------------------------------------------------
  
  input_output_list[[i]] <- correlation_results_final
  
  
  # ----------------------------------------------------------
  # 6.18. Guardar métricas del modelo
  # ----------------------------------------------------------
  
  rf_model_results_list[[i]] <- tibble(
    output_id = output_id_i,
    n_inputs_rf = n_inputs_rf,
    n_runs_rf = n_runs,
    cv_rmse = best_cv_rmse,
    test_rmse = test_rmse,
    test_r2 = test_r2,
    best_mtry = best_mtry,
    best_min_node_size = best_min_node_size
  )
  
  
  # ----------------------------------------------------------
  # Mensaje de progreso
  # ----------------------------------------------------------
  
  cat(
    "Output:",
    output_id_i,
    "| Inputs:",
    n_inputs_rf,
    "| Runs:",
    n_runs,
    "| CV RMSE:",
    round(best_cv_rmse, 4),
    "| Test RMSE:",
    round(test_rmse, 4),
    "| Test R2:",
    round(test_r2, 4),
    "\n"
  )
}


# ============================================================
# 7. Construir dataframes finales
# ============================================================

input_output_drivers <- bind_rows(
  input_output_list
)


rf_model_results <- bind_rows(
  rf_model_results_list
)


# ============================================================
# 8. Guardar resultados
# ============================================================

write.csv(
  input_output_drivers,
  "input_output_drivers.csv",
  row.names = FALSE
)


write.csv(
  rf_model_results,
  "rf_model_results.csv",
  row.names = FALSE
)














































































#### logits #####
##How many distinct input logits are ther
inputs_logits <- inputs 
# %>%
#   filter(
#     is.na(price_elasticity),
#     is.na(gcam_consumer),
#     is.na(satiation_level)
#   ) %>%
#   select(
#     -price_elasticity,
#     -gcam_consumer,
#     -satiation_level
#   )

# 1. Inputs únicos

input_keys <- inputs_logits %>%
  select(
    region,
    supplysector,
    subsector,
    nesting_subsector
  ) %>%
  distinct() %>%
  mutate(input_id = row_number())

inputs_logits <- inputs_logits %>%
  left_join(input_keys, by = c('region', 
                                     'supplysector', 
                                     'subsector', 
                                     'nesting_subsector'))

# 2. Preparar outputs

all_errors <- all_errors_output_by_tech %>%
  rename(
    output_sector = sector,
    output_subsector = subsector,
    output_output = output,
    output_technology = technology
  )

all_errors_keys <- all_errors %>%
  select(region, 
         output_sector, 
         output_subsector, 
         output_output, 
         output_technology) %>%
  distinct() %>%
  mutate(output_id = row_number())

all_errors <- all_errors %>%
  left_join(all_errors_keys, by = c('region', 
                                     'output_sector', 
                                     'output_subsector', 
                                     'output_output', 
                                     'output_technology'))


# 3. Threshold de correlación

correlation_threshold <- 0.1
# 4. Listas donde guardaremos los resultados

input_output_list <- vector("list", nrow(input_keys))



# ============================================================
# 5. Loop
# ============================================================

for (i in all_errors_keys$output_id) {
  
  line <- all_errors_keys[i, ]
  
  # region_i <- line$region
  # output_sector_i <- line$output_sector
  # output_subsector_i <- line$output_subsector
  # output_output_i <- line$output_output
  # output_technology_i <- line$output_technology
  output_id_i <- line$output_id
  
  # ----------------------------------------------------------
  # 5.1. Seleccionar ESTE output
  # ----------------------------------------------------------
  
  outputs_sub <- all_errors %>% filter(output_id == output_id_i)
  
  # ----------------------------------------------------------
  # 5.2. Join con TODOS los outputs de la misma
  #     región + run
  # ----------------------------------------------------------
  
  input_output_merge <- outputs_sub %>%
    left_join(
      inputs_logits,
      by = c("region", "run_id")
    )
  
  
  # ----------------------------------------------------------
  # 5.3. Calcular correlación para cada output
  # ----------------------------------------------------------
  
  correlation_results <- input_output_merge %>%
    group_by(
      # supplysector,
      # subsector,
      # nesting_subsector,
      input_id,
    ) %>%
    summarise(
      
      n = sum(
        complete.cases(logit, error_abs)
      ),
      
      correlation = if (n >= 3) {
        cor(
          logit,
          error_abs,
          use = "complete.obs",
          method = "spearman"
        )
      } else {
        NA_real_
      },
      
      .groups = "drop"
    ) %>%
    mutate(
      output_id = line$output_id,
      # region = region_i,
      # output_sector = output_sector_i,
      # output_subsector = output_subsector_i,
      # output_output = output_output_i,
      # output_technology = output_technology_i
    )
  
  # ----------------------------------------------------------
  # 5.4. Guardar outputs importantes
  # ----------------------------------------------------------
  
  input_output_list[[i]] <- correlation_results %>%
    filter(
       !is.na(correlation),
       abs(correlation) >= correlation_threshold
    ) %>%
    arrange(desc(abs(correlation)))
  
}


# ============================================================
# 6. Construir dataframes finales
# ============================================================

input_output_drivers <- bind_rows(input_output_list)

write.csv(input_output_drivers, 'input_output_drivers.csv', row.names = FALSE)






















# 
# 
# 
# 
# # ============================================================
# # 5. Loop
# # ============================================================
# 
# for (i in seq_len(nrow(input_keys))) {
#   
#   line <- input_keys[i, ]
#   
#   region_i <- line$region
#   supplysector_i <- line$supplysector
#   subsector_i <- line$subsector
#   nesting_subsector_i <- line$nesting_subsector
#   
#   # ----------------------------------------------------------
#   # 5.1. Seleccionar ESTE input
#   # ----------------------------------------------------------
#   
#   inputs_sub <- inputs_logits %>%
#     filter(
#       region == region_i,
#       
#       if (is.na(supplysector_i)) {
#         is.na(supplysector)
#       } else {
#         supplysector == supplysector_i
#       },
#       
#       if (is.na(subsector_i)) {
#         is.na(subsector)
#       } else {
#         subsector == subsector_i
#       },
#       
#       if (is.na(nesting_subsector_i)) {
#         is.na(nesting_subsector)
#       } else {
#         nesting_subsector == nesting_subsector_i
#       }
#     )
#   
#   
#   # ----------------------------------------------------------
#   # 5.2. Join con TODOS los outputs de la misma
#   #     región + run
#   # ----------------------------------------------------------
#   
#   input_output_merge <- inputs_sub %>%
#     left_join(
#       all_errors,
#       by = c("region", "run_id")
#     )
#   
#   
#   # ----------------------------------------------------------
#   # 5.3. Calcular correlación para cada output
#   # ----------------------------------------------------------
#   
#   correlation_results <- input_output_merge %>%
#     group_by(
#       output_sector,
#       output_subsector,
#       output_output,
#       output_technology
#     ) %>%
#     summarise(
#       
#       n = sum(
#         complete.cases(logit, error_abs)
#       ),
#       
#       correlation = if (n >= 3) {
#         cor(
#           logit,
#           error_abs,
#           use = "complete.obs",
#           method = "spearman"
#         )
#       } else {
#         NA_real_
#       },
#       
#       .groups = "drop"
#     ) %>%
#     mutate(
#       input_id = line$input_id,
#       region = region_i,
#       supplysector = supplysector_i,
#       subsector = subsector_i,
#       nesting_subsector = nesting_subsector_i
#     )
#   
#   
#   # ----------------------------------------------------------
#   # 5.4. Guardar outputs importantes
#   # ----------------------------------------------------------
#   
#   input_output_list[[i]] <- correlation_results %>%
#     filter(
#       !is.na(correlation),
#       abs(correlation) >= correlation_threshold
#     ) %>%
#     arrange(desc(abs(correlation)))
#   
# }
# 
# 
# # ============================================================
# # 6. Construir dataframes finales
# # # ============================================================
# 
# input_output_drivers <- bind_rows(input_output_list)

# 
# 
# 
# 
# lithuania_reg_oil <- inputs %>% 
#   filter(region == 'Lithuania', 
#          supplysector == 'regional oil', 
#          is.na(subsector), 
#          is.na(nesting_subsector)) %>%
#   left_join(all_errors_output_by_tech %>% 
#               filter(region == 'Lithuania', 
#                      sector == 'regional oil', 
#                      subsector == 'imported crude oil', 
#                      output == 'regional oil', 
#                      technology == 'imported crude oil'), by = 'run_id')
# lithuania_reg_oil <- lithuania_reg_oil %>%
#   filter(error_abs < 9.8)
# 
# plot(lithuania_reg_oil$logit, lithuania_reg_oil$error_abs)
