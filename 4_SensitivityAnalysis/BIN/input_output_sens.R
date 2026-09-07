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





#### ============================================================
#### LOGITS
#### Gradient Boosting / XGBoost
#### ============================================================

library(dplyr)
library(tidyr)
library(xgboost)


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
# 4. Parámetros Gradient Boosting
# ============================================================

set.seed(123)

# Proporción train/test
train_fraction <- 0.80

# Número de folds para CV
n_folds <- 5

# Número máximo de árboles
# xgb.cv utilizará early stopping para encontrar el número óptimo
max_nrounds <- 2000

# Número de iteraciones sin mejora antes de early stopping
early_stopping_rounds <- 50


# ============================================================
# 5. Lista donde guardaremos los resultados
# ============================================================

input_output_list <- vector(
  "list", 
  length(all_errors_keys$output_id)
)


# Lista adicional para guardar métricas de cada modelo
gb_model_results_list <- vector(
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
    filter(
      output_id == output_id_i
    )
  
  
  # ----------------------------------------------------------
  # 6.2. Join con TODOS los inputs de la misma
  #     región + run
  # ----------------------------------------------------------
  
  input_output_merge <- outputs_sub %>%
    left_join(
      inputs_logits,
      by = c(
        "region", 
        "run_id"
      )
    )
  
  
  # ----------------------------------------------------------
  # 6.3. Calcular correlación input -> output
  # ----------------------------------------------------------
  
  correlation_results <- input_output_merge %>%
    group_by(
      input_id
    ) %>%
    summarise(
      
      n = sum(
        complete.cases(
          logit,
          error_abs
        )
      ),
      
      correlation = {
        
        complete <- complete.cases(
          logit,
          error_abs
        )
        
        x <- logit[complete]
        y <- error_abs[complete]
        
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
    arrange(
      desc(abs(correlation))
    )
  
  
  # ----------------------------------------------------------
  # 6.5. Si no hay inputs seleccionados
  # ----------------------------------------------------------
  
  if (nrow(selected_inputs) == 0) {
    
    input_output_list[[i]] <- selected_inputs
    
    gb_model_results_list[[i]] <- tibble(
      output_id = output_id_i,
      n_inputs_gb = 0,
      n_runs_gb = 0,
      cv_rmse = NA_real_,
      test_rmse = NA_real_,
      test_r2 = NA_real_,
      best_eta = NA_real_,
      best_max_depth = NA_integer_,
      best_min_child_weight = NA_real_,
      best_subsample = NA_real_,
      best_colsample_bytree = NA_real_,
      best_gamma = NA_real_,
      best_lambda = NA_real_,
      best_alpha = NA_real_,
      best_nrounds = NA_integer_
    )
    
    next
  }
  
  
  # ----------------------------------------------------------
  # 6.6. IDs de los inputs seleccionados
  # ----------------------------------------------------------
  
  selected_input_ids <- selected_inputs$input_id
  
  
  
  # ==========================================================
  # 6.7. Preparar dataset para Gradient Boosting
  #
  # Cada fila = un run
  # Cada columna = un input
  # Target = error_abs
  # ==========================================================
  
  gb_long <- input_output_merge %>%
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
  # Comprobar duplicados
  # ----------------------------------------------------------
  
  duplicated_input_runs <- gb_long %>%
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
  # Warning si existen distintos logits para el mismo
  # run_id + input_id
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
          "para algunos pares run_id + input_id. ",
          "Se utilizará la media."
        )
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # Reducir a un único valor por run_id + input_id
  # ----------------------------------------------------------
  
  gb_long <- gb_long %>%
    group_by(
      run_id,
      input_id
    ) %>%
    summarise(
      logit = mean(
        logit,
        na.rm = TRUE
      ),
      error_abs = first(error_abs),
      .groups = "drop"
    )
  
  
  # ==========================================================
  # 6.8. Pasar a formato ancho
  # ==========================================================
  
  gb_data <- gb_long %>%
    pivot_wider(
      names_from = input_id,
      values_from = logit,
      names_prefix = "input_"
    )
  
  
  # ----------------------------------------------------------
  # Comprobar columnas tipo list
  # ----------------------------------------------------------
  
  list_columns <- names(
    gb_data
  )[sapply(
    gb_data,
    is.list
  )]
  
  
  if (length(list_columns) > 0) {
    
    stop(
      paste0(
        "El dataframe Gradient Boosting contiene ",
        "columnas tipo list: ",
        paste(
          list_columns,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # 6.9. Eliminar runs con NA
  # ----------------------------------------------------------
  
  gb_data <- gb_data %>%
    filter(
      complete.cases(.)
    )
  
  
  # ==========================================================
  # 6.10. Comprobar número de observaciones
  # ==========================================================
  
  n_runs <- nrow(gb_data)
  n_inputs_gb <- length(selected_input_ids)
  
  
  if (
    n_runs < 20 ||
    n_inputs_gb < 1
  ) {
    
    input_output_list[[i]] <- selected_inputs
    
    gb_model_results_list[[i]] <- tibble(
      output_id = output_id_i,
      n_inputs_gb = n_inputs_gb,
      n_runs_gb = n_runs,
      cv_rmse = NA_real_,
      test_rmse = NA_real_,
      test_r2 = NA_real_,
      best_eta = NA_real_,
      best_max_depth = NA_integer_,
      best_min_child_weight = NA_real_,
      best_subsample = NA_real_,
      best_colsample_bytree = NA_real_,
      best_gamma = NA_real_,
      best_lambda = NA_real_,
      best_alpha = NA_real_,
      best_nrounds = NA_integer_
    )
    
    next
  }
  
  
  # ==========================================================
  # 6.11. Train / Test split POR RUN
  # ==========================================================
  
  set.seed(
    123 + output_id_i
  )
  
  
  train_ids <- sample(
    gb_data$run_id,
    size = floor(
      train_fraction * n_runs
    )
  )
  
  
  train_data <- gb_data %>%
    filter(
      run_id %in% train_ids
    )
  
  
  test_data <- gb_data %>%
    filter(
      !run_id %in% train_ids
    )
  
  
  # ----------------------------------------------------------
  # Número real de folds
  # ----------------------------------------------------------
  
  n_folds_actual <- min(
    n_folds,
    nrow(train_data)
  )
  
  
  # ==========================================================
  # 6.12. Crear matrices X e y
  # ==========================================================
  
  predictor_names <- paste0(
    "input_",
    selected_input_ids
  )
  
  
  X_train <- train_data %>%
    select(
      all_of(predictor_names)
    ) %>%
    as.matrix()
  
  
  y_train <- train_data$error_abs
  
  
  X_test <- test_data %>%
    select(
      all_of(predictor_names)
    ) %>%
    as.matrix()
  
  
  y_test <- test_data$error_abs
  
  
  dtrain <- xgb.DMatrix(
    data = X_train,
    label = y_train
  )
  
  
  dtest <- xgb.DMatrix(
    data = X_test,
    label = y_test
  )
  
  
  # ==========================================================
  # 6.13. GRID SEARCH
  # ==========================================================
  
  # ----------------------------------------------------------
  # Learning rate
  # ----------------------------------------------------------
  
  eta_values <- c(
    0.01,
    0.03,
    0.05,
    0.10
  )
  
  
  # ----------------------------------------------------------
  # Profundidad de los árboles
  # ----------------------------------------------------------
  
  max_depth_values <- c(
    2,
    3,
    4,
    5,
    6
  )
  
  
  # ----------------------------------------------------------
  # Mínimo peso necesario en un nodo hijo
  # ----------------------------------------------------------
  
  min_child_weight_values <- c(
    1,
    3,
    5
  )
  
  
  # ----------------------------------------------------------
  # Fracción de observaciones utilizada en cada árbol
  # ----------------------------------------------------------
  
  subsample_values <- c(
    0.70,
    0.85,
    1.00
  )
  
  
  # ----------------------------------------------------------
  # Fracción de variables utilizada en cada árbol
  # ----------------------------------------------------------
  
  colsample_bytree_values <- c(
    0.70,
    0.85,
    1.00
  )
  
  
  # ----------------------------------------------------------
  # Gamma
  # ----------------------------------------------------------
  
  gamma_values <- c(
    0,
    0.1
  )
  
  
  # ----------------------------------------------------------
  # Regularización L2
  # ----------------------------------------------------------
  
  lambda_values <- c(
    1,
    5
  )
  
  
  # ----------------------------------------------------------
  # Regularización L1
  #
  # Para no hacer el grid excesivamente grande,
  # dejamos dos posibilidades.
  # ----------------------------------------------------------
  
  alpha_values <- c(
    0,
    0.1
  )
  
  
  # ----------------------------------------------------------
  # IMPORTANTE:
  #
  # Este grid es bastante amplio.
  #
  # Para evitar una explosión combinatoria innecesaria,
  # gamma/lambda/alpha se mantienen relativamente
  # conservadores.
  # ----------------------------------------------------------
  
  gb_grid <- expand.grid(
    eta = eta_values,
    max_depth = max_depth_values,
    min_child_weight = min_child_weight_values,
    subsample = subsample_values,
    colsample_bytree = colsample_bytree_values
  )
  
  
  # ==========================================================
  # 6.14. Cross-validation para cada combinación
  # ==========================================================
  
  cv_results <- vector(
    "list",
    nrow(gb_grid)
  )
  
  
  for (g in seq_len(nrow(gb_grid))) {
    
    current_eta <- gb_grid$eta[g]
    current_depth <- gb_grid$max_depth[g]
    current_min_child <- gb_grid$min_child_weight[g]
    current_subsample <- gb_grid$subsample[g]
    current_colsample <- gb_grid$colsample_bytree[g]
    
    
    # --------------------------------------------------------
    # Parámetros XGBoost
    # --------------------------------------------------------
    
    current_params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      
      eta = current_eta,
      max_depth = current_depth,
      min_child_weight = current_min_child,
      subsample = current_subsample,
      colsample_bytree = current_colsample,
      
      # Regularización
      gamma = 0,
      lambda = 1,
      alpha = 0
    )
    
    
    # --------------------------------------------------------
    # Cross-validation
    # --------------------------------------------------------
    
    set.seed(
      1000 +
        output_id_i +
        g
    )
    
    
    cv_model <- xgb.cv(
      params = current_params,
      data = dtrain,
      
      nrounds = max_nrounds,
      
      nfold = n_folds_actual,
      
      verbose = 0,
      
      early_stopping_rounds = early_stopping_rounds,
      
      maximize = FALSE,
      
      prediction = FALSE
    )
    
    
    # --------------------------------------------------------
    # Mejor número de árboles para esta combinación
    # --------------------------------------------------------
    
    best_row <- cv_model$evaluation_log %>%
      filter(
        test_rmse_mean == min(
          test_rmse_mean,
          na.rm = TRUE
        )
      ) %>%
      slice(1)
    
    best_iteration_current <- best_row$iter
    
    best_rmse_current <- best_row$test_rmse_mean
    
    
    cv_results[[g]] <- tibble(
      eta = current_eta,
      max_depth = current_depth,
      min_child_weight = current_min_child,
      subsample = current_subsample,
      colsample_bytree = current_colsample,
      cv_rmse = best_rmse_current,
      best_nrounds = best_iteration_current
    )
    
    
    # --------------------------------------------------------
    # Progreso
    # --------------------------------------------------------
    
    if (
      g %% 10 == 0 ||
      g == nrow(gb_grid)
    ) {
      
      cat(
        "Output:",
        output_id_i,
        "| Grid:",
        g,
        "/",
        nrow(gb_grid),
        "\n"
      )
    }
  }
  
  
  # ==========================================================
  # 6.15. Seleccionar mejor combinación
  # ==========================================================
  
  cv_results <- bind_rows(
    cv_results
  ) %>%
    arrange(
      cv_rmse
    )
  
  
  best_model_parameters <- cv_results %>%
    slice(
      1
    )
  
  
  best_eta <- best_model_parameters$eta
  
  best_max_depth <- best_model_parameters$max_depth
  
  best_min_child_weight <-
    best_model_parameters$min_child_weight
  
  best_subsample <-
    best_model_parameters$subsample
  
  best_colsample_bytree <-
    best_model_parameters$colsample_bytree
  
  best_cv_rmse <-
    best_model_parameters$cv_rmse
  
  best_nrounds <-
    best_model_parameters$best_nrounds
  
  
  # ==========================================================
  # 6.16. Entrenar modelo final sobre TODO el TRAIN
  # ==========================================================
  
  final_params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    
    eta = best_eta,
    max_depth = best_max_depth,
    min_child_weight = best_min_child_weight,
    subsample = best_subsample,
    colsample_bytree = best_colsample_bytree,
    
    gamma = 0,
    lambda = 1,
    alpha = 0
  )
  
  
  set.seed(
    789 + output_id_i
  )
  
  
  gb_final <- xgb.train(
    params = final_params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  
  # ==========================================================
  # 6.17. Evaluación sobre TEST
  # ==========================================================
  
  test_predictions <- predict(
    gb_final,
    dtest
  )
  
  
  # ----------------------------------------------------------
  # RMSE
  # ----------------------------------------------------------
  
  test_rmse <- sqrt(
    mean(
      (
        y_test -
          test_predictions
      )^2
    )
  )
  
  
  # ----------------------------------------------------------
  # R²
  # ----------------------------------------------------------
  
  if (
    var(y_test) > 0
  ) {
    
    test_r2 <- 1 -
      sum(
        (
          y_test -
            test_predictions
        )^2
      ) /
      sum(
        (
          y_test -
            mean(y_test)
        )^2
      )
    
  } else {
    
    test_r2 <- NA_real_
  }
  
  
  # ==========================================================
  # 6.18. Importancia de las variables
  # ==========================================================
  
  importance_results <- xgb.importance(
    feature_names = predictor_names,
    model = gb_final
  )
  
  
  # ----------------------------------------------------------
  # Si XGBoost devuelve importance
  # ----------------------------------------------------------
  
  if (
    nrow(importance_results) > 0
  ) {
    
    importance_results <- importance_results %>%
      as_tibble() %>%
      rename(
        variable = Feature,
        importance_gain = Gain,
        importance_cover = Cover,
        importance_frequency = Frequency
      ) %>%
      mutate(
        input_id = as.integer(
          sub(
            "input_",
            "",
            variable
          )
        )
      )
    
    # --------------------------------------------------------
    # Normalización del Gain
    # --------------------------------------------------------
    
    importance_results <- importance_results %>%
      mutate(
        importance_relative =
          importance_gain /
          sum(
            importance_gain,
            na.rm = TRUE
          )
      )
    
  } else {
    
    importance_results <- tibble(
      variable = character(),
      importance_gain = numeric(),
      importance_cover = numeric(),
      importance_frequency = numeric(),
      input_id = integer(),
      importance_relative = numeric()
    )
  }
  
  
  # ==========================================================
  # 6.19. Añadir importancia al dataframe de correlaciones
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
          importance_gain,
          importance_cover,
          importance_frequency,
          importance_relative
        ),
      by = "input_id"
    ) %>%
    mutate(
      
      # Métricas del modelo
      cv_rmse = best_cv_rmse,
      test_rmse = test_rmse,
      test_r2 = test_r2,
      
      # Hiperparámetros
      best_eta = best_eta,
      best_max_depth = best_max_depth,
      best_min_child_weight =
        best_min_child_weight,
      best_subsample = best_subsample,
      best_colsample_bytree =
        best_colsample_bytree,
      best_nrounds = best_nrounds,
      
      # Información del dataset
      n_runs_gb = n_runs,
      n_inputs_gb = n_inputs_gb
    ) %>%
    arrange(
      desc(
        abs(correlation)
      )
    )
  
  
  # ==========================================================
  # 6.20. Guardar resultados del output
  # ==========================================================
  
  input_output_list[[i]] <-
    correlation_results_final
  
  
  # ==========================================================
  # 6.21. Guardar métricas del modelo
  # ==========================================================
  
  gb_model_results_list[[i]] <- tibble(
    
    output_id = output_id_i,
    
    n_inputs_gb = n_inputs_gb,
    n_runs_gb = n_runs,
    
    cv_rmse = best_cv_rmse,
    test_rmse = test_rmse,
    test_r2 = test_r2,
    
    best_eta = best_eta,
    best_max_depth = best_max_depth,
    best_min_child_weight =
      best_min_child_weight,
    best_subsample = best_subsample,
    best_colsample_bytree =
      best_colsample_bytree,
    
    best_nrounds = best_nrounds
  )
  
  
  # ==========================================================
  # 6.22. Mensaje de progreso
  # ==========================================================
  
  cat(
    "\n",
    "====================================================\n",
    "Output:",
    output_id_i,
    "\n",
    "Inputs:",
    n_inputs_gb,
    "\n",
    "Runs:",
    n_runs,
    "\n",
    "CV RMSE:",
    round(
      best_cv_rmse,
      4
    ),
    "\n",
    "Test RMSE:",
    round(
      test_rmse,
      4
    ),
    "\n",
    "Test R2:",
    round(
      test_r2,
      4
    ),
    "\n",
    "Best eta:",
    best_eta,
    "\n",
    "Best max_depth:",
    best_max_depth,
    "\n",
    "Best min_child_weight:",
    best_min_child_weight,
    "\n",
    "Best subsample:",
    best_subsample,
    "\n",
    "Best colsample_bytree:",
    best_colsample_bytree,
    "\n",
    "Best nrounds:",
    best_nrounds,
    "\n",
    "====================================================\n"
  )
}


# ============================================================
# 7. Construir dataframes finales
# ============================================================

input_output_drivers <- bind_rows(
  input_output_list
)


gb_model_results <- bind_rows(
  gb_model_results_list
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
  gb_model_results,
  "gb_model_results.csv",
  row.names = FALSE
)

