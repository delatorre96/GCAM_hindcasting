library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(xgboost)

source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")


df_pca_xgboost <- read.csv('df_best_components.csv') %>%
  select(-MAE_log)



con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis/gcam_sensitivity.sqlite"
)

params <- list('logit' = c("EXP_5a2e3191",
                            "EXP_2e96286f",
                            "EXP_07b59f0c"),
               'satiation_level' = c('EXP_89083094'),
               'price_elasticity' = c('EXP_9e51df1d'))

inputsPerParam <- list()

for (param in names(params)){
  
  cat(paste0('********************************', param, '********************************'))
  
  # experiments <- dbReadTable(con, "Experiments")
  experiment_id <- params[[param]]
  
  
  inputs <- read_experiment_inputs(
    con = con,
    experiment_id
  )  
 
  # 1. Inputs únicos
  input_keys <- inputs %>%
    select(
     c(xml_file, xpath)
    ) %>%
    distinct() %>%
    mutate(input_id = row_number())
  
  
  inputs <- inputs %>%
    left_join(
      input_keys,
      by = c('xml_file', 'xpath')
    )
  
  inputs_id_wide <- inputs %>%
    select(run_id, input_id, param) %>%
    pivot_wider(
      id_cols = run_id,
      names_from = input_id,
      values_from = param,
      names_prefix = "input_"
    )
  
  cols2keep <- setdiff(names(inputs), c("year","run_id", param))
  input_keys <- inputs %>% 
    select(all_of(cols2keep)) %>%
    distinct()
  
  #### Estimacion
  
  
  # ============================================================
  # 1. Parámetros
  # ============================================================
  
  correlation_threshold <- 0.10
  
  # Umbral de importancia acumulada
  importance_cumulative_threshold <- 0.99
  
  set.seed(123)
  
  train_fraction <- 0.80
  
  # CV más ligero
  n_folds <- 3
  
  # Rondas máximas
  max_nrounds <- 1000
  early_stopping_rounds <- 30
  
  
  # ============================================================
  # 2. Identificar componentes PCA
  # ============================================================
  
  pc_names <- names(df_pca_xgboost) %>%
    setdiff("run_id")
  
  
  # Comprobar que son realmente PCs
  pc_names <- pc_names[
    grepl("^PC[0-9]+$", pc_names)
  ]
  
  
  if (length(pc_names) == 0) {
    stop(
      "No se han encontrado columnas PC en df_pca_xgboost."
    )
  }
  
  
  # ============================================================
  # 3. Identificar inputs
  # ============================================================
  
  input_names <- names(inputs_id_wide) %>%
    grep(
      pattern = "^input_[0-9]+$",
      value = TRUE
    )
  
  
  if (length(input_names) == 0) {
    stop(
      "No se han encontrado columnas input_* en inputs_id_wide."
    )
  }
  
  
  # ============================================================
  # 4. Comprobar run_id
  # ============================================================
  
  if (anyDuplicated(df_pca_xgboost$run_id) > 0) {
    stop(
      "df_pca_xgboost contiene run_id duplicados."
    )
  }
  
  
  if (anyDuplicated(inputs_id_wide$run_id) > 0) {
    stop(
      "inputs_id_wide contiene run_id duplicados."
    )
  }
  
  
  # ============================================================
  # 5. Unir PCA + inputs
  # ============================================================
  
  pca_inputs <- df_pca_xgboost %>%
    select(
      run_id,
      all_of(pc_names)
    ) %>%
    inner_join(
      inputs_id_wide %>%
        select(
          run_id,
          all_of(input_names)
        ),
      by = "run_id"
    )
  
  
  cat(
    "Runs comunes:",
    nrow(pca_inputs),
    "\n"
  )
  
  cat(
    "Componentes PCA:",
    length(pc_names),
    "\n"
  )
  
  cat(
    "Inputs disponibles:",
    length(input_names),
    "\n"
  )
  
  
  # ============================================================
  # 6. Inicializar resultados
  # ============================================================
 
  input_output_list <- vector(
    "list",
    length(pc_names)
  )
  
  gb_models_list <- vector(
    "list",
    length(pc_names)
  )
  
  names(input_output_list) <- pc_names
  names(gb_models_list) <- pc_names
  
  # ============================================================
  # 7. Grid de hiperparámetros
  # ============================================================
  
  # Grid reducido:
  #
  # 3 eta
  # × 3 max_depth
  # × 2 min_child_weight
  # × subsample fijo
  # × colsample_bytree fijo
  #
  # = 18 combinaciones
  
  eta_values <- c(
    0.03,
    0.05,
    0.10
  )
  
  max_depth_values <- c(
    2,
    4,
    6
  )
  
  min_child_weight_values <- c(
    1,
    5
  )
  
  subsample_values <- 0.80
  
  colsample_bytree_values <- 0.80
  
  
  gb_grid <- expand.grid(
    eta = eta_values,
    max_depth = max_depth_values,
    min_child_weight = min_child_weight_values,
    subsample = subsample_values,
    colsample_bytree = colsample_bytree_values
  )
  
  
  cat(
    "Combinaciones de hiperparámetros:",
    nrow(gb_grid),
    "\n"
  )
  
  
  # ============================================================
  # 8. LOOP POR COMPONENTE
  # ============================================================
  
  for (i in seq_along(pc_names)) {
    
    output_id_i <- pc_names[i]
    
    
    cat(
      "\n\n",
      "====================================================\n",
      "Procesando:",
      output_id_i,
      "\n",
      "====================================================\n"
    )
    
    
    # ==========================================================
    # 8.1. Dataset del componente
    # ==========================================================
    
    current_data <- pca_inputs %>%
      select(
        run_id,
        all_of(output_id_i),
        all_of(input_names)
      )
    
    
    # ==========================================================
    # 8.2. Correlación PC ~ input
    # ==========================================================
    
    correlation_results <- map_dfr(
      input_names,
      function(input_name) {
        
        x <- current_data[[input_name]]
        y <- current_data[[output_id_i]]
        
        valid <- complete.cases(x, y)
        
        if (sum(valid) < 3) {
          
          return(
            tibble(
              input = input_name,
              input_id = as.integer(
                sub(
                  "^input_",
                  "",
                  input_name
                )
              ),
              correlation = NA_real_,
              n_correlation = sum(valid)
            )
          )
        }
        
        tibble(
          input = input_name,
          input_id = as.integer(
            sub(
              "^input_",
              "",
              input_name
            )
          ),
          correlation = cor(
            x[valid],
            y[valid],
            method = "pearson"
          ),
          n_correlation = sum(valid)
        )
      }
    )
    
    
    # ==========================================================
    # 8.3. Seleccionar inputs por correlación
    # ==========================================================
    
    selected_inputs <- correlation_results %>%
      filter(
        !is.na(correlation),
        abs(correlation) >= correlation_threshold
      ) %>%
      arrange(
        desc(abs(correlation))
      )
    
    
    n_inputs_selected <- nrow(
      selected_inputs
    )
    
    
    cat(
      "Inputs seleccionados por correlación:",
      n_inputs_selected,
      "\n"
    )
    
    
    # ==========================================================
    # 8.4. Si no hay inputs
    # ==========================================================
    
    if (n_inputs_selected == 0) {
      
      input_output_list[[i]] <- selected_inputs
      # 
      # gb_model_results_list[[i]] <- tibble(
      #   output_id = output_id_i,
      #   n_inputs_correlation = 0,
      #   n_inputs_gb = 0,
      #   n_runs_gb = 0,
      #   cv_rmse = NA_real_,
      #   cv_rmse_sd = NA_real_,
      #   test_rmse = NA_real_,
      #   test_r2 = NA_real_,
      #   best_eta = NA_real_,
      #   best_max_depth = NA_integer_,
      #   best_min_child_weight = NA_real_,
      #   best_subsample = NA_real_,
      #   best_colsample_bytree = NA_real_,
      #   best_nrounds = NA_integer_
      # )
      
      next
    }
    
    
    # ==========================================================
    # 8.5. Variables seleccionadas
    # ==========================================================
    
    selected_input_names <- selected_inputs$input
    
    
    # ==========================================================
    # 8.6. Dataset para XGBoost
    # ==========================================================
    
    gb_data <- current_data %>%
      select(
        run_id,
        all_of(output_id_i),
        all_of(selected_input_names)
      ) %>%
      drop_na()
    
    
    n_runs <- nrow(
      gb_data
    )
    
    
    cat(
      "Runs disponibles para GB:",
      n_runs,
      "\n"
    )
    
    
    # ==========================================================
    # 8.7. Comprobar número de observaciones
    # ==========================================================
    
    if (n_runs < 20) {
      
      input_output_list[[i]] <-
        selected_inputs
      
      # gb_model_results_list[[i]] <- tibble(
      #   output_id = output_id_i,
      #   n_inputs_correlation = n_inputs_selected,
      #   n_inputs_gb = n_inputs_selected,
      #   n_runs_gb = n_runs,
      #   cv_rmse = NA_real_,
      #   cv_rmse_sd = NA_real_,
      #   test_rmse = NA_real_,
      #   test_r2 = NA_real_,
      #   best_eta = NA_real_,
      #   best_max_depth = NA_integer_,
      #   best_min_child_weight = NA_real_,
      #   best_subsample = NA_real_,
      #   best_colsample_bytree = NA_real_,
      #   best_nrounds = NA_integer_
      # )
      
      next
    }
    
    
    # ==========================================================
    # 8.8. Train / Test split
    # ==========================================================
    
    set.seed(
      123 + i
    )
    
    
    train_indices <- sample(
      seq_len(n_runs),
      size = floor(
        train_fraction * n_runs
      )
    )
    
    
    train_data <- gb_data[
      train_indices,
      ,
      drop = FALSE
    ]
    
    
    test_data <- gb_data[
      -train_indices,
      ,
      drop = FALSE
    ]
    
    
    # ==========================================================
    # 8.9. Número real de folds
    # ==========================================================
    
    n_folds_actual <- min(
      n_folds,
      nrow(train_data)
    )
    
    
    # ==========================================================
    # 8.10. Matrices X / y
    # ==========================================================
    
    X_train <- train_data %>%
      select(
        all_of(selected_input_names)
      ) %>%
      as.matrix()
    
    
    y_train <- train_data[[output_id_i]]
    
    
    X_test <- test_data %>%
      select(
        all_of(selected_input_names)
      ) %>%
      as.matrix()
    
    
    y_test <- test_data[[output_id_i]]
    
    
    dtrain <- xgb.DMatrix(
      data = X_train,
      label = y_train
    )
    
    
    dtest <- xgb.DMatrix(
      data = X_test,
      label = y_test
    )
    
    
    # ==========================================================
    # 8.11. Cross-validation + Grid Search
    # ==========================================================
    
    cv_results <- vector(
      "list",
      nrow(gb_grid)
    )
    
    
    for (g in seq_len(nrow(gb_grid))) {
      
      current_params <- list(
        
        objective = "reg:squarederror",
        
        eval_metric = "rmse",
        
        eta =
          gb_grid$eta[g],
        
        max_depth =
          gb_grid$max_depth[g],
        
        min_child_weight =
          gb_grid$min_child_weight[g],
        
        subsample =
          gb_grid$subsample[g],
        
        colsample_bytree =
          gb_grid$colsample_bytree[g],
        
        gamma = 0,
        
        lambda = 1,
        
        alpha = 0
      )
      
      
      set.seed(
        1000 +
          i * 10000 +
          g
      )
      
      
      cv_model <- xgb.cv(
        
        params = current_params,
        
        data = dtrain,
        
        nrounds = max_nrounds,
        
        nfold = n_folds_actual,
        
        verbose = 0,
        
        early_stopping_rounds =
          early_stopping_rounds,
        
        maximize = FALSE,
        
        prediction = FALSE
      )
      
      
      # --------------------------------------------------------
      # Mejor iteración
      # --------------------------------------------------------
      
      evaluation_log <-
        cv_model$evaluation_log
      
      
      best_row <- evaluation_log %>%
        filter(
          test_rmse_mean ==
            min(
              test_rmse_mean,
              na.rm = TRUE
            )
        ) %>%
        slice(1)
      
      
      cv_results[[g]] <- tibble(
        
        eta =
          gb_grid$eta[g],
        
        max_depth =
          gb_grid$max_depth[g],
        
        min_child_weight =
          gb_grid$min_child_weight[g],
        
        subsample =
          gb_grid$subsample[g],
        
        colsample_bytree =
          gb_grid$colsample_bytree[g],
        
        cv_rmse =
          best_row$test_rmse_mean,
        
        cv_rmse_sd =
          best_row$test_rmse_std,
        
        best_nrounds =
          best_row$iter
      )
      
      
      # --------------------------------------------------------
      # Progreso
      # --------------------------------------------------------
      
      if (
        g %% 5 == 0 ||
        g == nrow(gb_grid)
      ) {
        
        cat(
          "  Grid:",
          g,
          "/",
          nrow(gb_grid),
          "\n"
        )
      }
    }
    
    
    # ==========================================================
    # 8.12. Mejor configuración
    # ==========================================================
    
    cv_results <- bind_rows(
      cv_results
    ) %>%
      arrange(
        cv_rmse
      )
    
    
    best_model_parameters <-
      cv_results %>%
      slice(1)
    
    
    best_eta <-
      best_model_parameters$eta
    
    best_max_depth <-
      best_model_parameters$max_depth
    
    best_min_child_weight <-
      best_model_parameters$min_child_weight
    
    best_subsample <-
      best_model_parameters$subsample
    
    best_colsample_bytree <-
      best_model_parameters$colsample_bytree
    
    best_cv_rmse <-
      best_model_parameters$cv_rmse
    
    best_cv_rmse_sd <-
      best_model_parameters$cv_rmse_sd
    
    best_nrounds <-
      best_model_parameters$best_nrounds
    
    
    # ==========================================================
    # 8.13. Entrenamiento final
    # ==========================================================
    
    final_params <- list(
      
      objective = "reg:squarederror",
      
      eval_metric = "rmse",
      
      eta = best_eta,
      
      max_depth =
        best_max_depth,
      
      min_child_weight =
        best_min_child_weight,
      
      subsample =
        best_subsample,
      
      colsample_bytree =
        best_colsample_bytree,
      
      gamma = 0,
      
      lambda = 1,
      
      alpha = 0
    )
    
    
    set.seed(
      789 + i
    )
    
    
    gb_final <- xgb.train(
      
      params = final_params,
      
      data = dtrain,
      
      nrounds = best_nrounds,
      
      verbose = 0
    )
    
    
    # ==========================================================
    # 8.14. Predicción TEST
    # ==========================================================
    
    test_predictions <- predict(
      gb_final,
      dtest
    )
    
    
    # ==========================================================
    # 8.15. Métricas TEST
    # ==========================================================
    
    test_rmse <- sqrt(
      mean(
        (
          y_test -
            test_predictions
        )^2
      )
    )
    
    
    # R²
    ss_res <- sum(
      (
        y_test -
          test_predictions
      )^2
    )
    
    
    ss_tot <- sum(
      (
        y_test -
          mean(y_test)
      )^2
    )
    
    
    test_r2 <- if (
      ss_tot > 0
    ) {
      
      1 -
        ss_res /
        ss_tot
      
    } else {
      
      NA_real_
    }
    
    
    # ==========================================================
    # 8.16. Importancia de variables
    # ==========================================================
    
    importance_results <- xgb.importance(
      feature_names =
        selected_input_names,
      model = gb_final
    )
    
    
    if (
      nrow(importance_results) > 0
    ) {
      
      importance_results <-
        importance_results %>%
        as_tibble() %>%
        rename(
          
          variable = Feature,
          
          importance_gain = Gain,
          
          importance_cover = Cover,
          
          importance_frequency = Frequency
          
        ) %>%
        mutate(
          
          input_id =
            as.integer(
              sub(
                "^input_",
                "",
                variable
              )
            )
        ) %>%
        
        # ------------------------------------------------------
      # Ordenar por Gain
      # ------------------------------------------------------
      
      arrange(
        desc(importance_gain)
      ) %>%
        
        # ------------------------------------------------------
      # Importancia relativa
      # ------------------------------------------------------
      
      mutate(
        
        importance_relative =
          importance_gain /
          sum(
            importance_gain,
            na.rm = TRUE
          ),
        
        # ----------------------------------------------------
        # Importancia acumulada
        # ----------------------------------------------------
        
        importance_cumulative =
          cumsum(
            importance_relative
          )
      )
      
    } else {
      
      importance_results <- tibble(
        
        variable =
          character(),
        
        importance_gain =
          numeric(),
        
        importance_cover =
          numeric(),
        
        importance_frequency =
          numeric(),
        
        input_id =
          integer(),
        
        importance_relative =
          numeric(),
        
        importance_cumulative =
          numeric()
      )
    }
    
    
    # ==========================================================
    # 8.17. Seleccionar variables hasta el 99% de importancia
    # ==========================================================
    
    if (nrow(importance_results) > 0) {
      
      # Primera variable que hace que la importancia acumulada
      # alcance el umbral. Esa variable también se conserva.
      
      cutoff_position <- which(
        importance_results$importance_cumulative >=
          importance_cumulative_threshold
      )[1]
      
      # Por seguridad, si no se alcanza el umbral,
      # conservar todas las variables.
      
      if (is.na(cutoff_position)) {
        cutoff_position <- nrow(importance_results)
      }
      
      importance_selected <- importance_results %>%
        slice(seq_len(cutoff_position))
      
    } else {
      
      importance_selected <- importance_results
    }
    
    
    # Número final de inputs
    n_inputs_gb <- nrow(importance_selected)
    
    
    # Variables definitivas del modelo
    final_input_names <- importance_selected$variable
    
    
    cat(
      "Inputs retenidos por importancia acumulada:",
      n_inputs_gb,
      "\n"
    )
    
    
    cat(
      "Importancia acumulada final:",
      ifelse(
        n_inputs_gb > 0,
        round(
          max(
            importance_selected$importance_cumulative
          ),
          4
        ),
        NA
      ),
      "\n"
    )
    
    
    # ==========================================================
    # 8.18. Dataset FINAL para reentrenar XGBoost
    # ==========================================================
    
    gb_data_final <- current_data %>%
      select(
        run_id,
        all_of(output_id_i),
        all_of(final_input_names)
      ) %>%
      drop_na()
    
    
    n_runs_final <- nrow(gb_data_final)
    
    
    cat(
      "Runs disponibles para modelo final:",
      n_runs_final,
      "\n"
    )
    
    
    # ==========================================================
    # 8.19. Train / Test split FINAL
    # ==========================================================
    
    # IMPORTANTE:
    # Utilizamos los mismos índices del split original siempre
    # que las observaciones sigan presentes.
    
    final_train_data <- gb_data_final %>%
      filter(
        run_id %in% train_data$run_id
      )
    
    final_test_data <- gb_data_final %>%
      filter(
        run_id %in% test_data$run_id
      )
    
    n_runs_final <- nrow(gb_data_final)
    
    # ==========================================================
    # 8.20. Matrices X / y del modelo FINAL
    # ==========================================================
    
    X_train_final <- final_train_data %>%
      select(
        all_of(final_input_names)
      ) %>%
      as.matrix()
    
    
    y_train_final <- final_train_data[[output_id_i]]
    
    
    X_test_final <- final_test_data %>%
      select(
        all_of(final_input_names)
      ) %>%
      as.matrix()
    
    
    y_test_final <- final_test_data[[output_id_i]]
    
    
    dtrain_final <- xgb.DMatrix(
      data = X_train_final,
      label = y_train_final
    )
    
    
    dtest_final <- xgb.DMatrix(
      data = X_test_final,
      label = y_test_final
    )
    
    
    # ==========================================================
    # 8.21. REENTRENAMIENTO DEL MODELO FINAL
    # ==========================================================
    
    # Utilizamos exactamente los hiperparámetros encontrados
    # en el grid search anterior.
    
    set.seed(
      789 + i
    )
    
    gb_final <- xgb.train(
      
      params = final_params,
      
      data = dtrain_final,
      
      nrounds = best_nrounds,
      
      verbose = 0
    )
    
    
    # ==========================================================
    # 8.22. Predicción TEST del modelo FINAL
    # ==========================================================
    
    test_predictions_final <- predict(
      gb_final,
      dtest_final
    )
    
    
    # ==========================================================
    # 8.23. Métricas TEST del modelo FINAL
    # ==========================================================
    
    test_rmse_final <- sqrt(
      mean(
        (
          y_test_final -
            test_predictions_final
        )^2
      )
    )
    
    
    # R²
    ss_res_final <- sum(
      (
        y_test_final -
          test_predictions_final
      )^2
    )
    
    
    ss_tot_final <- sum(
      (
        y_test_final -
          mean(y_test_final)
      )^2
    )
    
    
    test_r2_final <- if (
      ss_tot_final > 0
    ) {
      
      1 -
        ss_res_final /
        ss_tot_final
      
    } else {
      
      NA_real_
    }
    
    
    # ==========================================================
    # 8.24. Importancia del MODELO FINAL
    # ==========================================================
    
    importance_final <- xgb.importance(
      feature_names = final_input_names,
      model = gb_final
    )
    
    
    if (nrow(importance_final) > 0) {
      
      importance_final <- importance_final %>%
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
              "^input_",
              "",
              variable
            )
          )
        ) %>%
        arrange(
          desc(importance_gain)
        ) %>%
        mutate(
          importance_relative =
            importance_gain /
            sum(
              importance_gain,
              na.rm = TRUE
            ),
          
          importance_cumulative =
            cumsum(
              importance_relative
            )
        )
      
    } else {
      
      importance_final <- tibble(
        variable = character(),
        importance_gain = numeric(),
        importance_cover = numeric(),
        importance_frequency = numeric(),
        input_id = integer(),
        importance_relative = numeric(),
        importance_cumulative = numeric()
      )
    }
    
    
    # ==========================================================
    # 8.25. Combinar correlación + importancia FINAL
    # ==========================================================
    
    correlation_results_final <-
      selected_inputs %>%
      
      inner_join(
        importance_final %>%
          select(
            input_id,
            importance_gain,
            importance_cover,
            importance_frequency,
            importance_relative,
            importance_cumulative
          ),
        by = "input_id"
      ) %>%
      
      mutate(
        
        output_id =
          output_id_i,
        
        cv_rmse =
          best_cv_rmse,
        
        cv_rmse_sd =
          best_cv_rmse_sd,
        
        test_rmse =
          test_rmse_final,
        
        test_r2 =
          test_r2_final,
        
        best_eta =
          best_eta,
        
        best_max_depth =
          best_max_depth,
        
        best_min_child_weight =
          best_min_child_weight,
        
        best_subsample =
          best_subsample,
        
        best_colsample_bytree =
          best_colsample_bytree,
        
        best_nrounds =
          best_nrounds,
        
        n_runs_gb =
          n_runs_final,
        
        n_inputs_correlation =
          n_inputs_selected,
        
        n_inputs_gb =
          n_inputs_gb,
        
        importance_threshold =
          importance_cumulative_threshold
      ) %>%
      
      arrange(
        desc(
          importance_gain
        )
      )
    
    
    # ==========================================================
    # 8.26. Guardar modelo FINAL
    # ==========================================================
    
    gb_models_list[[i]] <- list(
      
      # Modelo XGBoost FINAL
      model = gb_final,
      
      # Identificación
      output_id = output_id_i,
      
      # Variables definitivas
      input_names = final_input_names,
      feature_names = final_input_names,
      input_ids = importance_selected$input_id,
      
      # Información completa de los inputs
      input_keys = input_keys %>%
        filter(
          input_id %in% importance_selected$input_id
        ),
      
      # Hiperparámetros
      params = final_params,
      nrounds = best_nrounds,
      
      # Información del entrenamiento
      n_runs = n_runs_final,
      train_fraction = train_fraction,
      
      # Selección de variables
      correlation_threshold =
        correlation_threshold,
      
      importance_cumulative_threshold =
        importance_cumulative_threshold,
      
      # Métricas CV
      cv_rmse = best_cv_rmse,
      cv_rmse_sd = best_cv_rmse_sd,
      
      # Métricas modelo FINAL
      test_rmse = test_rmse_final,
      test_r2 = test_r2_final,
      
      # Importancia modelo FINAL
      importance = importance_final
    )
    
    
    # ==========================================================
    # 8.27. Guardar resultados de este PC
    # ==========================================================
    
    input_output_list[[i]] <-
      correlation_results_final

    
  
  }
  
  input_output_drivers <- bind_rows(
    input_output_list
  )
  
  input_keys_drivers <- input_output_drivers %>% 
    select(
      input_id,
      correlation,
      importance_gain,
      importance_cumulative,
      output_id,
      test_rmse,
      test_r2
    ) %>%
    left_join(
      input_keys,
      by = "input_id"
    )
  
  inputsPerParam[[param]] <- list(
    drivers = input_keys_drivers,
    model = gb_models_list
  )
  
}


saveRDS(
  inputsPerParam,
  "gradient_boosting_models.rds"
)