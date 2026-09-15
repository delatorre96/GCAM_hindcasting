library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

################### DATA ########################

######## outputs PCA #########
df_MAE_log <- read.csv('outputs_PCA.csv') %>%
  select(run_id,MAE_log) # Nos quedamos solo con MAE_log





con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis/gcam_sensitivity.sqlite"
)

experiments <- dbReadTable(con, "Experiments")
experiment_id <- c("EXP_5a2e3191",
                   "EXP_2e96286f",
                   "EXP_07b59f0c")


source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

inputs <- read_experiment_inputs(
  con = con,
  experiment_id
)  %>% 
  select(-xml_file, -xpath, -fillout, -year) %>%
  filter(!is.na(supplysector),
         !is.na(subsector),
         !is.na(nesting_subsector))


# 1. Inputs únicos

input_keys <- inputs %>%
  select(
    region,
    supplysector,
    subsector,
    nesting_subsector
  ) %>%
  distinct() %>%
  mutate(input_id = row_number())


inputs <- inputs %>%
  left_join(
    input_keys,
    by = c(
      "region", 
      "supplysector", 
      "subsector", 
      "nesting_subsector"
    )
  )

inputs_id_wide <- inputs %>%
  select(run_id, input_id, logit) %>%
  pivot_wider(
    id_cols = run_id,
    names_from = input_id,
    values_from = logit,
    names_prefix = "input_"
  )

df_total <- df_MAE_log %>%left_join(inputs_id_wide, by = 'run_id')




####  Gradient Boosting ######## 
#Explore importances
library(xgboost)
library(dplyr)
library(ggplot2)


df_model <- df_total %>%
  select(MAE_log, starts_with("input")) %>%
  na.omit()


# Train / test
set.seed(123)

idx <- sample(
  1:nrow(df_model),
  size = 0.80 * nrow(df_model)
)

train <- df_model[idx, ]
test  <- df_model[-idx, ]



# Matrices
X_train <- as.matrix(train %>% select(starts_with("input")))
X_test  <- as.matrix(test %>% select(starts_with("input")))





y_train <- train$MAE_log
y_test  <- test$MAE_log
dtrain <- xgb.DMatrix(
  data = X_train,
  label = y_train
)


# HIPERPARÁMETROS

# grid <- expand.grid(
#   max_depth = c(2, 3, 4, 5, 6),
#   eta = c(0.01, 0.03, 0.05, 0.1),
#   min_child_weight = c(1, 3, 5),
#   subsample = c(0.7, 0.8, 1),
#   colsample_bytree = c(0.7, 0.8, 1),
#   lambda = c(0, 1, 5),
#   alpha = c(0, 0.1, 1)
# )
grid <- expand.grid(
  max_depth = c(2, 3, 4),
  eta = c(0.03, 0.1),
  min_child_weight = c(3),
  subsample = c(0.8),
  colsample_bytree = c(0.8),
  lambda = c(1, 5),
  alpha = c(0)
)
nrounds <- 500


# 4. GRID SEARCH + CROSS VALIDATION
# ============================================================

results <- vector("list", nrow(grid))

set.seed(123)

for (i in 1:nrow(grid)) {
  
  params <- list(
    objective = "reg:squarederror",
    
    # Calcular las tres métricas
    eval_metric = c("rmse", "mae"),
    
    max_depth = grid$max_depth[i],
    eta = grid$eta[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i],
    lambda = grid$lambda[i],
    alpha = grid$alpha[i]
  )
  
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    nfold = 5,
    early_stopping_rounds = 30,
    verbose = 0
  )
  
  
  # ==========================================================
  # Mejor iteración según RMSE
  # ==========================================================
  
  best_iter <- which.min(
    cv$evaluation_log$test_rmse_mean
  )
  
  
  # Métricas en la mejor iteración
  best_rmse <- cv$evaluation_log$test_rmse_mean[best_iter]
  
  best_mae <- cv$evaluation_log$test_mae_mean[best_iter]
  
  
  # R² calculado a partir del RMSE
  # R² = 1 - MSE / varianza de y
  best_mse <- best_rmse^2
  
  best_r2 <- 1 - (
    best_mse / var(y_train)
  )
  
  
  # ==========================================================
  # Guardar resultados
  # ==========================================================
  
  results[[i]] <- data.frame(
    max_depth = grid$max_depth[i],
    eta = grid$eta[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i],
    lambda = grid$lambda[i],
    alpha = grid$alpha[i],
    
    nrounds = best_iter,
    
    RMSE_CV = best_rmse,
    MAE_CV = best_mae,
    R2_CV = best_r2
  )
  
  
  cat(
    "Combinación", i, "/", nrow(grid),
    "- RMSE:", round(best_rmse, 4),
    "- MAE:", round(best_mae, 4),
    "- R²:", round(best_r2, 4),
    "\n"
  )
}


# Unir resultados
results <- bind_rows(results)

# Mejor según RMSE
results %>%
  arrange(RMSE_CV) %>%
  head(10)


# Mejor según MAE
results %>%
  arrange(MAE_CV) %>%
  head(10)


# Mejor según R²
results %>%
  arrange(desc(R2_CV)) %>%
  head(10)



results <- bind_rows(results)


# 5. MEJOR COMBINACIÓN


results <- results %>%
  arrange(RMSE_CV)

best_params <- results %>%
  slice(1)

print(best_params)

# Modelo

set.seed(123)

params_best <- list(
  objective = "reg:squarederror",
  
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  min_child_weight = best_params$min_child_weight,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree,
  lambda = best_params$lambda,
  alpha = best_params$alpha
)


model <- xgboost(
  data = X_train,
  label = y_train,
  
  nrounds = best_params$nrounds,
  params = params_best,
  
  verbose = 0
)

# Predicciones
pred_train <- predict(model, X_train)
pred_test <- predict(model, X_test)



# Métricas
R2_train <- 1 - sum((y_train - pred_train)^2) /
  sum((y_train - mean(y_train))^2)

R2_test <- 1 - sum((y_test - pred_test)^2) /
  sum((y_test - mean(y_test))^2)

RMSE_test <- sqrt(mean((y_test - pred_test)^2))

MAE_test <- mean(abs(y_test - pred_test))

cat("R² train :", round(R2_train, 4), "\n")
cat("R² test  :", round(R2_test, 4), "\n")
cat("RMSE test:", round(RMSE_test, 4), "\n")
cat("MAE test :", round(MAE_test, 4), "\n")


#### Seleccionar componentes mas importantes y reentrenar el modelo


# Importancia de las PCs
importance <- xgb.importance(
  feature_names = colnames(X_train),
  model = model
)

importance_99 <- importance %>%
  mutate(
    Gain_cum = cumsum(Gain),
    Gain_pct = 100 * Gain_cum
  ) %>%
  filter(
    Gain_cum <= 0.99 |
      lag(Gain_cum, default = 0) < 0.99
  )

pcs_99 <- importance_99$Feature
cols <- c("MAE_log", pcs_99)






