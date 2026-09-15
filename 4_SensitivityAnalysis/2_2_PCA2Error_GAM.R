library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

################### DATA ########################

######## outputs PCA #########

df_pca_xgboost <- read.csv('df_best_components.csv')
########  GAM  ########

library(mgcv)
library(dplyr)
library(ggplot2)



# 1. Preparar datos


df_model <- df_pca_xgboost %>%
  select(MAE_log, starts_with("PC")) %>%
  na.omit()



# 2. Train / test


set.seed(123)

idx <- sample(
  1:nrow(df_model),
  size = 0.80 * nrow(df_model)
)

train <- df_model[idx, ]
test  <- df_model[-idx, ]


y_train <- train$MAE_log
y_test  <- test$MAE_log



# 3. Identificar PCs


pc_names <- names(train)[
  grepl("^PC", names(train))
]

cat(
  "Número de PCs:",
  length(pc_names),
  "\n"
)

cat(
  "PCs:",
  paste(pc_names, collapse = ", "),
  "\n"
)



# 4. Construir fórmula GAM


# Modelo aditivo:
#
# MAE_log =
#     intercept
#     + f1(PC1)
#     + f2(PC2)
#     + ...
#
# bs = "tp" = thin plate regression spline
#
# k controla la complejidad máxima de cada spline.

gam_formula <- as.formula(
  paste(
    "MAE_log ~",
    paste(
      paste0("s(", pc_names, ", bs = 'tp', k = 20)"),
      collapse = " + "
    )
  )
)


cat("\nFórmula GAM:\n")
print(gam_formula)



# 5. Entrenar GAM


set.seed(123)

model <- gam(
  formula = gam_formula,
  data = train,
  method = "REML"
)



# 6. Resumen del modelo


summary(model)



# 7. Predicciones


pred_train <- predict(
  model,
  newdata = train
)

pred_test <- predict(
  model,
  newdata = test
)



# 8. Métricas


R2_train <- 1 -
  sum((y_train - pred_train)^2) /
  sum((y_train - mean(y_train))^2)

R2_test <- 1 -
  sum((y_test - pred_test)^2) /
  sum((y_test - mean(y_test))^2)

RMSE_train <- sqrt(
  mean((y_train - pred_train)^2)
)

RMSE_test <- sqrt(
  mean((y_test - pred_test)^2)
)

MAE_train <- mean(
  abs(y_train - pred_train)
)

MAE_test <- mean(
  abs(y_test - pred_test)
)


cat("\n==============================\n")
cat("RESULTADOS GAM\n")
cat("==============================\n")

cat(
  "R² train :",
  round(R2_train, 4),
  "\n"
)

cat(
  "R² test  :",
  round(R2_test, 4),
  "\n"
)

cat(
  "RMSE train:",
  round(RMSE_train, 4),
  "\n"
)

cat(
  "RMSE test :",
  round(RMSE_test, 4),
  "\n"
)

cat(
  "MAE train :",
  round(MAE_train, 4),
  "\n"
)

cat(
  "MAE test  :",
  round(MAE_test, 4),
  "\n"
)



# 9. Evaluar significancia / importancia de las PCs


gam_summary <- summary(model)


# Tabla de significancia de los smooths
smooth_table <- as.data.frame(
  gam_summary$s.table
)

smooth_table$Feature <- rownames(
  gam_summary$s.table
)

rownames(smooth_table) <- NULL


smooth_table <- smooth_table %>%
  select(
    Feature,
    everything()
  )


print(smooth_table)



# 10. Guardar modelo


saveRDS(
  model,
  "F_model_GAM.rds"
)



# 11. Metadata


model_metadata <- list(
  
  # Variables de entrada
  features = pc_names,
  
  # Variable objetivo
  target = "MAE_log",
  
  # Fórmula
  formula = formula(model),
  
  # Método de estimación
  method = "REML",
  
  # Resultados test
  R2_train = R2_train,
  R2_test = R2_test,
  
  RMSE_train = RMSE_train,
  RMSE_test = RMSE_test,
  
  MAE_train = MAE_train,
  MAE_test = MAE_test,
  
  # Seed
  seed = 123,
  
  # Fecha
  date = Sys.time(),
  
  # Versión de mgcv
  mgcv_version =
    as.character(packageVersion("mgcv"))
)


saveRDS(
  model_metadata,
  "F_model_GAM_metadata.rds"
)




