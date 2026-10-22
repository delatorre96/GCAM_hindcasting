library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ranger)


regions_eur <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czech Republic",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Netherlands",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Albania",
  "Bosnia and Herzegovina",
  "Iceland",
  "Macedonia",
  "Moldova",
  "Norway",
  "Serbia and Montenegro",
  "Turkey",
  "UK",
  "Ukraine",
  "Appenine_Peninsula",
  "British_Isles",
  "Central_Eastern_Europe",
  "Central_Western_Europe",
  "Iberian_Peninsula",
  "Northern_Europe",
  "South_Eastern_Europe",
  "Ukraine_Moldova"
)


all_errors <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% filter (region %in% regions_eur) %>%
  select ("region","sector","subsector",  "technology", "output",  "abs_error")

all_errors_non_zero <- all_errors %>% filter(abs_error > 0.001)

cat(nrow(all_errors_non_zero), ' -> ', nrow(all_errors))

X <- model.matrix(
  ~ region + sector + subsector + technology + output - 1,
  data = all_errors_non_zero
)

#----- combinar con abs_error -----

df_model <- data.frame(
  abs_error = all_errors_non_zero$abs_error,
  X,
  check.names = FALSE
)


########### CORRELACIONES ###########

correlations <- cor(
  df_model[, -1],
  df_model$abs_error,
  use = "complete.obs"
)


df_correlations <- data.frame(
  characteristic = colnames(X),
  correlation = as.numeric(correlations),
  abs_correlation = abs(as.numeric(correlations))
)


df_correlations$n_ones <- colSums(X == 1)

#----- ordenar por correlación absoluta -----

df_correlations <- df_correlations[
  order(-df_correlations$abs_correlation),
] %>% filter(abs_correlation >0.01)

df_model <- df_model %>% select(all_of(df_correlations$characteristic),abs_error)

cat(nrow(df_correlations))





############ TREE #############

#----- preparar datos -----

library(rpart)
library(rpart.plot)
library(dplyr)

df_tree <- df_model %>%
  mutate(log_abs_error = log(abs_error)) %>%
  select(-abs_error)

#----- construir arbol grande con todos los datos -----

tree_full <- rpart(
  log_abs_error ~ .,
  data = df_tree,
  method = "anova",
  control = rpart.control(
    cp = 0.0005,
    minsplit = 50,
    minbucket = 20,
    maxdepth = 10,
    xval = 10
  )
)

#----- inspeccionar estructura -----

print(tree_full)
printcp(tree_full)

cat("Nodos terminales:", sum(tree_full$frame$var == "<leaf>"), "\n")


#----- visualizar arbol completo -----

dev.new(width = 16, height = 10)

rpart.plot(
  tree_full,
  type = 3,
  extra = 101,
  fallen.leaves = TRUE,
  tweak = 0.7,
  cex = 1,
  faclen = 2,
  varlen = 12,
  main = "Árbol completo de la magnitud del error"
)





########### GRADIENT BOOSTING ###########

#
#
# #----- paquetes -----
#
# library(xgboost)
# library(dplyr)
#
# #----- preparar datos -----
#
# set.seed(123)
#
# y <- log(df_model$abs_error)
#
# X <- as.matrix(
#   df_model[, setdiff(names(df_model), "abs_error"), drop = FALSE]
# )
#
# storage.mode(X) <- "double"
#
# #----- division entrenamiento / prueba -----
#
# n <- nrow(X)
# id_train <- sample(seq_len(n), size = floor(0.8 * n))
#
# X_train <- X[id_train, , drop = FALSE]
# X_test  <- X[-id_train, , drop = FALSE]
#
# y_train <- y[id_train]
# y_test  <- y[-id_train]
#
# dtrain <- xgb.DMatrix(data = X_train, label = y_train)
# dtest  <- xgb.DMatrix(data = X_test, label = y_test)
#
# #----- parametros del gradient boosting -----
#
# params <- list(
#   objective = "reg:squarederror",
#   eval_metric = "rmse",
#   eta = 0.03,
#   max_depth = 4,
#   min_child_weight = 5,
#   subsample = 0.8,
#   colsample_bytree = 0.8,
#   lambda = 5
# )
#
# #----- entrenar con parada temprana -----
#
# model_gbm <- xgb.train(
#   params = params,
#   data = dtrain,
#   nrounds = 1000,
#   evals = list(
#     train = dtrain,
#     test = dtest
#   ),
#   early_stopping_rounds = 30,
#   verbose = 1
# )
#
# #----- evaluar en datos no usados para entrenar -----
# best_iteration <- as.integer(
#   xgb.attributes(model_gbm)[["best_iteration"]]
# )
#
# pred <- predict(
#   model_gbm,
#   dtest,
#   iterationrange = c(1, best_iteration)
# )
#
# rmse_log <- sqrt(mean((y_test - pred)^2))
# mae_log  <- mean(abs(y_test - pred))
#
# R2 <- 1 - sum((y_test - pred)^2) /
#   sum((y_test - mean(y_test))^2)
#
#
# cat("R2:", R2, "\n")
# cat("Mejor iteracion:", model_gbm$best_iteration, "\n")
# cat("RMSE log:", rmse_log, "\n")
# cat("MAE log:", mae_log, "\n")
#
#
#
