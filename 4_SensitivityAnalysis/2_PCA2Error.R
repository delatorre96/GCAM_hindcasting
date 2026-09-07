library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

################### DATA ########################

con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis/gcam_sensitivity.sqlite"
)

#See interested experiments:
experiments <- dbReadTable(con, "Experiments")
experiment_id <- 'EXP_2e96286f'
query_name <- "outputs_by_tech"

###Extract outputs ####
outputs_by_tech <- read_experiment_output(
  con = con,
  query_name = query_name,
  execution_errors = NULL
  #experiment_id
)


####### Reference #######
ref_values <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% 
  select(-query, -year, -rel_error, -value_chY, -error, -abs_error)

###errors###
all_errors_output_by_tech <- outputs_by_tech %>% 
  left_join(ref_values, by = c('region', 'technology', 'subsector', 'output', 'sector'))   %>% 
  filter(!is.na(value_ref) & !is.na(`2021`)) %>%
  mutate(error_abs = abs(value_ref -`2021`),
         error = value_ref -`2021`)%>%
  select(-`2021`, -value_ref) 


#errors per run#
error_per_run <- all_errors_output_by_tech %>% 
  group_by(run_id) %>%
  summarise(MAE = mean(error_abs),
            RMSE = sqrt(mean(error^2))) %>%
  mutate(MAE_log = log1p(MAE),
         RMSE_log = log1p(RMSE))

log1MAE <- error_per_run %>% select(run_id, MAE_log)

######## outputs PCA #########
df_PCA <- read.csv('outputs_PCA.csv')

df_total <- log1MAE %>% left_join(df_PCA, by = 'run_id') %>% drop_na()


########  Gradient Boosting ######## 
#Explore importances
library(xgboost)
library(dplyr)
library(ggplot2)


df_model <- df_total %>%
  select(MAE_log, starts_with("PC")) %>%
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
X_train <- as.matrix(train %>% select(starts_with("PC")))
X_test  <- as.matrix(test %>% select(starts_with("PC")))

y_train <- train$MAE_log
y_test  <- test$MAE_log



# HIPERPARÁMETROS

nrounds <- 500
max_depth <- 4
eta <- 0.03
min_child_weight <- 1
subsample <- 0.8
colsample_bytree <- 0.8
lambda <- 1
alpha <- 0



# Modelo
set.seed(123)

model <- xgboost(
  data = X_train,
  label = y_train,
  
  nrounds = nrounds,
  max_depth = max_depth,
  eta = eta,
  min_child_weight = min_child_weight,
  subsample = subsample,
  colsample_bytree = colsample_bytree,
  
  lambda = lambda,
  alpha = alpha,
  
  objective = "reg:squarederror",
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

importance_95 <- importance %>%
  mutate(
    Gain_cum = cumsum(Gain),
    Gain_pct = 100 * Gain_cum
  ) %>%
  filter(
    Gain_cum <= 0.95 |
      lag(Gain_cum, default = 0) < 0.95
  )

pcs_95 <- importance_95$Feature

X_train_95 <- as.matrix(
  train[, pcs_95]
)

X_test_95 <- as.matrix(
  test[, pcs_95]
)


set.seed(123)

model_95 <- xgboost(
  data = X_train_95,
  label = y_train,
  
  nrounds = nrounds,
  max_depth = max_depth,
  eta = eta,
  min_child_weight = min_child_weight,
  subsample = subsample,
  colsample_bytree = colsample_bytree,
  
  lambda = lambda,
  alpha = alpha,
  
  objective = "reg:squarederror",
  verbose = 0
)



# 4. Predicciones
pred_train_95 <- predict(
  model_95,
  X_train_95
)

pred_test_95 <- predict(
  model_95,
  X_test_95
)



# 5. Métricas
R2_train_95 <- 1 -
  sum((y_train - pred_train_95)^2) /
  sum((y_train - mean(y_train))^2)

R2_test_95 <- 1 -
  sum((y_test - pred_test_95)^2) /
  sum((y_test - mean(y_test))^2)

RMSE_test_95 <- sqrt(
  mean((y_test - pred_test_95)^2)
)

MAE_test_95 <- mean(
  abs(y_test - pred_test_95)
)


cat("\n")
cat("=================================\n")
cat("MODELO CON PCs = 95% GAIN\n")
cat("=================================\n")
cat("Número de PCs :", length(pcs_95), "\n")
cat("R² train      :", round(R2_train_95, 4), "\n")
cat("R² test       :", round(R2_test_95, 4), "\n")
cat("RMSE test     :", round(RMSE_test_95, 4), "\n")
cat("MAE test      :", round(MAE_test_95, 4), "\n")


########  Interpretar Gradient Boosting ######## 
library(SHAPforxgboost)
library(pdp)






























############# Bayesian network
library(bnlearn)
library(igraph)

cols <- c("MAE_log", pcs_95)

df_network <- df_total %>%
  select(all_of(cols)) %>%
  na.omit()

# Estandarizar
df_network_scaled <- as.data.frame(scale(df_network))
set.seed(123)

boot_net <- boot.strength(
  data = df_network_scaled,
  R = 500,
  algorithm = "hc",
  algorithm.args = list(score = "bic-g")
)

boot_net %>%
  arrange(desc(strength)) %>%
  head(20)

bn_avg <- averaged.network(
  boot_net,
  threshold = 0.70
)

plot(
  bn_avg,
  main = "Consensus Bayesian network"
)



edges <- boot_net %>%
  filter(
    strength >= 0.70,
    direction >= 0.50
  )

g <- graph_from_data_frame(
  edges[, c("from", "to", "strength")],
  directed = TRUE
)
plot(
  g,
  vertex.size = 30,
  vertex.label.cex = 1,
  vertex.frame.color = "black",
  
  edge.width = 1 + 2 * E(g)$strength,
  edge.arrow.size = 0.8,
  edge.arrow.width = 1.5,
  edge.curved = 0,
  
  edge.label = sprintf("%.2f", E(g)$strength),
  edge.label.cex = 0.8,
  
  layout = layout_with_kk(g)
)











########  regresion lineal ######## 

##split
set.seed(123)

df_reg <- df_total %>% select(-run_id)

n <- nrow(df_reg)

idx_train <- sample(seq_len(n), size = 0.8 * n)

train <- df_reg[idx_train, ]
test  <- df_reg[-idx_train, ]

model <- lm(
  MAE_log ~ .,
  data = train
)

summary(model)

pred <- predict(model, newdata = test)


resultados <- data.frame(
  real = test$MAE_log,
  pred = pred
)

head(resultados)



### Metricas
y_real <- test$MAE_log
y_pred <- pred

R2 <- 1 - sum((y_real - y_pred)^2) /
  sum((y_real - mean(y_real))^2)

RMSE <- sqrt(mean((y_real - y_pred)^2))

MAE <- mean(abs(y_real - y_pred))

MAPE <- mean(abs((y_real - y_pred) / y_real)) * 100

c(
  R2 = R2,
  RMSE = RMSE,
  MAE = MAE,
  MAPE = MAPE
)
plot(resultados$real, resultados$pred)

########  regresion tree######## 
library(rpart)
library(rpart.plot)

# 1. Train / test split


set.seed(123)

n <- nrow(df_reg)

id_train <- sample(
  seq_len(n),
  size = floor(0.80 * n)
)

train <- df_reg[id_train, ]
test  <- df_reg[-id_train, ]


# Comprobar tamaños
dim(train)
dim(test)



# 2. Ajustar el árbol SOLO con train


tree_mae <- rpart(
  MAE_log ~ .,
  data = train,
  method = "anova"
)



# 3. Seleccionar el tamaño óptimo mediante CV
#    (la CV se hace únicamente dentro de train)


printcp(tree_mae)

plotcp(tree_mae)


# CP que minimiza el error de validación cruzada
best_cp <- tree_mae$cptable[
  which.min(tree_mae$cptable[, "xerror"]),
  "CP"
]

best_cp



# 4. Podar el árbol


tree_pruned <- prune(
  tree_mae,
  cp = best_cp
)



# 5. Visualizar el árbol final


rpart.plot(
  tree_pruned,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  main = "Árbol de regresión para MAE_log"
)



# 6. Predicciones sobre TRAIN y TEST


pred_train <- predict(
  tree_pruned,
  newdata = train
)

pred_test <- predict(
  tree_pruned,
  newdata = test
)



# 7. Evaluación


# RMSE
rmse_train <- sqrt(
  mean((train$MAE_log - pred_train)^2)
)

rmse_test <- sqrt(
  mean((test$MAE_log - pred_test)^2)
)


# MAE
mae_train <- mean(
  abs(train$MAE_log - pred_train)
)

mae_test <- mean(
  abs(test$MAE_log - pred_test)
)


# R²
r2_train <- 1 -
  sum((train$MAE_log - pred_train)^2) /
  sum((train$MAE_log - mean(train$MAE_log))^2)

r2_test <- 1 -
  sum((test$MAE_log - pred_test)^2) /
  sum((test$MAE_log - mean(test$MAE_log))^2)


# Resultados
data.frame(
  Dataset = c("Train", "Test"),
  RMSE = c(rmse_train, rmse_test),
  MAE = c(mae_train, mae_test),
  R2 = c(r2_train, r2_test)
)


# 8. Importancia de los PCs


importance <- sort(
  tree_pruned$variable.importance,
  decreasing = TRUE
)

importance


