library(dplyr)
library(tidyr)
library(fastDummies)
library(Matrix)


all_errors <- readRDS("Data/all_errors.rds")

################ Create Query dataFrame ################
all_error_ml_df <- all_errors %>%
  select(-value_ref, -value_chY,-abs_error, -rel_error,   
         -resource,
         -subresource,
         -technology,
         -grade,
         -fuel,
         -subsector,
         -output,
         -sector,
         -input,
         -building,
         -nodeinput,
         -`building-node-input`,
         -`gcam-consumer`,
         -mode,
         -landleaf,
         -ghg,
         -account,
         -`social-accounting-matrix-row`,
         -column,
         -basin,
         -`runoff water`,
         -groundwater,
         -year,
         -region)


all_error_ml_df$query <- make.names(all_error_ml_df$query)

X_query <- model.matrix(~ query - 1, data = all_error_ml_df)
y <- all_error_ml_df$error



################ Queries importance for error ################
library(xgboost)

y <- scale(all_error_ml_df$error)
X_mat <- X_query

dtrain <- xgb.DMatrix(data = X_mat, label = y)

params <- list(
  objective = "reg:squarederror",
  max_depth = 6,
  learning_rate = 0.1
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  verbose = 1
)

xgb.importance(model = model)







X <- Matrix::sparse.model.matrix(
  ~ query,
  data = all_error_ml_df
)

################ Queries importance for error ################
library(xgboost)

y <- scale(all_error_ml_df$error)
X_mat <- X 

dtrain <- xgb.DMatrix(data = X_mat, label = y)

params <- list(
  objective = "reg:squarederror",
  max_depth = 6,
  learning_rate = 0.1
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  verbose = 1
)

xgb.importance(model = model)


