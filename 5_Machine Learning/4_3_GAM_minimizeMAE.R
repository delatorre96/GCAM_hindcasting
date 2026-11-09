
#----- librerias -----

library(mgcv)
library(numDeriv)

#----- cargar modelos -----

meta_F <- readRDS("F_model_GAM_metadata.rds")
F_model <- readRDS("F_model_GAM.rds")
GAM_models <- readRDS("GAM_models.rds")

stopifnot(inherits(F_model, "gam"))

#----- seleccionar modelos por componente -----

selected_family <- c(
  PC2 = "price_elasticity",
  PC4 = "satiation_level",
  PC3 = "satiation_level",
  PC7 = "satiation_level",
  PC6 = "satiation_level",
  PC1 = "price_elasticity"
)

selected_models <- lapply(names(selected_family), function(pc) {

  family <- selected_family[[pc]]

  node <- GAM_models[[family]]$model[[pc]]

  if (is.null(node$model) || !inherits(node$model, "gam")) {
    stop("No se encuentra el GAM ajustado para ", pc)
  }

  node$model
})

names(selected_models) <- names(selected_family)

#----- identificar inputs de cada modelo -----

get_predictors <- function(fit) {

  response_vars <- all.vars(formula(fit)[[2]])

  setdiff(
    all.vars(formula(fit)),
    response_vars
  )
}

predictors_by_pc <- lapply(selected_models, get_predictors)

input_names <- unique(unlist(predictors_by_pc))

cat("Número de inputs únicos:", length(input_names), "\n")
print(input_names)

#----- obtener rangos de entrenamiento -----

ranges_by_input <- setNames(
  lapply(input_names, function(x) list()),
  input_names
)

for (pc in names(selected_models)) {

  fit <- selected_models[[pc]]
  predictors <- predictors_by_pc[[pc]]

  if (is.null(fit$model)) {
    stop("El modelo de ", pc, " no conserva los datos de entrenamiento.")
  }

  for (v in predictors) {

    if (!v %in% names(fit$model)) {
      stop("No se encuentra ", v, " en el model frame de ", pc)
    }

    z <- fit$model[[v]]

    if (!is.numeric(z)) {
      stop("El input ", v, " no es numérico.")
    }

    z <- z[is.finite(z)]

    ranges_by_input[[v]][[length(ranges_by_input[[v]]) + 1]] <-
      c(lower = min(z), upper = max(z), initial = median(z))
  }
}

bounds <- lapply(input_names, function(v) {

  rr <- ranges_by_input[[v]]

  lo <- max(vapply(rr, function(z) z["lower"], numeric(1)))
  hi <- min(vapply(rr, function(z) z["upper"], numeric(1)))

  if (lo > hi) {
    stop("No hay rango común de entrenamiento para ", v)
  }

  starts <- vapply(rr, function(z) z["initial"], numeric(1))

  c(
    lower = lo,
    upper = hi,
    initial = min(max(median(starts), lo), hi)
  )
})

names(bounds) <- input_names

lower <- vapply(bounds, function(z) z["lower"], numeric(1))
upper <- vapply(bounds, function(z) z["upper"], numeric(1))
x_initial <- vapply(bounds, function(z) z["initial"], numeric(1))

#----- predecir componentes PCA -----

predict_components <- function(x) {

  names(x) <- input_names

  pcs <- setNames(numeric(length(selected_models)),
                  names(selected_models))

  for (pc in names(selected_models)) {

    fit <- selected_models[[pc]]
    predictors <- predictors_by_pc[[pc]]

    newdata <- as.data.frame(
      as.list(x[predictors]),
      optional = TRUE
    )

    pcs[pc] <- as.numeric(
      predict(fit, newdata = newdata, type = "response")[1]
    )
  }

  pcs
}

#----- predecir MAE mediante el GAM final -----

predict_final_mae <- function(pcs) {

  newdata <- as.data.frame(
    as.list(pcs[meta_F$features]),
    optional = TRUE
  )

  as.numeric(
    predict(F_model, newdata = newdata, type = "response")[1]
  )
}

#----- funcion compuesta J(X) -----

objective <- function(x) {

  names(x) <- input_names

  pcs <- predict_components(x)

  predict_final_mae(pcs)
}

#----- evaluar configuracion inicial -----

initial_pcs <- predict_components(x_initial)
initial_mae <- objective(x_initial)

cat("\nMAE inicial predicho:", initial_mae, "\n")
print(initial_pcs)

#----- derivadas numericas en el punto inicial -----

gradient_initial <- numDeriv::grad(
  func = objective,
  x = x_initial
)

gradient_initial_table <- data.frame(
  input_id = input_names,
  value = x_initial,
  derivative = gradient_initial
)

gradient_initial_table <- gradient_initial_table[
  order(abs(gradient_initial_table$derivative),
        decreasing = TRUE),
]

print(gradient_initial_table)

#----- optimizacion multistart -----

set.seed(123)

n_starts <- 30

starts <- c(
  list(x_initial),
  replicate(
    n_starts - 1,
    setNames(
      runif(length(input_names), lower, upper),
      input_names
    ),
    simplify = FALSE
  )
)

optim_results <- lapply(starts, function(x0) {

  optim(
    par = x0,
    fn = objective,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = list(maxit = 2000)
  )
})

#----- elegir el mejor resultado convergente -----

valid <- which(vapply(optim_results, function(z) {
  z$convergence == 0 &&
    is.finite(z$value) &&
    all(is.finite(z$par))
}, logical(1)))

if (length(valid) == 0) {
  stop("Ninguna optimización ha convergido correctamente.")
}

best_idx <- valid[
  which.min(vapply(
    optim_results[valid],
    function(z) z$value,
    numeric(1)
  ))
]

best <- optim_results[[best_idx]]

x_best <- setNames(best$par, input_names)
pcs_best <- predict_components(x_best)
mae_best <- objective(x_best)

#----- derivadas en el optimo -----

gradient_best <- numDeriv::grad(
  func = objective,
  x = x_best
)

#----- resultados -----

cat("\nMAE inicial:", initial_mae, "\n")
cat("Mejor MAE predicho:", mae_best, "\n")
cat("Convergencia:", best$convergence, "\n")

cat("\nInputs optimizados:\n")
print(x_best)

cat("\nComponentes PCA optimizados:\n")
print(pcs_best)

cat("\nGradiente en el optimo:\n")
print(data.frame(
  input_id = input_names,
  value = x_best,
  derivative = gradient_best
))

#----- guardar resultados -----

results <- list(
  selected_family = selected_family,
  predictors_by_pc = predictors_by_pc,
  initial_inputs = x_initial,
  initial_components = initial_pcs,
  initial_mae = initial_mae,
  initial_gradient = gradient_initial_table,
  optimized_inputs = x_best,
  optimized_components = pcs_best,
  optimized_mae = mae_best,
  optimized_gradient = gradient_best,
  optimization = best
)

saveRDS(results, "GCAM_GAM_optimization_results.rds")

write.csv(
  data.frame(input_id = names(x_best),
             optimized_value = as.numeric(x_best)),
  "GCAM_GAM_optimized_inputs.csv",
  row.names = FALSE
)

write.csv(
  data.frame(PC = names(pcs_best),
             predicted_value = as.numeric(pcs_best)),
  "GCAM_GAM_optimized_components.csv",
  row.names = FALSE
)

write.csv(
  gradient_initial_table,
  "GCAM_GAM_initial_gradient.csv",
  row.names = FALSE
)



GAM_best_inputs <-   data.frame(input_id = names(x_best),
                                optimized_value = as.numeric(x_best)) %>%
  mutate(input_id = as.integer(sub("input_", "", input_id)))

all_inputs <- bind_rows(
  inputsPerParam$satiation_level$drivers,
inputsPerParam$price_elasticity$drivers) %>% inner_join(GAM_best_inputs, by = 'input_id') %>%
  select(input_id, xml_file, xpath, region, gcam_consumer, optimized_value) %>% distinct() %>% mutate(xml_xpath = paste0(xml_file,'_',xpath))

df_params <- read_csv('create_dfParams/df_allParams.csv') %>%
  mutate(xml_xpath = paste0(xml_file,'_',xpath)) %>% inner_join (all_inputs, by = c('xml_file','xpath','xml_xpath', 'region', 'gcam_consumer')) %>%
  mutate(old_values = param) %>%  mutate(param = round(optimized_value,3))

write_csv(df_params, 'create_dfParams/df_params_optimazed_values.csv')






