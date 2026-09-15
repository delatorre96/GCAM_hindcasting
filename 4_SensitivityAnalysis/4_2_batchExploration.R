library(xgboost)
library(dplyr)
library(tidyr)


df_best_components <- read.csv('df_best_components.csv')

F_model <- readRDS("F_model_GAM.rds")
#xgb.load("F_model_xgboost.json")

inputsPerParam <- readRDS("gradient_boosting_models.rds")



# 1. Modelos de componentes


pc_models <- list(
  
  PC2 = inputsPerParam$logit$model$PC2,
  PC1 = inputsPerParam$logit$model$PC1,
  
  PC7 = inputsPerParam$satiation_level$model$PC7,
  PC6 = inputsPerParam$satiation_level$model$PC6,
  PC5 = inputsPerParam$satiation_level$model$PC5
  
)



# 2. Obtener todos los inputs utilizados por los modelos


inputs_by_pc <- lapply(
  pc_models,
  function(x) x$feature_names
)

all_inputs <- unique(
  unlist(inputs_by_pc)
)

cat(
  "Número total de inputs utilizados:",
  length(all_inputs),
  "\n"
)


# ============================================================
# 3. Definir los priors / rangos iniciales
# ============================================================

# Los inputs pertenecen a dos tipos de parámetros:
#
#   logit            -> [-510, 0]
#   satiation_level  -> [0, 20]
#
# Estos límites definen el espacio inicial sobre el que
# generaremos la población sintética.


input_bounds <- data.frame(
  
  input = all_inputs,
  
  min = NA_real_,
  max = NA_real_,
  
  parameter_type = NA_character_
  
)


# ============================================================
# 3.1 Identificar el tipo de cada input
# ============================================================

for (i in seq_len(nrow(input_bounds))) {
  
  input <- input_bounds$input[i]
  
  
  # ----------------------------------------------------------
  # Comprobar si el input pertenece al modelo de logits
  # ----------------------------------------------------------
  
  if (input %in% inputs_by_pc$PC2 ||
      input %in% inputs_by_pc$PC1) {
    
    input_bounds$min[i] <- -1000
    input_bounds$max[i] <- 0
    
    input_bounds$parameter_type[i] <- "logit"
    
  }
  
  
  # ----------------------------------------------------------
  # Comprobar si el input pertenece al modelo de
  # satiation_level
  # ----------------------------------------------------------
  
  else if (input %in% inputs_by_pc$PC7 ||
           input %in% inputs_by_pc$PC6 ||
           input %in% inputs_by_pc$PC5) {
    
    input_bounds$min[i] <- 0
    input_bounds$max[i] <- 40
    
    input_bounds$parameter_type[i] <- "satiation_level"
    
  }
  
}


# ============================================================
# 3.2 Comprobar que todos los inputs han sido clasificados
# ============================================================

unknown_inputs <- input_bounds$input[
  is.na(input_bounds$parameter_type)
]

if (length(unknown_inputs) > 0) {
  
  stop(
    paste(
      "No se ha podido determinar el tipo de estos inputs:",
      paste(unknown_inputs, collapse = ", ")
    )
  )
  
}




# 4. Generar población sintética inicial


generate_initial_population <- function(n, bounds) {
  
  population <- as.data.frame(
    lapply(
      seq_len(nrow(bounds)),
      function(i) {
        
        runif(
          n,
          min = bounds$min[i],
          max = bounds$max[i]
        )
        
      }
    )
  )
  
  names(population) <- bounds$input
  
  population
}



# 5. Inputs -> PCs


predict_components <- function(new_inputs, pc_models) {
  
  predictions <- list()
  
  for (pc_name in names(pc_models)) {
    
    info <- pc_models[[pc_name]]
    
    feature_names <- info$feature_names
    model <- info$model
    
    # --------------------------------------------------------
    # Comprobar que todos los inputs necesarios están presentes
    # --------------------------------------------------------
    
    missing_inputs <- setdiff(
      feature_names,
      names(new_inputs)
    )
    
    if (length(missing_inputs) > 0) {
      
      stop(
        paste(
          "El modelo de",
          pc_name,
          "necesita estos inputs que no están presentes:",
          paste(missing_inputs, collapse = ", ")
        )
      )
      
    }
    
    
    # --------------------------------------------------------
    # Seleccionar exactamente los inputs utilizados
    # por este modelo
    # --------------------------------------------------------
    
    df_features <- new_inputs[
      ,
      feature_names,
      drop = FALSE
    ]
    
    
    # --------------------------------------------------------
    # Crear DMatrix
    # --------------------------------------------------------
    
    dmat <- xgb.DMatrix(
      data = as.matrix(df_features)
    )
    
    
    # --------------------------------------------------------
    # Predicción del componente
    # --------------------------------------------------------
    
    predictions[[pc_name]] <- predict(
      model,
      dmat
    )
    
  }
  
  
  as.data.frame(predictions)
  
}



# 6. PCs -> MAE

# Gradient Boosting predict_mae
# predict_mae <- function(pc_predictions, F_model) {
#   
#   # ----------------------------------------------------------
#   # Variables esperadas por F
#   # ----------------------------------------------------------
#   
#   F_features <- F_model$feature_names
#   
#   # Si feature_names no está disponible, usar los 5 PCs
#   if (is.null(F_features)) {
#     
#     F_features <- c(
#       "PC2",
#       "PC7",
#       "PC6",
#       "PC1",
#       "PC5"
#     )
#     
#   }
#   
#   
#   # ----------------------------------------------------------
#   # Comprobar que existen
#   # ----------------------------------------------------------
#   
#   missing_pc <- setdiff(
#     F_features,
#     names(pc_predictions)
#   )
#   
#   if (length(missing_pc) > 0) {
#     
#     stop(
#       paste(
#         "F_model necesita estos componentes:",
#         paste(missing_pc, collapse = ", ")
#       )
#     )
#     
#   }
#   
#   
#   # ----------------------------------------------------------
#   # Extraer únicamente las variables necesarias
#   # ----------------------------------------------------------
#   
#   df_F <- pc_predictions[
#     ,
#     F_features,
#     drop = FALSE
#   ]
#   
#   
#   # ----------------------------------------------------------
#   # Convertir explícitamente a matriz numérica
#   # ----------------------------------------------------------
#   
#   mat_F <- matrix(
#     as.numeric(
#       unlist(
#         df_F,
#         use.names = FALSE
#       )
#     ),
#     nrow = nrow(df_F),
#     ncol = ncol(df_F)
#   )
#   
#   
#   # ----------------------------------------------------------
#   # Crear DMatrix
#   # ----------------------------------------------------------
#   
#   dmat_F <- xgb.DMatrix(
#     data = mat_F
#   )
#   
#   
#   # ----------------------------------------------------------
#   # Predicción
#   # ----------------------------------------------------------
#   
#   predict(
#     F_model,
#     dmat_F
#   )
#   
# }

# GAM predict_mae

predict_mae <- function(pc_predictions, F_model) {
  
  # ----------------------------------------------------------
  # Variables esperadas por F
  # ----------------------------------------------------------
  
  F_features <- c(
    "PC2",
    "PC7",
    "PC6",
    "PC1",
    "PC5"
  )
  
  # ----------------------------------------------------------
  # Comprobar que existen
  # ----------------------------------------------------------
  
  missing_pc <- setdiff(
    F_features,
    names(pc_predictions)
  )
  
  if (length(missing_pc) > 0) {
    stop(
      paste(
        "F_model necesita estos componentes:",
        paste(missing_pc, collapse = ", ")
      )
    )
  }
  
  # ----------------------------------------------------------
  # Extraer únicamente las variables necesarias
  # ----------------------------------------------------------
  
  df_F <- pc_predictions[
    ,
    F_features,
    drop = FALSE
  ]
  
  # ----------------------------------------------------------
  # Asegurar que son numéricas
  # ----------------------------------------------------------
  
  df_F[] <- lapply(df_F, as.numeric)
  
  # ----------------------------------------------------------
  # Predicción del GAM
  # ----------------------------------------------------------
  
  predict(
    F_model,
    newdata = df_F,
    type = "response"
  )
}



# ============================================================
# 7. Búsqueda de 10 millones de combinaciones por batches
# ============================================================

set.seed(123)

n_samples <- 10000000

# Tamaño de cada batch
batch_size <- 50000

# Número de candidatos que queremos conservar
# al terminar todo el proceso
top_n <- 10000


# Número de batches
n_batches <- ceiling(n_samples / batch_size)


cat(
  "\nNúmero total de muestras:",
  format(n_samples, big.mark = ","),
  "\n"
)

cat(
  "Tamaño del batch:",
  format(batch_size, big.mark = ","),
  "\n"
)

cat(
  "Número de batches:",
  n_batches,
  "\n\n"
)


# ============================================================
# 8. Contenedor para los mejores resultados
# ============================================================

best_results <- NULL


# ============================================================
# 9. Procesar batches
# ============================================================

for (b in seq_len(n_batches)) {
  
  cat(
    "\n--------------------------------------------\n"
  )
  
  cat(
    "Procesando batch",
    b,
    "de",
    n_batches,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Número de filas de este batch
  # ----------------------------------------------------------
  
  start <- (b - 1) * batch_size + 1
  end   <- min(b * batch_size, n_samples)
  
  n_batch <- end - start + 1
  
  
  cat(
    "Combinaciones:",
    format(start, big.mark = ","),
    "-",
    format(end, big.mark = ","),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Generar únicamente este batch
  # ----------------------------------------------------------
  
  population_batch <- generate_initial_population(
    n = n_batch,
    bounds = input_bounds
  )
  
  
  # ----------------------------------------------------------
  # Inputs -> PCs
  # ----------------------------------------------------------
  
  pc_predictions_batch <- predict_components(
    new_inputs = population_batch,
    pc_models = pc_models
  )
  
  
  # ----------------------------------------------------------
  # PCs -> MAE
  # ----------------------------------------------------------
  
  mae_batch <- predict_mae(
    pc_predictions = pc_predictions_batch,
    F_model = F_model
  )
  
  
  # ----------------------------------------------------------
  # Crear resultados del batch
  # ----------------------------------------------------------
  
  results_batch <- bind_cols(
    population_batch,
    pc_predictions_batch
  ) %>%
    mutate(
      MAE = mae_batch
    )
  
  
  # ----------------------------------------------------------
  # Conservar únicamente los mejores candidatos
  # ----------------------------------------------------------
  
  best_results <- bind_rows(
    best_results,
    results_batch
  ) %>%
    arrange(MAE) %>%
    slice_head(n = top_n)
  
  
  # ----------------------------------------------------------
  # Mostrar progreso
  # ----------------------------------------------------------
  
  cat(
    "MAE mínimo del batch:",
    min(mae_batch, na.rm = TRUE),
    "\n"
  )
  
  cat(
    "MAE mínimo acumulado:",
    min(best_results$MAE, na.rm = TRUE),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Liberar memoria
  # ----------------------------------------------------------
  
  rm(
    population_batch,
    pc_predictions_batch,
    mae_batch,
    results_batch
  )
  
  gc()
}


# ============================================================
# 10. Resultado final
# ============================================================

results <- best_results


cat(
  "\n============================================\n"
)

cat(
  "BÚSQUEDA FINALIZADA\n"
)

cat(
  "Número de candidatos conservados:",
  nrow(results),
  "\n"
)

cat(
  "MAE mínimo:",
  min(results$MAE, na.rm = TRUE),
  "\n"
)

cat(
  "MAE mediano de los Top-N:",
  median(results$MAE, na.rm = TRUE),
  "\n"
)


# ============================================================
# 11. Inspección
# ============================================================

plot(
  density(results$MAE),
  main = "Distribución del MAE de los mejores candidatos",
  xlab = "MAE"
)


head(results)

