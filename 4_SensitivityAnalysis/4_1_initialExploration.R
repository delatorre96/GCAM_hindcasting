library(xgboost)
library(dplyr)
library(tidyr)


df_best_components <- read.csv('df_best_components.csv')

F_model <- xgb.load("F_model_xgboost.json")

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

predict_mae <- function(pc_predictions, F_model) {
  
  # ----------------------------------------------------------
  # Variables esperadas por F
  # ----------------------------------------------------------
  
  F_features <- F_model$feature_names
  
  # Si feature_names no está disponible, usar los 5 PCs
  if (is.null(F_features)) {
    
    F_features <- c(
      "PC2",
      "PC7",
      "PC6",
      "PC1",
      "PC5"
    )
    
  }
  
  
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
  # Convertir explícitamente a matriz numérica
  # ----------------------------------------------------------
  
  mat_F <- matrix(
    as.numeric(
      unlist(
        df_F,
        use.names = FALSE
      )
    ),
    nrow = nrow(df_F),
    ncol = ncol(df_F)
  )
  
  
  # ----------------------------------------------------------
  # Crear DMatrix
  # ----------------------------------------------------------
  
  dmat_F <- xgb.DMatrix(
    data = mat_F
  )
  
  
  # ----------------------------------------------------------
  # Predicción
  # ----------------------------------------------------------
  
  predict(
    F_model,
    dmat_F
  )
  
}


# 7. Primera población sintética


set.seed(123)

n_samples <- 10000000


population <- generate_initial_population(
  n = n_samples,
  bounds = input_bounds
)


cat(
  "\nPoblación sintética generada:",
  nrow(population),
  "combinaciones\n"
)



# 8. Inputs -> PCs


pc_predictions <- predict_components(
  new_inputs = population,
  pc_models = pc_models
)



# 9. PCs -> MAE


mae_pred <- predict_mae(
  pc_predictions = pc_predictions,
  F_model = F_model
)



# 10. Crear dataframe final


results <- bind_cols(
  population,
  pc_predictions
) %>%
  mutate(
    MAE = mae_pred
  )



# 11. Inspección inicial


cat(
  "MAE mínimo:",
  min(results$MAE, na.rm = TRUE),
  "\n"
)

plot(density(results$MAE))


