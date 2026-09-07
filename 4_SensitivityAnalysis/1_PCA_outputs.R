library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ranger)
source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")



con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis/gcam_sensitivity.sqlite"
)

#See interested experiments:
experiments <- dbReadTable(con, "Experiments")
experiment_id <- 'EXP_2e96286f'
query_name <- "outputs_by_tech"

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

## id per output



output_keys <- all_errors_output_by_tech %>%
  select(
    "region","sector","subsector","output","technology" 
  ) %>%
  distinct() %>%
  mutate(output_id = row_number())


outputs <- all_errors_output_by_tech %>%
  left_join(
    output_keys,
    by = c(
      "region","sector","subsector","output","technology" 
    )
  ) %>%
  select(output_id, run_id, error) 

outputs_wide <- outputs %>%
  select(run_id, output_id, error) %>%
  pivot_wider(
    id_cols = run_id,
    names_from = output_id,
    values_from = error,
    names_prefix = "output_"
)


###### Filtrar outputs por varianza - contribución al error######
# Cada observación es el error vinculado a un output y a un run específicos. Esto quiere decir no hace falta controlar la varianza por la magnitud de la observación, 
# puesto que inherentemente aquellos que tienen varianza alta, aunque venga dada por una magnitud elevada, su contribución al error es alta y por tanto también de interés


## Cuanto contribuye en promedio cada output al error

# 1. CONTRIBUCIÓN AL ERROR


error_per_run <- all_errors_output_by_tech %>% 
  group_by(run_id) %>%
  summarise(
    SAE = sum(error_abs),
    .groups = "drop"
  )

contribution_output2error <- error_per_run %>%
  left_join(outputs_wide, by = "run_id") %>%
  mutate(
    across(
      starts_with("output"),
      ~ abs(.x) / SAE
    )
  )

output_cols <- grep("^output", names(contribution_output2error))

X <- as.matrix(contribution_output2error[, output_cols])

contribution_stats <- data.frame(
  output = names(contribution_output2error)[output_cols],
  mean = colMeans(X, na.rm = TRUE),
  sd = apply(X, 2, sd, na.rm = TRUE)
) %>%
  arrange(desc(mean)) %>%
  mutate(cumsum = cumsum(mean))

# Outputs que explican hasta el 99.9% del error
n_99 <- which(contribution_stats$cumsum >= 0.99)[1]

outputs_contribution <- contribution_stats$output[1:n_99]



# 2. VARIANZA


output_cols <- grep("^output", names(outputs_wide))

vars <- sapply(outputs_wide[, output_cols], function(x) {
  var(x, na.rm = TRUE)
})

outputs_variance <- names(vars)[
  !is.na(vars) &
    vars != 0  &
    is.finite(vars)
]


# 3. COMBINAR LOS DOS CRITERIOS

# Mantener si cumple AL MENOS UNO de los dos criterios
outputs_keep <- intersect(
  outputs_contribution,
  outputs_variance
)

# Añadir run_id
cols_keep <- c(outputs_keep, "run_id")

#### Filtrar outputs más significativos para el error ####
outputs_wide_filtered <- outputs_wide %>%
  select(all_of(cols_keep)) 

output_cols <- grep("^output", names(outputs_wide_filtered))

cat(ncol(outputs_wide), ' vars -> ', ncol(outputs_wide_filtered),' vars \n', (ncol(outputs_wide) - ncol(outputs_wide_filtered))/ncol(outputs_wide)*100,'% less')

####  Eliminar Na o NaN que sean muy numerosos y no se hayan podiod eliminar por varianza baja o contribución al error
# 
output_cols <- grep("^output_", names(outputs_wide_filtered))

# Matriz de outputs
X <- as.matrix(outputs_wide_filtered[, output_cols])

# Número de valores no finitos por fila y columna
n_bad_rows <- rowSums(!is.finite(X))
n_bad_cols <- colSums(!is.finite(X))

# Umbral automático: percentil 99%
threshold_rows <- quantile(n_bad_rows, 0.99)
threshold_cols <- quantile(n_bad_cols, 0.99)

# Filas y columnas a eliminar
rows_remove <- which(n_bad_rows > threshold_rows)
cols_remove <- which(n_bad_cols > threshold_cols)

# Eliminar filas
outputs_wide_filtered <- outputs_wide_filtered[-rows_remove, ]

# Eliminar columnas
outputs_wide_filtered <- outputs_wide_filtered[
  ,
  -output_cols[cols_remove]
]



###### PCA #####
X <- outputs_wide_filtered %>%
  select(-run_id)

cols_na <- names(X)[
  sapply(X, function(x) any(is.na(x)))
]

X_clean <- X %>%
  select(-all_of(cols_na)) ##eliminamos outputs que tengan NA

pca <- prcomp(
  X_clean,
  center = TRUE,
  scale. = TRUE
)

var_explained <- pca$sdev^2 / sum(pca$sdev^2)

# Varianza explicada acumulada
cum_var <- cumsum(var_explained)

# Número de componentes necesarios para llegar al 80%
n_pc <- which(cum_var >= 0.99)[1]

pca_scores <- as.data.frame(pca$x[, 1:n_pc])

df_PCA <- cbind(run_id = outputs_wide_filtered$run_id, pca_scores)

write.csv(df_PCA, 'outputs_PCA.csv', row.names = FALSE)




############################## DRAFT ##############################


###### Var clustering ######

library(ggplot2)
library(cluster)

# 1. Preparar los datos

# Eliminamos run_id porque es una variable identificativa
X <- outputs_wide_filtered %>%
  select(-run_id) %>%
  as.matrix()

# Cada fila = un run
# Cada columna = un output/variable

# Estandarizamos cada variable
X_scaled <- scale(X)

# Trasponemos para hacer clustering de VARIABLES
# Cada fila = una variable output_i
# Cada columna = un run
X_variables <- t(X_scaled)

### Eliminar variables problemáticas 

# Identificar variables que contienen algún NA/NaN/Inf
bad_variables <- rownames(X_variables)[
  !apply(is.finite(X_variables), 1, all)
]

# Eliminar las variables problemáticas
X_variables <- X_variables[
  apply(is.finite(X_variables), 1, all),
  ,
  drop = FALSE
]

cor_pearson <- cor(
  X_variables,
  method = "pearson",
  use = "pairwise.complete.obs"
)



# 5. Convertir correlación en distancia
#    d = 1 - rho
dist_pearson <- as.dist(1 - abs(cor_pearson))

# 6. Clustering jerárquico aglomerativo
hc <- hclust(
  dist_pearson,
  method = "complete"
)

# 7. Dendrograma
plot(
  hc,
  main = "Clustering jerárquico de variables",
  xlab = "Variables",
  ylab = "Distancia (1 - pearson)",
  sub = "",
  cex = 0.5
)





### Clustering kmeans
mds <- cmdscale(
  dist_pearson,
  k = 10,
  eig = TRUE
)

X_mds <- mds$points
## Elboww method
k_values <- 2:20

wss <- sapply(k_values, function(k) {
  kmeans(
    X_mds,
    centers = k,
    nstart = 10
  )$tot.withinss
})

elbow_df <- data.frame(
  k = k_values,
  WSS = wss
)

ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Método del codo",
    x = "Número de clusters (k)",
    y = "Within-cluster sum of squares"
  ) +
  theme_minimal()

#silhouette
sil_width <- sapply(k_values, function(k) {
  
  km <- kmeans(
    X_mds,
    centers = k,
    nstart = 10
  )
  
  sil <- silhouette(
    km$cluster,
    dist_pearson
  )
  
  mean(sil[, "sil_width"])
})

sil_df <- data.frame(
  k = k_values,
  silhouette = sil_width
)

ggplot(sil_df, aes(x = k, y = silhouette)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Ancho medio de silueta",
    x = "Número de clusters (k)",
    y = "Silhouette"
  ) +
  theme_minimal()






