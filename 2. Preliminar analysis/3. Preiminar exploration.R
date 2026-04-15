################ ERRORS ##################


vars <- names(all_errors)[names(all_errors) %in% variables]
df_long <- all_errors %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "val",
    values_drop_na = TRUE
  )

df_plot <- df_long %>%
  filter(is.finite(abs_error), abs_error > 0)

df_plot <- df_plot %>%
  mutate(log_abs_error = log10(abs_error))

df_plot_cut <- df_plot %>%
  filter(log_abs_error > -40)

p1 <- ggplot(df_plot, aes(x = log_abs_error)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Complete distribution of the absolute error (log10)",
    x = "log10(Absolute Error)",
    y = "Density"
  ) +
  theme_minimal(base_size = 16)
p2 <- ggplot(df_plot_cut, aes(x = log_abs_error)) +
  geom_density(fill = "tomato", alpha = 0.4) +
  labs(
    title = "Absolute error distribution (truncated tail)",
    x = "log10(Absolute Error)",
    y = "Density"
  ) +
  theme_minimal(base_size = 16)

ggsave("absoluteError1.jpg", plot = p1, width = 10, height = 8, dpi = 300)
ggsave("absoluteError2.jpg", plot = p2, width = 10, height = 8, dpi = 300)



#################### ERROR BIAS ####################
all_errors_bias <- all_errors %>%
  select(query, region, year, value_ref, value_chY, error) %>%
  mutate(error_bias = error) %>%
  #mutate(error_bias = if_else (value_ref == 0 & value_chY == 0, 0 , (value_ref- value_chY) / value_ref)) %>%
  filter(if_all(everything(), ~ !is.infinite(.)))
# %>%
#   filter(
#     error_bias >= quantile(error_bias, 0.25, na.rm = TRUE) - 1.5 * IQR(error_bias, na.rm = TRUE) &
#       error_bias <= quantile(error_bias, 0.75, na.rm = TRUE) + 1.5 * IQR(error_bias, na.rm = TRUE)
#   )

summary_bias <- all_errors_bias %>%
  mutate(
    bias_sign = case_when(
      error < 0  ~ "neg",
      error > 0  ~ "pos",
      error == 0 ~ "zero"
    )
  )

summary_table <- summary_bias %>%
  group_by(bias_sign) %>%
  summarise(
    n = n(),
    perc = n() / nrow(summary_bias) * 100,
    mean = mean(error, na.rm = TRUE),
    sd = sd(error, na.rm = TRUE),
    min = min(error, na.rm = TRUE),
    max = max(error, na.rm = TRUE)
  )

summary_table <- summary_table %>%
  mutate(bias_sign = factor(bias_sign, levels = c("neg", "zero", "pos"))) %>%
  arrange(bias_sign)
library(knitr)

kable(summary_table, format = "latex", booktabs = TRUE, digits = 2)


#################### NEGATIVE CORRELATIONS ####################


correlations_df <- df_long %>%
  group_by(query, region, variable) %>%
  summarise(
    spearman_corr = cor(value_ref, value_chY,
                        method = "spearman",
                        use = "complete.obs"),
    .groups = "drop"
  )

### spurious and negative corr
negCorr <- correlations_df %>%
  filter(!is.na(spearman_corr), spearman_corr <= 0.1)

###### Densities ######

### Total
density_corr <- ggplot(correlations_df,
                       aes(x = spearman_corr)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Distribution of Spearman Correlations",
    subtitle = "Across all time series comparisons",
    x = "Spearman correlation",
    y = "Density"
  )  +
  theme_minimal(base_size = 16)



density_neg_corr <- ggplot(negCorr,
                           aes(x = spearman_corr)) +
  geom_density(fill = "tomato", alpha = 0.4) +
  labs(
    title = "Low or Spurious Correlations",
    subtitle = "Spearman correlation ≤ 0.1",
    x = "Spearman correlation",
    y = "Density"
  )  +
  theme_minimal(base_size = 16)


ggsave("density_corr_all.jpg",
       plot = density_corr,
       width = 8, height = 5, dpi = 300)

ggsave("density_corr_low.jpg",
       plot = density_neg_corr,
       width = 8, height = 5, dpi = 300)



################ REGION ##################
#### Qué region tiende a acumular más error y cómo es su naturaleza?

region_plot_df <- region_metrics_all %>%
  group_by(region) %>% summarise(MAE = sum(MAE),
                                 RMSE = sum(RMSE),
                                 rel_MAE_RMSE = mean(rel_MAE_RMSE),
                                 rel_error = mean(rel_error),
                                 Min_spearman_corr = min(spearman_corr, na.rm = TRUE))  %>%
  arrange(desc(MAE)) 

### Gráficos región

region_plot_df <- region_plot_df %>%
  mutate(rel_MAE_RMSE_cap = rel_MAE_RMSE/max(rel_MAE_RMSE))

bars_relRMSE_MAE_by_region <- ggplot(
  region_plot_df,
  aes(
    x = reorder(region, MAE),
    y = MAE,
    fill = rel_MAE_RMSE
  )
) +
  geom_col(width = 0.75, alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#2b6cb0",
    mid = "white",
    high = "#c53030",
    midpoint = median(region_plot_df$rel_MAE_RMSE, na.rm = TRUE),
    limits = quantile(region_plot_df$rel_MAE_RMSE, c(0.05, 0.95), na.rm = TRUE),
    name = "RMSE / MAE"
  ) +
  labs(
    title = "Regional Error Ranking",
    subtitle = "MAE with relative RMSE/MAE signal",
    x = NULL,
    y = "Mean Absolute Error"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    legend.position = "right"
  )

ggsave("bars_relRMSE_MAE_by_region.jpg",
       plot = bars_relRMSE_MAE_by_region,
       width = 8, height = 5, dpi = 300)


region_plot_df <- region_plot_df %>%
  mutate(rel_error_cap = pmax(pmin(rel_error, 1), -1))
bars_MAE_dir_error_region <- ggplot(
  region_plot_df,
  aes(
    x = reorder(region, MAE),
    y = MAE,
    fill = rel_error_cap
  )
) +
  geom_col(width = 0.75, alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#2b6cb0",
    mid = "white",
    high = "#c53030",
    midpoint = 0,
    name = "Relative error"
  ) +
  labs(
    title = "Regional MAE with Error Direction",
    subtitle = "Positive = overestimation, Negative = underestimation",
    x = NULL,
    y = "Mean Absolute Error"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    legend.position = "right"
  )
ggsave("bars_MAE_dir_error_region.jpg",
       plot = bars_MAE_dir_error_region,
       width = 8, height = 5, dpi = 300)

#### Densidad errores total

#### Densidad errores por regiones
top_regions <- region_plot_df %>%
  slice_max(MAE, n = 20) %>%
  pull(region)

all_errors_top <- all_errors %>% filter(region %in% top_regions)

density_errors_top20Regions <- ggplot(all_errors_top,
                                      aes(x = error)) +
  geom_density(alpha = 0.3) +
  scale_x_log10() +
  theme_minimal()

#### Corr per region  

corr_region_negCorr <- correlations_df %>%
  group_by(region) %>%
  summarise(
    count_negCorr = sum(spearman_corr <= 0.1, na.rm = TRUE),
    count_posCorr = sum(spearman_corr > 0.1, na.rm = TRUE)
  ) %>%
  mutate(
    porc_neg = count_negCorr / (count_negCorr + count_posCorr)
  ) %>%
  arrange(desc(porc_neg)) 

negCorr_per_region <- ggplot(
  corr_region_negCorr,
  aes(
    x = reorder(region, porc_neg),
    y = porc_neg
  )
) +
  geom_col(width = 0.75, fill = "#2b6cb0", alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Share of Spurious or Negative Correlations by Region",
    subtitle = "Proportion of Spearman correlations ≤ 0.1",
    x = NULL,
    y = "Share of weak correlations"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9)
  )


ggsave("negCorr_per_region.jpg",
       plot = negCorr_per_region,
       width = 8, height = 5, dpi = 300)


##Sistematicamente encontramos que hay un bias negativo, lo cual implica que value_chY > value_ref, es decir el valor estimado sobreestimado sobre el histórico


################# POR QUERY ##################

####Top 20% queries with more error acumulado. Qué query acumulad más error?
query_metrics_top20 <- query_metrics_all %>%
  filter(MAE > quantile(MAE, 0.80, na.rm = TRUE)) %>%
  arrange(desc(MAE)) %>%
  slice_head(n = 10) %>% mutate(query = ifelse(query == 'international competition share-weights (Armington intl. taste)', 
                                               'international competition share-weights', query))


query_metrics_top20 <- query_metrics_top20 %>%
  mutate(rel_MAE_RMSE_cap = rel_MAE_RMSE/max(rel_MAE_RMSE))

bars_relRMSE_MAE_by_query <-ggplot(query_metrics_top20, aes(x = reorder(query, MAE), y = MAE, fill = rel_MAE_RMSE_cap)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 16)+
  #scale_y_log10() +
  scale_fill_gradient2(
    low = "yellow",
    mid = "orange",
    high = "red",
    midpoint = median(query_metrics_top20$rel_MAE_RMSE_cap, na.rm = TRUE),
    limits = quantile(query_metrics_top20$rel_MAE_RMSE_cap, c(0.05, 0.95), na.rm = TRUE)
  ) +
  labs(x = "query", y = "MAE total", fill = "RMSE / MAE", , title = "Error ranking by query")


ggplot(query_metrics_top20,
       aes(x = reorder(query, MAE), y = MAE)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 16)+
  labs(x = "Query", y = "MAE (log)")


#########Top query#########

all_errors_top_query <- all_errors %>%
  filter(query == "profit rate") %>%
  select(where(~ !all(is.na(.))))

variable_error <- all_errors_top_query %>%
  group_by(landleaf) %>%
  summarise(mean_abs_error = mean(abs_error, na.rm = TRUE),
            total = n()) %>%
  arrange(desc(mean_abs_error))

ggplot(variable_error,
       aes(x = reorder(landleaf, mean_abs_error),
           y = mean_abs_error)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 16)+
  labs(x = "Sector", y = "Mean Abs Error")

##traded beef
all_errors_top_query_beef <- all_errors_top_query %>% 
  filter(sector == 'traded beef')

region_error <- all_errors_top_query_beef %>%
  group_by(subsector ) %>%
  summarise(mean_abs_error = mean(abs_error, na.rm = TRUE),
            total = n()) %>%
  arrange(desc(mean_abs_error))

ggplot(region_error,
       aes(x = reorder(subsector, mean_abs_error),
           y = mean_abs_error)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 16)+
  labs(x = "Region", y = "Mean Abs Error")

### indian traded beef

indianTRadedBeef <- all_errors_top_query %>% 
  filter(sector == 'traded beef' & subsector == 'India traded beef')
df_long <- indianTRadedBeef %>%
  select(year, value_ref, value_chY) %>%
  pivot_longer(cols = c(value_ref, value_chY),
               names_to = "series",
               values_to = "value")
ggplot(df_long, aes(x = year, y = value, color = series)) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 16)+
  scale_y_log10() +
  labs(x = "Year", y = "Value", color = "Series")








####corr negativa
corr_query_negCorr <- correlations_df %>%
  group_by(query) %>%
  summarise(
    count_negCorr = sum(spearman_corr <= 0.1, na.rm = TRUE),
    count_posCorr = sum(spearman_corr > 0.1, na.rm = TRUE)
  ) %>%
  mutate(
    porc_neg = count_negCorr / (count_negCorr + count_posCorr)
  )  %>%
  arrange(desc(porc_neg)) 

df_plot <- correlations_df %>%
  group_by(query) %>%
  summarise(
    count_negCorr = sum(spearman_corr <= 0.1, na.rm = TRUE),
    count_posCorr = sum(spearman_corr > 0.1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porc_neg = count_negCorr / (count_negCorr + count_posCorr)
  ) %>%
  arrange(desc(porc_neg)) %>%
  slice_head(n = 20)

negCorr_per_query <- ggplot(df_plot, aes(x = reorder(query, porc_neg), y = porc_neg)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = count_negCorr), hjust = -0.2, size = 3) +
  theme_minimal(base_size = 16)+
  labs(x = "Query", y = "Proporción (< 0.1)")




##################### Indicador de error conjunto

######## Normalización: Solo usamos lo normalizado si queremos combinar métricas ######## 
clean_numeric <- function(x) {
  x[!is.finite(x)] <- NA_real_
  x
}

normalize <- function(x) {
  x <- clean_numeric(x)
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  
  (x - m) / s
}
normalize_df <- function(df){
  df <- df %>%
    mutate(
      MAE = normalize(MAE),
      RMSE = normalize(RMSE),
      bias_ratio = normalize(bias_ratio),
      rel_error = normalize(rel_error)
    )
  return (df)
}

year_metrics_norm <- normalize_df(year_metrics_all)
region_metrics_norm <- normalize_df(region_metrics_all)
query_metrics_norm <-normalize_df(query_metrics_all)




########################## BORADOR ########################## 



# points_RMSE_MAE_by_region <- ggplot(region_plot_df, aes(x = MAE, y = RMSE, label = region)) +
#   geom_point() +
#   geom_text(size = 2, vjust = -0.5) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_minimal()
# 
# bars_MAE_by_region <- ggplot(region_plot_df, aes(x = reorder(region, MAE), y = MAE)) +
#   geom_col() +
#   coord_flip() +
#   theme_minimal() +
#   labs(x = "Region", y = "Total MAE", title = "Ranking de error por región")

# 
# ggplot(region_plot_df, aes(x = MAE)) +
#   geom_density(fill = "grey", alpha = 0.4) +
#   theme_minimal() +
#   labs(x = "MAE", y = "Density")




# bottom_regions <- region_plot_df %>%
#   slice_min(MAE, n = 4) %>%
#   pull(region)
# 
# all_errors_bottom <- all_errors %>% filter(region %in% bottom_regions)
# 
# ggplot(all_errors_bottom,
#        aes(x = error, fill = region)) +
#   geom_density(alpha = 0.3) +
#   scale_x_log10() +
#   theme_minimal()
# 
# 
# 
# 
# 
# 
# top_regions <- region_plot_df %>%
#   slice_max(MAE, n = 4) %>%
#   pull(region)
# 
# all_errors_top <- all_errors %>% filter(region %in% top_regions)
# 
# ggplot(all_errors_top,
#        aes(x = error, fill = region)) +
#   geom_density(alpha = 0.3) +
#   scale_x_log10() +
#   theme_minimal()
# 
# 
# 
# 
# region_metrics_norm_negCorr <- region_metrics_all %>%
#   group_by(region) %>%
#   summarise(
#     count_negCorr = sum(spearman_corr < 0, na.rm = TRUE),
#     count_posCorr = sum(spearman_corr > 0, na.rm = TRUE),
#     total_count   = n()
#   ) %>% mutate(
#     count_negCorr_porc = count_negCorr/ total_count,
#     count_posCorr_porc = count_posCorr/ total_count
#   )
# 
# region_metrics_all %>%
#   filter(spearman_corr < 0) %>%
#   group_by(region) %>%
#   summarise(total_count   = n()) %>%
#   arrange(desc(total_count))
# 


# ###spurious relationship
# spurious <-  correlations_df %>%
#   filter(!is.na(spearman_corr), spearman_corr > -0.1, spearman_corr < 0.1)
# 
# density_spurious_corr <- ggplot(spurious,
#                                 aes(x = spearman_corr)) +
#   geom_density(alpha = 0.3) +
#   theme_minimal()
# 
# ###weak non-positive relationship
# weak <-  correlations_df %>%
#   filter(!is.na(spearman_corr), spearman_corr > 0.1, spearman_corr < 0.5)
# 
# density_weak_corr <- ggplot(weak,
#                             aes(x = spearman_corr)) +
#   geom_density(alpha = 0.3) +
#   theme_minimal()


# 
# 
# correlations_var_negCorr <- correlations_df %>%
#   group_by(variable) %>%
#   summarise(
#     count_negCorr = sum(spearman_corr <= 0.1, na.rm = TRUE),
#     count_posCorr = sum(spearman_corr > 0.1, na.rm = TRUE)
#   ) %>%
#   mutate(
#     porc_neg = count_negCorr / (count_negCorr + count_posCorr)
#   )  %>%
#   arrange(desc(porc_neg)) 


# bias_stats <- all_errors_bias %>%
#   summarise(
#     neg = mean(error_bias < 0, na.rm = TRUE),
#     zero = mean(error_bias == 0, na.rm = TRUE),
#     pos = mean(error_bias > 0, na.rm = TRUE)
#   )
#   
# label_df <- data.frame(
#   x = c(-max(abs(all_errors_bias$error_bias), na.rm = TRUE) * 0.6,
#         0,
#         max(abs(all_errors_bias$error_bias), na.rm = TRUE) * 0.6),
#   y = c(0, 0, -2),
#   label = c(
#     paste0("Negative: ", scales::percent(bias_stats$neg)),
#     paste0("Zero: ", scales::percent(bias_stats$zero)),
#     paste0("Positive: ", scales::percent(bias_stats$pos))
#   )
# )
# 
# 
# bias_error <- ggplot(all_errors_bias, aes(x = error_bias)) +
#   geom_histogram(bins = 50, fill = "tomato", alpha = 0.7) +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   theme_minimal(base_size = 16) +
#   labs(
#     title = "Bias Distribution",
#     x = "Relative bias",
#     y = "Count"
#   )
# ggsave("bias_error.jpg", plot = bias_error, width = 10, height = 8, dpi = 300)
# 

##### otro

# ECDF_bias <- ggplot(all_errors_bias, aes(x = error_bias)) +
#   stat_ecdf(
#     geom = "step",
#     linewidth = 1,
#     color = "steelblue"
#   ) +
#   geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8, color = "red") +
#   labs(
#     title = "Cumulative Distribution of Relative Bias",
#     subtitle = "ECDF of (prediction - reference) / reference",
#     x = "Relative bias",
#     y = "Cumulative probability"
#   ) +
#   theme_minimal(base_size = 16)+
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal(base_size = 13) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank()
#   ) +
#   theme_minimal(base_size = 16)
# 
# 
# ggsave("bias_ECDF_error.jpg", plot = ECDF_bias, width = 10, height = 8, dpi = 300)



####### heatmap #########

# 
# 
# # -----------------------------
# # 1. AGREGACIÓN DE ERROR
# # -----------------------------
# query_error <- all_errors %>%
#   group_by(query, region, year) %>%
#   summarise(
#     abs_error = mean(abs(value_ref - value_chY), na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # -----------------------------
# # 2. MATRIZ (queries como columnas)
# # -----------------------------
# mat <- query_error %>%
#   mutate(cell = paste(region, year, sep = "_")) %>%
#   select(query, cell, abs_error) %>%
#   pivot_wider(names_from = query, values_from = abs_error)
# 
# # quitar identificadores
# mat_values <- mat %>% select(-cell)
# 
# # asegurar numéricos puros
# mat_values <- as.data.frame(lapply(mat_values, function(x) as.numeric(x)))
# 
# # quitar queries sin variación
# mat_values <- mat_values[, apply(mat_values, 2, sd, na.rm = TRUE) > 0]
# 
# # -----------------------------
# # 3. CORRELACIÓN ENTRE QUERIES
# # -----------------------------
# cor_mat <- cor(mat_values, use = "pairwise.complete.obs", method = "pearson")
# 
# # -----------------------------
# # 4. FILTRADO DE RELACIONES FUERTES
# # -----------------------------
# cor_mat_strong <- cor_mat
# cor_mat_strong[abs(cor_mat_strong) < 0.8] <- NA
# 
# # eliminar filas/columnas vacías
# keep <- rowSums(!is.na(cor_mat_strong)) > 0
# cor_mat_strong <- cor_mat_strong[keep, keep]
# 
# # -----------------------------
# # 5. LONG FORMAT PARA GGPLOT
# # -----------------------------
# cor_long_strong <- as.data.frame(cor_mat_strong) %>%
#   rownames_to_column("query1") %>%
#   pivot_longer(-query1, names_to = "query2", values_to = "corr") %>%
#   filter(!is.na(corr))
# 
# # -----------------------------
# # 6. HEATMAP
# # -----------------------------
# ggplot(cor_long_strong, aes(x = query1, y = query2, fill = corr)) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "blue",
#     mid = "white",
#     high = "red",
#     midpoint = 0
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
#   )
# 
# ####### CLUSTERING ####### 
# query_features <- all_errors %>%
#   group_by(query) %>%
#   summarise(
#     mean_abs_error = mean(abs_error, na.rm = TRUE),
#     sd_abs_error   = sd(abs_error, na.rm = TRUE),
#     spearman_corr  = cor(value_ref, value_chY,
#                          method = "spearman",
#                          use = "complete.obs"),
#     n_obs = n()
#   )
# 
# var_presence <- all_errors %>%
#   select(query, all_of(vars)) %>%
#   pivot_longer(-query, names_to = "variable", values_to = "value") %>%
#   mutate(present = !is.na(value)) %>%
#   group_by(query, variable) %>%
#   summarise(present = mean(present), .groups = "drop") %>%
#   pivot_wider(names_from = variable, values_from = present, values_fill = 0)
# 
# query_features <- query_features %>%
#   left_join(var_presence, by = "query")
# 
# query_features <- query_features %>%
#   mutate(spearman_corr = ifelse(is.na(spearman_corr), 0, spearman_corr))
# 
# df_clust <- query_features %>%
#   select(-query)
# 
# df_scaled <- scale(df_clust)
# 
# 
# set.seed(123)
# 
# ss <- numeric(10)
# wss <- numeric(10)
# for (k in 1:10) {
#   km <- kmeans(df_scaled, centers = k, nstart = 10)
#   wss[k] <- km$tot.withinss
# }
# plot(1:10, wss, type = "b",
#      xlab = "Número de clusters (k)",
#      ylab = "Within-cluster sum of squares",
#      main = "Método del codo")
# 
# k <- 3 # ajústalo si quieres
# km <- kmeans(df_scaled, centers = k)
# 
# query_features$cluster <- km$cluster
# 
# 
# pca <- prcomp(df_scaled)
# 
# pca_df <- as.data.frame(pca$x[,1:2])
# pca_df$cluster <- as.factor(query_features$cluster)
# 
# ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
#   geom_point() +
#   theme_minimal()
