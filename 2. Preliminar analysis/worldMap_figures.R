library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(tidyr)
library(rmap)
library(sf)

region_metrics_all <- read.csv('Data/region_metrics_all.csv')


region_plot_df <- region_metrics_all %>%
  group_by(region) %>% summarise(MAE = sum(MAE),
                                 RMSE = sum(RMSE),
                                 rel_MAE_RMSE = mean(rel_MAE_RMSE),
                                 rel_error = mean(rel_error),
                                 Min_spearman_corr = min(spearman_corr, na.rm = TRUE))  %>%
  arrange(desc(MAE))




###### World Visualization

### Gráfico de barras

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


#################### grafico m
df_summary <- region_plot_df %>%
  mutate(
    region = case_when(
      region == "EU-12" ~ "EU_12",
      region == "EU-15" ~ "EU_15",
      TRUE ~ region
    )
  )

df_map <- dplyr::left_join(mapGCAMReg32, df_summary, by = "region")

p_mae <- ggplot(df_map) +
  geom_sf(aes(fill = MAE)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Regional MAE with Error Direction") +
  theme_minimal()

p_rel <- ggplot(df_map) +
  geom_sf(
    aes(fill = rel_error_cap),
    color = "grey40",
    linewidth = 0.1
  ) +
  scale_fill_gradient2(
    low = "#2b6cb0",
    mid = "white",
    high = "#c53030",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Relative error"
  ) +
  coord_sf(
    expand = FALSE
  ) +
  labs(
    title = "Regional Error Direction",
    subtitle = "Blue = underestimation, Red = overestimation",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(
      color = "grey80",
      linewidth = 0.3
    )
  )

####################

all_errors <- read.csv('Data/all_errors.csv')
key_names <- c("query","region","year","value_ref", "value_chY","error","abs_error","rel_error")
vars <- setdiff(names(all_errors), key_names)
df_long <- all_errors %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "val",
    values_drop_na = TRUE
  )


correlations_df <- df_long %>%
  group_by(query, region, variable) %>%
  summarise(
    spearman_corr = cor(value_ref, value_chY,
                        method = "spearman",
                        use = "complete.obs"),
    .groups = "drop"
  )


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


####################
df_summary <- corr_region_negCorr %>%
  mutate(
    region = case_when(
      region == "EU-12" ~ "EU_12",
      region == "EU-15" ~ "EU_15",
      TRUE ~ region
    )
  )

df_map <- dplyr::left_join(mapGCAMReg32, df_summary, by = "region")

p_corr<- ggplot(df_map) +
  geom_sf(aes(fill = porc_neg)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Share of Spurious or Negative Correlations by Region",
       subtitle = "Proportion of Spearman correlations ≤ 0.1") +
  theme_minimal()



######
ggsave(
  filename = "Figures/worldMap_mae.png",
  plot = p_mae,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "Figures/worldMap_rel.png",
  plot = p_rel,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "Figures/worldMap_corr.png",
  plot = p_corr,
  width = 10,
  height = 6,
  dpi = 300
)
