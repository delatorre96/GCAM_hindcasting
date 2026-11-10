library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ranger)
library(readxl)
library(ggplot2)

source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis_dipc/gcam_sensitivity.sqlite"
)

#See interested experiments:
experiments <- dbReadTable(con, "Experiments")

runs <- dbReadTable(con, "Runs")

datasets <- dbReadTable(con, "Datasets")

## Change paths ##

old_path <- "/scratch/gcamteam/hindcasting/GCAM_sensitivity_analysis"

new_path <- "C:/GCAM/Nacho/GCAM_sensitivity_analysis_dipc"

dbExecute(
  con,
  "
  UPDATE Datasets
  SET filepath = REPLACE(filepath, ?, ?)
  WHERE filepath LIKE ?
  ",
  params = list(
    old_path,
    new_path,
    paste0(old_path, "%")
  )
)



query_name <- "outputs_by_tech"
experiment_id <- 'EXP_02a66a90'


###### Load output Runs #######
outputs_by_tech <- read_experiment_output(con,
                             query_name,
                             experiment_id,
                             execution_errors = NULL) %>%
  rename(value_run = `2021`)

####### Load Reference #######
hindcasting_values <- read.csv('../2. Extraction/Data/all_errors_output_by_tech_norm.csv') %>%
  filter(Units %in% c('EJ', 'Mt N', 'Mt NH3'))

MAE_ref <- mean(hindcasting_values$abs_error, na.rm = TRUE)
RMSE_ref <- sqrt(mean(hindcasting_values$error^2, na.rm = TRUE))
abs_pct_ch_ref <- mean(hindcasting_values$abs_pct_ch, na.rm = TRUE)
net_pct_ch_ref <- mean(hindcasting_values$net_pct_ch, na.rm = TRUE)


ref_values <- hindcasting_values %>%
  select("region", "sector", "subsector", "technology", "output","scalar", 'value_ref')



####### Calculate errors #######
all_errors_output_by_tech <- outputs_by_tech %>%
  right_join(ref_values, by = c('region', 'technology', 'subsector', 'output', 'sector')) %>%
  ####### Dirk normalization #######
  mutate(value_norm = value_run * scalar,
         error_abs = abs(value_ref - value_norm),
         error = value_ref - value_norm,
         abs_pct_ch = abs((value_norm - value_ref)/value_ref),
         net_pct_ch = (value_norm - value_ref)/value_ref)



if (!dir.exists("Data")) {
  dir.create("Data")
}

library(arrow)
write_parquet(all_errors_output_by_tech, "Data/all_errors_output_by_tech.parquet")



#errors per run#
error_by_run <- all_errors_output_by_tech %>%
  group_by(run_id) %>%
  summarise(MAE = mean(error_abs, na.rm = TRUE),
            RMSE = sqrt(mean(error^2, na.rm = TRUE)),
            abs_pct_ch_mean = mean(abs_pct_ch, na.rm = TRUE),
            net_pct_ch_mean = mean(net_pct_ch, na.rm = TRUE))



#################################### Figures ####################################

# =============================================================================
# MAE per run
# =============================================================================

plot(
  sort(error_by_run$MAE),
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "MAE per Monte Carlo run",
  xlab = "Monte Carlo runs (sorted by MAE)",
  ylab = "MAE",
  cex.main = 1.1
)

# Reference MAE
abline(
  h = MAE_ref,
  col = "red",
  lwd = 2,
  lty = 2
)

# Legend with exact reference value
legend(
  "topleft",
  legend = sprintf("Reference MAE = %.4f", MAE_ref),
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)

# =============================================================================
# MAE per Monte Carlo run
# =============================================================================

#----- Prepare data -----

df_MAE_plot <- error_by_run %>%
  arrange(MAE) %>%
  mutate(run_rank = row_number())

#----- Plot -----

p_MAE <- ggplot(df_MAE_plot, aes(x = run_rank, y = MAE)) +

  # Area under the curve
  geom_ribbon(
    aes(ymin = 0, ymax = MAE),
    fill = "#4C78A8",
    alpha = 0.12
  ) +

  # MAE curve
  geom_line(
    color = "#315B7D",
    linewidth = 1
  ) +

  # Reference MAE
  geom_hline(
    yintercept = MAE_ref,
    color = "#D65F5F",
    linewidth = 1,
    linetype = "22"
  ) +

  # Reference label
  annotate(
    "label",
    x = Inf,
    y = MAE_ref,
    label = sprintf("Reference MAE: %.4f", MAE_ref),
    hjust = 1.05,
    vjust = -0.7,
    color = "#B44747",
    fill = "white",
    label.size = NA,
    size = 3.5,
    fontface = "bold"
  ) +

  # Labels
  labs(
    title = "Monte Carlo simulation performance",
    subtitle = "Distribution of Mean Absolute Error across simulations",
    x = "Monte Carlo simulations (ranked)",
    y = "Mean Absolute Error (MAE)"
  ) +

  #----- Scale -----

scale_x_continuous(
  expand = expansion(mult = c(0.01, 0.02))
) +

  scale_y_continuous(
    breaks = seq(0.006, 0.014, by = 0.002),
    expand = expansion(mult = c(0, 0))
  ) +

  coord_cartesian(
    ylim = c(0.006, 0.014)
  ) +

  # Theme
  theme_minimal(base_size = 12) +

  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#263238",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey40",
      margin = margin(b = 20)
    ),
    axis.title = element_text(
      size = 10,
      face = "bold",
      color = "#37474F"
    ),
    axis.text = element_text(
      size = 9,
      color = "grey35"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey90",
      linewidth = 0.35
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(15, 20, 15, 15)
  )

#----- Display -----

p_MAE


ggsave(
  "Figures/MAE_per_Monte_Carlo_run.png",
  p_MAE,
  width = 10,
  height = 5.5,
  dpi = 400,
  bg = "white"
)







# =============================================================================
# RMSE per run
# =============================================================================

plot(
  sort(error_by_run$RMSE),
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "RMSE per Monte Carlo run",
  xlab = "Monte Carlo runs (sorted by RMSE)",
  ylab = "RMSE",
  cex.main = 1.1
)

# Reference RMSE
abline(
  h = RMSE_ref,
  col = "red",
  lwd = 2,
  lty = 2
)

# Legend with exact reference value
legend(
  "topleft",
  legend = sprintf("Reference MAE = %.4f", RMSE_ref),
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)


# =============================================================================
# pct change per run
# =============================================================================
#
# plot(
#   sort(error_by_run$abs_pct_ch_mean *100),
#   type = "l",
#   lwd = 2,
#   col = "steelblue",
#   main = "Abs % of cahnge per Monte Carlo run",
#   xlab = "Monte Carlo runs (sorted by Net % of cahnge)",
#   ylab = "Abs % of cahnge",
#   cex.main = 1.1
# )
#
# # Reference RMSE
# abline(
#   h = abs_pct_ch_ref*100,
#   col = "red",
#   lwd = 2,
#   lty = 2
# )
#
# # Legend with exact reference value
# legend(
#   "topleft",
#   legend = sprintf("Reference net %% of change = %.2f%%",abs_pct_ch_ref*100),
#   col = "red",
#   lty = 2,
#   lwd = 2,
#   bty = "n"
# )


# =============================================================================
# Absolute Percentage Change per Monte Carlo run
# =============================================================================

#----- Prepare data -----

df_pct_change_plot <- error_by_run %>%
  arrange(abs_pct_ch_mean) %>%
  mutate(
    run_rank = row_number(),
    abs_pct_change = abs_pct_ch_mean * 100
  )

# Reference value
pct_change_ref <- abs_pct_ch_ref * 100


#----- Plot -----

p_pct_change <- ggplot(
  df_pct_change_plot,
  aes(x = run_rank, y = abs_pct_change)
) +

  # Area under the curve
  geom_ribbon(
    aes(ymin = 0, ymax = abs_pct_change),
    fill = "#4C78A8",
    alpha = 0.12
  ) +

  # Absolute percentage change curve
  geom_line(
    color = "#315B7D",
    linewidth = 1
  ) +

  # Reference value
  geom_hline(
    yintercept = pct_change_ref,
    color = "#D65F5F",
    linewidth = 1,
    linetype = "22"
  ) +

  # Reference label
  annotate(
    "label",
    x = Inf,
    y = pct_change_ref,
    label = sprintf("Reference MAPC: %.3f%%", pct_change_ref),
    hjust = 1.05,
    vjust = -0.7,
    color = "#B44747",
    fill = "white",
    label.size = NA,
    size = 3.5,
    fontface = "bold"
  ) +

  # Labels
  labs(
    title = "Monte Carlo simulation performance",
    subtitle = "Mean Absolute Percentage Change across simulations",
    x = "Monte Carlo simulations (ranked)",
    y = "Mean Absolute Percentage Change (%)"
  ) +

  # Scales
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.02))
  ) +

  scale_y_continuous(
    breaks = seq(105, 135, by = 5),
    limits = c(105, 135),
    expand = c(0, 0)
  )+

  # Theme
  theme_minimal(base_size = 12) +

  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#263238",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey40",
      margin = margin(b = 20)
    ),
    axis.title = element_text(
      size = 10,
      face = "bold",
      color = "#37474F"
    ),
    axis.text = element_text(
      size = 9,
      color = "grey35"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey90",
      linewidth = 0.35
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(15, 20, 15, 15)
  )


#----- Display -----

p_pct_change


# =============================================================================
# Save figures
# =============================================================================

if (!dir.exists("Figures")) {
  dir.create("Figures")
}

ggsave(
  "Figures/Absolute_Percentage_Change_per_Monte_Carlo_run.png",
  p_pct_change,
  width = 10,
  height = 5.5,
  dpi = 400,
  bg = "white"
)





# =============================================================================
# MAE density
# =============================================================================
#
# mae_density <- density(error_by_run$MAE, na.rm = TRUE)
#
# plot(
#   mae_density,
#   type = "l",
#   lwd = 2,
#   col = "steelblue",
#   main = "Distribution of MAE across Monte Carlo runs",
#   xlab = "MAE",
#   ylab = "Density"
# )
#
# # Reference MAE
# abline(
#   v = MAE_ref,
#   col = "red",
#   lwd = 2,
#   lty = 2
# )
#
# # Legend
# legend(
#   "topright",
#   legend = sprintf("Reference MAE = %.4f", MAE_ref),
#   col = "red",
#   lty = 2,
#   lwd = 2,
#   bty = "n"
# )
#----- MAE density plot -----

p_mae_density <- ggplot(error_by_run, aes(x = MAE)) +
  geom_density(
    fill = "steelblue",
    color = "steelblue4",
    alpha = 0.25,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = MAE_ref,
    color = "#D9534F",
    linewidth = 1,
    linetype = "dashed"
  ) +
  annotate(
    "label",
    x = MAE_ref,
    y = Inf,
    label = sprintf("Reference MAE = %.4f", MAE_ref),
    color = "#B23A35",
    fill = "white",
    label.size = 0,
    size = 3.5,
    hjust = -0.05,
    vjust = 1.8
  ) +
  labs(
    title = "Distribution of MAE across Monte Carlo runs",
    subtitle = "Kernel density estimation",
    x = "Mean Absolute Error (MAE)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      color = "grey20"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey45"
    ),
    axis.title = element_text(
      face = "bold",
      color = "grey25"
    ),
    axis.text = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# Display
print(p_mae_density)

# Save
ggsave(
  filename = "Figures/MAE_density.png",
  plot = p_mae_density,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
# =============================================================================
# % of change density
# =============================================================================
#
# abs_pct_ch_mean_density <- density(error_by_run$abs_pct_ch_mean *100, na.rm = TRUE)
#
# plot(
#   abs_pct_ch_mean_density,
#   type = "l",
#   lwd = 2,
#   col = "steelblue",
#   main = "Distribution of % of change across Monte Carlo runs",
#   xlab = "% of change",
#   ylab = "Density"
# )
#
# # Reference MAE
# abline(
#   v = abs_pct_ch_ref*100,
#   col = "red",
#   lwd = 2,
#   lty = 2
# )
#
# # Legend with exact reference value
# legend(
#   "topleft",
#   legend = sprintf("Reference abs %% of change = %.2f%%", abs_pct_ch_ref*100),
#   col = "red",
#   lty = 2,
#   lwd = 2,
#   bty = "n"
# )
#
# if (!dir.exists("Figures")) {
#   dir.create("Figures")
# }

#----- % Change density plot -----

p_abs_pct_ch_density <- ggplot(
  error_by_run,
  aes(x = abs_pct_ch_mean * 100)
) +
  geom_density(
    fill = "steelblue",
    color = "steelblue4",
    alpha = 0.25,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = abs_pct_ch_ref * 100,
    color = "#D9534F",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  annotate(
    "label",
    x = abs_pct_ch_ref * 100,
    y = Inf,
    label = sprintf("Reference value:\n%.2f%%", abs_pct_ch_ref * 100),
    color = "#B23A35",
    fill = "white",
    label.size = 0,
    size = 3.5,
    hjust = 1.1,
    vjust = 1.8,
    lineheight = 1.1
  ) +
  labs(
    title = "Distribution of Mean Absolute Percentage Change",
    subtitle = "Across Monte Carlo runs",
    x = "Mean Absolute Percentage Change (%)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      color = "grey20"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey45"
    ),
    axis.title = element_text(
      face = "bold",
      color = "grey25"
    ),
    axis.text = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# Display
print(p_abs_pct_ch_density)

# Save
ggsave(
  filename = "abs_pct_change_density.png",
  plot = p_abs_pct_ch_density,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)


