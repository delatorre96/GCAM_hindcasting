library(RSQLite)
library(dplyr)
library(tidyr)
library(arrow)

source("mc_link_sensitivity.R")

source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis_dipc/gcam_sensitivity.sqlite"
)


###### Load input Runs #######
query_name <- "outputs_by_tech"
experiment_id <- 'EXP_02a66a90'
inputs_run <- read_experiment_inputs(con,
                                     experiment_id,
                                     execution_errors = NULL)


all_errors_output_by_tech <- read_parquet('Data/all_errors_output_by_tech.parquet')# %>% filter(Units %in% c('EJ', 'Mt N', 'Mt NH3'))



relevance <- read_csv("create_dfParams/df_allParams_relevance.csv")
ptab <- mc_param_table(inputs_run, relevance)

ptab_ <- ptab %>%
  select(type_of_param, param, region, param_default, run_id, supplysector, subsector, demand_sector, input_name_csv) %>%
  rename('sector' = 'supplysector')

params_outputs <- all_errors_output_by_tech %>%
  inner_join(ptab_, by = c("region", "sector","subsector", "run_id"))


most_var <- params_outputs %>%
  group_by(region, sector, subsector, output, technology) %>%
  summarise(
    variance = sd(param, na.rm = TRUE),
    .groups = "drop"
  )

region = 'Spain'
sector = 'agricultural energy use'
subsector = 'stationary'
output = 'agricultural energy use'
technology = 'biomass'





params_outputs_most_var <- params_outputs %>%
  filter (region == 'Spain',
          sector == 'agricultural energy use',
          subsector == 'stationary',
          output == 'agricultural energy use',
          technology == 'biomass')

# 2. Sensitivity analysis: Logit parameter vs. output error


plot(
  params_outputs_most_var$param,
  params_outputs_most_var$error,
  main = paste0(
    "Sensitivity analysis: ", output,
    "\n", region, " | ", sector, " | ", subsector,
    " | ", technology
  ),
  xlab = "Logit parameter value",
  ylab = paste0("Output error (", unique(params_outputs_most_var$Units), ")"),
  pch = 19,
  col = rgb(0, 0, 1, 0.5),
  cex = 0.8
)

# Zero-error reference
abline(
  h = 0,
  col = "black",
  lty = 2,
  lwd = 2
)

# Linear trend
# abline(
#   lm(error ~ param, data = params_outputs_most_var),
#   col = "red",
#   lwd = 2
# )

# Default parameter reference
abline(
  v = unique(params_outputs_most_var$param_default),
  col = "darkgreen",
  lty = 2,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Simulation results", "Zero error", "Default parameter"),
  col = c(rgb(0, 0, 1, 0.5), "black", "darkgreen"),
  pch = c(19, NA, NA, NA),
  lty = c(NA, 1, 2, 2),
  lwd = c(NA, 2, 2, 2),
  bty = "n"
)
params_outputs_most_var_zeroError <- params_outputs_most_var %>%
  filter(error_abs < 0.00001)
params <- params_outputs_most_var_zeroError$param
params_min <- min(params)
params_max <- max(params)
cat('(',params_min, params_max,')')

############### Logits general behavior ###############



############## Que params cambian mas los outputs ##########

# =============================================================================
# Parameter identification
# =============================================================================

params_outputs_id <- params_outputs %>%
  mutate(
    parameter_id = interaction(
      type_of_param,
      region,
      sector,
      subsector,
      demand_sector,
      input_name_csv,
      drop = TRUE
    )
  )

# =============================================================================
# Output and error sensitivity by parameter
# =============================================================================

stats_output_sensitivity <- params_outputs_id %>%
  group_by(
    parameter_id,
    type_of_param,
    region,
    sector,
    subsector,
    output,
    technology
  ) %>%
  summarise(

    n = sum(!is.na(value_norm)),

    # Logit variation
    mean_abs_delta = mean(abs(param - param_default), na.rm = TRUE),

    # Output variability
    mean_output = mean(value_norm, na.rm = TRUE),
    sd_output = sd(value_norm, na.rm = TRUE),

    cv_output = ifelse(
      mean_output != 0,
      sd_output / abs(mean_output) * 100,
      NA_real_
    ),

    # Error variability
    mean_error = mean(error, na.rm = TRUE),
    mean_abs_error = mean(abs(error), na.rm = TRUE),
    sd_error = sd(error, na.rm = TRUE),

    # Association
    cor_param_output = cor(
      param, value_run,
      use = "complete.obs"
    ),

    cor_param_error = cor(
      param, error,
      use = "complete.obs"
    ),

    .groups = "drop"
  )


# =============================================================================
# Top 40 logits associated with highest error variability
# =============================================================================
library(dplyr)
library(ggplot2)
library(forcats)

#----- Select top 20 parameters -----

top_error_sensitivity <- stats_output_sensitivity %>%
  slice_max(order_by = sd_error, n = 20, with_ties = FALSE) %>%
  mutate(
    parameter = paste(region, output, technology, sep = " | "),
    parameter = fct_reorder(parameter, sd_error)
  )

#----- Plot -----

mostSensParams <- ggplot(top_error_sensitivity, aes(x = sd_error, y = parameter, fill = sd_error)) +

  geom_col(width = 0.72, alpha = 0.9) +

  geom_text(
    aes(label = sprintf("%.3f", sd_error)),
    hjust = -0.15,
    size = 3.2,
    color = "grey25"
  ) +

  scale_fill_gradient(
    low = "#A6CEE3",
    high = "#08519C",
    guide = "none"
  ) +

  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +

  labs(
    title = "Parameters with highest error variability",
    subtitle = "Top 20 parameters ranked by standard deviation of absolute error",
    x = "Standard deviation of error",
    y = NULL
  ) +

  theme_minimal(base_size = 12) +

  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = "grey15"
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "grey45",
      margin = margin(b = 15)
    ),
    axis.text.y = element_text(
      size = 9,
      color = "grey25"
    ),
    axis.text.x = element_text(
      size = 9,
      color = "grey40"
    ),
    axis.title.x = element_text(
      size = 10,
      face = "bold",
      margin = margin(t = 10)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "grey90",
      linewidth = 0.4
    ),
    plot.margin = margin(15, 30, 15, 15)
  )
mostSensParams

ggsave(
  "Figures/mostSensParams.png",
  mostSensParams,
  width = 13,
  height = 9,
  dpi = 400,
  bg = "white"
)





### Plotear los más altos:

region  <- 'Germany'
sector <- 'regional biomassOil'
subsector <- 'regional biomassOil'
output <- 'regional biomassOil'
technology <- 'OilPalm'


params_outputs_most_var <- params_outputs %>%
  filter(
    region == .env$region,
    sector == .env$sector,
    subsector == .env$subsector,
    output == .env$output,
    technology == .env$technology
  )

# 2. Sensitivity analysis: Logit parameter vs. output error

#----- Plot -----

ggplot(params_outputs_most_var, aes(x = param, y = error)) +

  # Simulation results
  geom_point(
    color = "#2878B5",
    alpha = 0.55,
    size = 2.3
  ) +

  # Zero-error reference
  geom_hline(
    yintercept = 0,
    color = "grey35",
    linetype = "dashed",
    linewidth = 0.8
  ) +

  # Default parameter
  geom_vline(
    xintercept = param_default,
    color = "#D1495B",
    linetype = "dashed",
    linewidth = 1
  ) +

  labs(
    title = paste0("Sensitivity analysis: ", output),
    subtitle = paste0(
      region, " | ", sector, " | ", subsector,
      " | ", technology
    ),
    x = "Logit parameter value",
    y = paste0(
      "Output error (",
      unique(params_outputs_most_var$Units),
      ")"
    )
  ) +

  theme_minimal(base_size = 12) +

  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 20, 10, 10)
  )







params_outputs_most_var_zeroError <- params_outputs_most_var %>%
  filter(error_abs < 0.1)
params <- params_outputs_most_var_zeroError$param
params_min <- min(params)
params_max <- max(params)
cat('(',params_min, params_max,')')


#----- Density plot -----



library(ggplot2)

#----- Density estimation -----

param_values <- params_outputs_most_var_zeroError$param
param_default <- unique(params_outputs_most_var$param_default)

dens <- density(param_values)

# Maximum density (mode)
max_idx <- which.max(dens$y)
param_mode <- dens$x[max_idx]
density_max <- dens$y[max_idx]

#----- Plot -----

ggplot(data.frame(x = dens$x, y = dens$y), aes(x, y)) +

  # Density area
  geom_area(
    fill = "#2878B5",
    alpha = 0.18
  ) +

  # Density curve
  geom_line(
    color = "#2878B5",
    linewidth = 1.2
  ) +

  # Default parameter
  geom_vline(
    xintercept = param_default,
    color = "#D1495B",
    linetype = "dashed",
    linewidth = 1
  ) +

  # Maximum density point (mode)
  geom_point(
    data = data.frame(x = param_mode, y = density_max),
    aes(x, y),
    color = "#E69F00",
    size = 3.5
  ) +

  # Mode label
  annotate(
    "label",
    x = param_mode,
    y = density_max,
    label = paste0("Mode = ", round(param_mode, 3)),
    color = "#9A6500",
    fill = "white",
    fontface = "bold",
    size = 3.5,
    vjust = -0.9
  ) +

  # Default parameter legend
  scale_color_manual(
    name = NULL,
    values = c("Default parameter" = "#D1495B")
  ) +

  labs(
    title = "Parameter distribution under low prediction error",
    subtitle = paste0(
      "Absolute error < 0.1 | n = ",
      length(param_values)
    ),
    x = "Parameter value",
    y = "Density",
    color = NULL
  ) +

  # Extra space above maximum density
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.22))
  ) +

  theme_minimal(base_size = 12) +

  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.margin = margin(15, 25, 10, 10)
  )






###########################

# library(dplyr)
# library(ggplot2)
# library(patchwork)
#
# #----- Data preparation -----
#
# regions <- c("Germany", "France")
#
# params_comparison <- params_outputs %>%
#   filter(
#     region %in% regions,
#     sector == .env$sector,
#     subsector == .env$subsector,
#     output == .env$output,
#     technology == .env$technology
#   )
#
# #----- Scatterplot function -----
#
# plot_scatter <- function(df, country) {
#
#   param_default <- unique(df$param_default)
#   units <- unique(df$Units)
#
#   ggplot(df, aes(x = param, y = error)) +
#
#     geom_point(
#       color = "#2878B5",
#       alpha = 0.55,
#       size = 2
#     ) +
#
#     geom_hline(
#       yintercept = 0,
#       color = "grey35",
#       linetype = "dashed",
#       linewidth = 0.7
#     ) +
#
#     geom_vline(
#       xintercept = param_default,
#       color = "#D1495B",
#       linetype = "dashed",
#       linewidth = 0.9
#     ) +
#
#     labs(
#       title = country,
#       subtitle = "Simulation results",
#       x = "Logit parameter value",
#       y = paste0("Output error (", units[1], ")")
#     ) +
#
#     theme_minimal(base_size = 11) +
#
#     theme(
#       plot.title = element_text(
#         face = "bold",
#         size = 13
#       ),
#       plot.subtitle = element_text(
#         color = "grey45",
#         size = 9
#       ),
#       axis.title = element_text(face = "bold", size = 10),
#       panel.grid.minor = element_blank(),
#       plot.margin = margin(8, 12, 8, 8)
#     )
# }
#
#
# #----- Density plot function -----
#
# plot_density <- function(df, country) {
#
#   # Select low-error simulations
#   param_values <- df %>%
#     filter(error_abs < 0.1) %>%
#     pull(param)
#
#   param_default <- unique(df$param_default)
#
#   # Density estimation
#   dens <- density(param_values)
#
#   max_idx <- which.max(dens$y)
#
#   param_mode <- dens$x[max_idx]
#   density_max <- dens$y[max_idx]
#
#   dens_df <- data.frame(
#     x = dens$x,
#     y = dens$y
#   )
#
#   # Plot
#   ggplot(dens_df, aes(x, y)) +
#
#     geom_area(
#       fill = "#2878B5",
#       alpha = 0.18
#     ) +
#
#     geom_line(
#       color = "#2878B5",
#       linewidth = 1.1
#     ) +
#
#     geom_vline(
#       xintercept = param_default,
#       color = "#D1495B",
#       linetype = "dashed",
#       linewidth = 0.9
#     ) +
#
#     geom_point(
#       data = data.frame(
#         x = param_mode,
#         y = density_max
#       ),
#       aes(x, y),
#       color = "#E69F00",
#       size = 3
#     ) +
#
#     annotate(
#       "label",
#       x = param_mode,
#       y = density_max,
#       label = paste0("Mode = ", round(param_mode, 3)),
#       color = "#9A6500",
#       fill = "white",
#       fontface = "bold",
#       size = 3,
#       vjust = -0.9
#     ) +
#
#     scale_y_continuous(
#       expand = expansion(mult = c(0.02, 0.25))
#     ) +
#
#     labs(
#       title = country,
#       subtitle = paste0(
#         "Low-error parameter distribution | n = ",
#         length(param_values)
#       ),
#       x = "Logit parameter value",
#       y = "Density"
#     ) +
#
#     theme_minimal(base_size = 11) +
#
#     theme(
#       plot.title = element_text(
#         face = "bold",
#         size = 13
#       ),
#       plot.subtitle = element_text(
#         color = "grey45",
#         size = 9
#       ),
#       axis.title = element_text(face = "bold", size = 10),
#       panel.grid.minor = element_blank(),
#       plot.margin = margin(8, 12, 8, 8)
#     )
# }
#
#
# #----- Generate individual plots -----
#
# p_scatter_GER <- plot_scatter(
#   params_comparison %>% filter(region == "Germany"),
#   "Germany"
# )
#
# p_density_GER <- plot_density(
#   params_comparison %>% filter(region == "Germany"),
#   "Germany"
# )
#
# p_scatter_FRA <- plot_scatter(
#   params_comparison %>% filter(region == "France"),
#   "France"
# )
#
# p_density_FRA <- plot_density(
#   params_comparison %>% filter(region == "France"),
#   "France"
# )
#
#
# #----- Combine plots -----
#
# final_plot <- (
#   p_scatter_FRA | p_density_FRA
# ) / (
#   p_scatter_GER | p_density_GER
# ) +
#
#   plot_annotation(
#     title = paste0(
#       "Sensitivity analysis: ", output
#     ),
#     subtitle = paste0(
#       sector, " | ", subsector, " | ", technology
#     ),
#     theme = theme(
#       plot.title = element_text(
#         face = "bold",
#         size = 16
#       ),
#       plot.subtitle = element_text(
#         color = "grey40",
#         size = 11
#       )
#     )
#   )
#
# final_plot
#
#
# ggsave(
#   "Germany_France_sensitivity.png",
#   final_plot,
#   width = 13,
#   height = 9,
#   dpi = 400,
#   bg = "white"
# )

###################################################################

library(dplyr)
library(ggplot2)
library(patchwork)

#----- Parameters -----

regions <- c("Germany", "France")

sector <- "regional biomassOil"
subsector <- "regional biomassOil"
output <- "regional biomassOil"
technology <- "OilPalm"

#----- Data preparation -----

params_comparison <- params_outputs %>%
  filter(
    region %in% regions,
    sector == .env$sector,
    subsector == .env$subsector,
    output == .env$output,
    technology == .env$technology
  )

#----- Common X-axis -----

x_range <- range(params_comparison$param, na.rm = TRUE)

# Fewer X-axis breaks (approximately 5 intervals)
x_breaks <- pretty(x_range, n = 5)

#----- Common axis scale -----

common_x_scale <- scale_x_continuous(
  breaks = x_breaks,
  limits = x_range,
  expand = expansion(mult = 0.01)
)


#----- Scatterplot function -----

plot_scatter <- function(df, country, show_x_labels = FALSE) {

  param_default <- unique(df$param_default)
  units <- unique(df$Units)

  ggplot(df, aes(x = param, y = error)) +

    geom_point(
      color = "#2878B5",
      alpha = 0.55,
      size = 2
    ) +

    # Zero-error reference
    geom_hline(
      yintercept = 0,
      color = "grey35",
      linetype = "dashed",
      linewidth = 0.7
    ) +

    # Default parameter
    geom_vline(
      xintercept = param_default,
      color = "#D1495B",
      linetype = "dashed",
      linewidth = 0.9
    ) +

    common_x_scale +

    labs(
      title = country,
      x = NULL,
      y = paste0("Output error (", units[1], ")")
    ) +

    theme_minimal(base_size = 11) +

    theme(
      plot.title = element_text(
        face = "bold",
        size = 13
      ),
      axis.title = element_text(
        face = "bold",
        size = 10
      ),
      axis.text.x = if (show_x_labels) {
        element_text(size = 9)
      } else {
        element_blank()
      },
      axis.ticks.x = if (show_x_labels) {
        element_line()
      } else {
        element_blank()
      },
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 12, 2, 8)
    )
}




#----- Density plot function -----

plot_density <- function(df, show_x_labels = FALSE) {

  # Select low-error simulations
  param_values <- df %>%
    filter(error_abs < 0.1) %>%
    pull(param)

  param_default <- unique(df$param_default)

  # Density estimation
  dens <- density(param_values)

  # Maximum density (mode)
  max_idx <- which.max(dens$y)

  param_mode <- dens$x[max_idx]
  density_max <- dens$y[max_idx]

  dens_df <- data.frame(
    x = dens$x,
    y = dens$y
  )

  ggplot(dens_df, aes(x, y)) +

    # Density area
    geom_area(
      fill = "#2878B5",
      alpha = 0.18
    ) +

    # Density curve
    geom_line(
      color = "#2878B5",
      linewidth = 1.1
    ) +

    # Default parameter
    geom_vline(
      xintercept = param_default,
      color = "#D1495B",
      linetype = "dashed",
      linewidth = 0.9
    ) +

    # Maximum density point
    geom_point(
      data = data.frame(
        x = param_mode,
        y = density_max
      ),
      aes(x, y),
      color = "#E69F00",
      size = 3
    ) +

    # Mode label
    annotate(
      "label",
      x = param_mode,
      y = density_max,
      label = paste0("Mode = ", round(param_mode, 3)),
      color = "#9A6500",
      fill = "white",
      fontface = "bold",
      size = 3,
      vjust = -0.9
    ) +

    common_x_scale +

    scale_y_continuous(
      expand = expansion(mult = c(0.02, 0.25))
    ) +

    labs(
      x = NULL,
      y = "Density"
    ) +

    theme_minimal(base_size = 11) +

    theme(
      axis.title = element_text(
        face = "bold",
        size = 10
      ),
      axis.text.x = if (show_x_labels) {
        element_text(size = 9)
      } else {
        element_blank()
      },
      axis.ticks.x = if (show_x_labels) {
        element_line()
      } else {
        element_blank()
      },
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 12, 2, 8)
    )
}




#----- Germany -----

p_scatter_GER <- plot_scatter(
  params_comparison %>% filter(region == "Germany"),
  "Germany",
  show_x_labels = FALSE
)

p_density_GER <- plot_density(
  params_comparison %>% filter(region == "Germany"),
  show_x_labels = FALSE
)


#----- France -----

p_scatter_FRA <- plot_scatter(
  params_comparison %>% filter(region == "France"),
  "France",
  show_x_labels = TRUE
)

p_density_FRA <- plot_density(
  params_comparison %>% filter(region == "France"),
  show_x_labels = TRUE
)






#----- Final comparison -----

final_plot <- (
  p_scatter_GER | p_density_GER
) / (
  p_scatter_FRA | p_density_FRA
) +

  plot_annotation(
    title = paste0(
      "Sensitivity analysis: ", output
    ),
    subtitle = paste0(
      sector, " | ", subsector, " | ", technology
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(
        color = "grey40",
        size = 11
      )
    )
  ) &

  theme(
    axis.text.x = element_text(
      size = 9
    )
  )

final_plot





ggsave(
  "Figures/Germany_France_sensitivity.png",
  final_plot,
  width = 13,
  height = 9,
  dpi = 400,
  bg = "white"
)
