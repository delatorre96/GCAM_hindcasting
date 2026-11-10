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


all_errors_output_by_tech <- read_parquet('all_errors_output_by_tech.parquet') %>% filter(Units %in% c('EJ', 'Mt N', 'Mt NH3'))



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

# 1. Sensitivity analysis: Logit parameter vs. output value

plot(
  params_outputs_most_var$param,
  params_outputs_most_var$value_run,
  main = paste0(
    "Sensitivity analysis: ", output,
    "\n", region, " | ", sector, " | ", subsector,
    " | ", technology
  ),
  xlab = "Logit parameter value",
  ylab = paste0("Output value (", unique(params_outputs_most_var$Units), ")"),
  pch = 19,
  col = rgb(0, 0, 1, 0.5),
  cex = 0.8
)

# Linear trend
abline(
  lm(value_run ~ param, data = params_outputs_most_var),
  col = "red",
  lwd = 2
)

# Default parameter reference
abline(
  v = unique(params_outputs_most_var$param_default),
  col = "darkgreen",
  lty = 2,
  lwd = 2
)

legend(
  "topright",
  legend = c("Simulation results", "Linear trend", "Default parameter"),
  col = c(rgb(0, 0, 1, 0.5), "red", "darkgreen"),
  pch = c(19, NA, NA),
  lty = c(NA, 1, 2),
  lwd = c(NA, 2, 2),
  bty = "n"
)



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
abline(
  lm(error ~ param, data = params_outputs_most_var),
  col = "red",
  lwd = 2
)

# Default parameter reference
abline(
  v = unique(params_outputs_most_var$param_default),
  col = "darkgreen",
  lty = 2,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Simulation results", "Linear trend", "Zero error", "Default parameter"),
  col = c(rgb(0, 0, 1, 0.5), "red", "black", "darkgreen"),
  pch = c(19, NA, NA, NA),
  lty = c(NA, 1, 2, 2),
  lwd = c(NA, 2, 2, 2),
  bty = "n"
)


############### Logits general behavior ###############

# =============================================================================
# 1. Prepare logit dataset
# =============================================================================

logits <- ptab %>%
  select(
    type_of_param,
    param,
    param_default,
    region,
    run_id,
    supplysector,
    subsector,
    demand_sector,
    input_name_csv
  ) %>%
  distinct() %>%
  mutate(
    delta = param - param_default,
    direction = case_when(
      delta > 0 ~ "Increase",
      delta < 0 ~ "Decrease",
      delta == 0 ~ "Unchanged",
      TRUE ~ NA_character_
    )
  )

# Check dataset
glimpse(logits)

# Basic checks
summary(logits$delta)
table(logits$direction, useNA = "ifany")



# =============================================================================
# 2. Global statistical summary
# =============================================================================

global_stats <- logits %>%
  summarise(
    n = sum(!is.na(delta)),

    mean_delta = mean(delta, na.rm = TRUE),
    median_delta = median(delta, na.rm = TRUE),
    sd_delta = sd(delta, na.rm = TRUE),

    min_delta = min(delta, na.rm = TRUE),
    p05 = quantile(delta, 0.05, na.rm = TRUE),
    p25 = quantile(delta, 0.25, na.rm = TRUE),
    p75 = quantile(delta, 0.75, na.rm = TRUE),
    p95 = quantile(delta, 0.95, na.rm = TRUE),
    max_delta = max(delta, na.rm = TRUE),

    pct_increase = mean(delta > 0, na.rm = TRUE) * 100,
    pct_decrease = mean(delta < 0, na.rm = TRUE) * 100,
    pct_unchanged = mean(delta == 0, na.rm = TRUE) * 100
  )

print(global_stats)



# Distribution of logit perturbations

hist(
  logits$delta,
  breaks = 50,
  main = "Global distribution of logit perturbations",
  xlab = "Logit variation (param - param_default)",
  ylab = "Frequency",
  col = "lightblue",
  border = "white"
)

abline(
  v = 0,
  col = "red",
  lwd = 2,
  lty = 2
)

abline(
  v = mean(logits$delta, na.rm = TRUE),
  col = "darkgreen",
  lwd = 2
)

legend(
  "topright",
  legend = c("No variation", "Mean variation"),
  col = c("red", "darkgreen"),
  lty = 2,
  lwd = 2,
  bty = "n"
)

#####Analisis por tipo de param


# =============================================================================
# 3. Statistical summary by parameter type
# =============================================================================

stats_by_type <- logits %>%
  group_by(type_of_param) %>%
  summarise(
    n = sum(!is.na(delta)),

    mean_delta = mean(delta, na.rm = TRUE),
    median_delta = median(delta, na.rm = TRUE),
    sd_delta = sd(delta, na.rm = TRUE),

    p05 = quantile(delta, 0.05, na.rm = TRUE),
    p95 = quantile(delta, 0.95, na.rm = TRUE),

    pct_increase = mean(delta > 0, na.rm = TRUE) * 100,
    pct_decrease = mean(delta < 0, na.rm = TRUE) * 100,
    pct_unchanged = mean(delta == 0, na.rm = TRUE) * 100,

    .groups = "drop"
  ) %>%
  arrange(desc(mean_delta))

print(stats_by_type)


boxplot(
  delta ~ type_of_param,
  data = logits,
  main = "Logit perturbations by parameter type",
  xlab = "Parameter type",
  ylab = "Logit variation",
  col = "lightblue",
  border = "gray40",
  las = 2
)

abline(
  h = 0,
  col = "red",
  lty = 2,
  lwd = 2
)

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
    technology,
    Units
  ) %>%
  summarise(

    n = sum(!is.na(value_run)),

    # Logit variation
    mean_abs_delta = mean(abs(param - param_default), na.rm = TRUE),

    # Output variability
    mean_output = mean(value_run, na.rm = TRUE),
    sd_output = sd(value_run, na.rm = TRUE),

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
# Top 40 logits associated with highest output variability
# =============================================================================

top_output_sensitivity <- stats_output_sensitivity %>%
  arrange(desc(cv_output)) %>%
  slice_head(n = 40) %>%
  arrange(cv_output)  # Lowest to highest for horizontal barplot

# Parameter labels
parameter_labels <- paste(
  top_output_sensitivity$type_of_param,
  top_output_sensitivity$region,
  top_output_sensitivity$output,
  top_output_sensitivity$technology,
  sep = " | "
)

par(mar = c(5, 20, 4, 2))

barplot(
  top_output_sensitivity$cv_output,
  names.arg = parameter_labels,
  horiz = TRUE,
  las = 1,
  col = "steelblue",
  border = NA,
  main = "Top 40 parameters by relative output variability",
  xlab = "Output coefficient of variation (%)",
  cex.names = 0.7
)

par(mar = c(5, 4, 4, 2))

############# Con error
top_error_sensitivity <- stats_output_sensitivity %>%
  arrange(desc(sd_error)) %>%
  slice_head(n = 20) %>%
  arrange(sd_error)  # Lowest to highest for horizontal barplot

# Parameter labels
parameter_labels <- paste(
  top_error_sensitivity$type_of_param,
  top_error_sensitivity$region,
  top_error_sensitivity$output,
  top_error_sensitivity$technology,
  sep = " | "
)

par(mar = c(5, 20, 4, 2))

barplot(
  top_error_sensitivity$sd_error,
  names.arg = parameter_labels,
  horiz = TRUE,
  las = 1,
  col = "steelblue",
  border = NA,
  main = "Top 20 parameters by absolute error variability",
  xlab = "Standard deviation of error",
  cex.names = 0.7
)

par(mar = c(5, 4, 4, 2))
