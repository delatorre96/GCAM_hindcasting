library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ranger)
library(readxl)

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
hindcasting_values <- read.csv('../2. Extraction/Data/all_errors_output_by_tech_norm.csv')

MAE_ref <- mean(hindcasting_values$abs_error, na.rm = TRUE)
RMSE_ref <- sqrt(mean(hindcasting_values$error^2, na.rm = TRUE))
MAE_norm_ref <- mean(hindcasting_values$abs_error_norm, na.rm = TRUE)
RMSE_norm_ref <- sqrt(mean(hindcasting_values$error_norm^2, na.rm = TRUE))


ref_values <- hindcasting_values %>%
  select("Units", "region", "sector", "subsector", "technology", "output", "value_ref", "value_ref_norm", "scalar")



####### Calculate errors #######
all_errors_output_by_tech <- outputs_by_tech %>%
  left_join(ref_values, by = c('region', 'technology', 'subsector', 'output', 'sector'))  %>%
  ####### Dirk normalization #######
  mutate(value_norm = value_run * scalar,
         error_abs = abs(value_ref - value_run),
         error = value_ref - value_run,
         error_abs_norm = abs(value_ref_norm - value_norm),
         error_norm = value_ref_norm - value_norm)
library(arrow)

write_parquet(all_errors_output_by_tech, "all_errors_output_by_tech.parquet")






#errors per run#
error_by_run <- all_errors_output_by_tech %>%
  group_by(run_id) %>%
  summarise(MAE = mean(error_abs, na.rm = TRUE),
            RMSE = sqrt(mean(error^2, na.rm = TRUE)),
            MAE_norm = mean(error_abs_norm, na.rm = TRUE),
            RMSE_norm = sqrt(mean(error_norm^2, na.rm = TRUE)))

# =============================================================================
# MAE per run - Non-normalized
# =============================================================================

plot(
  sort(error_by_run$MAE),
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "MAE per Monte Carlo run (Non-normalized)",
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
# MAE per run - Normalized
# =============================================================================

plot(
  sort(error_by_run$MAE_norm),
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "MAE per Monte Carlo run (Normalized)",
  xlab = "Monte Carlo runs (sorted by MAE)",
  ylab = "Normalized MAE",
  cex.main = 1.1
)

# Reference normalized MAE
abline(
  h = MAE_norm_ref,
  col = "red",
  lwd = 2,
  lty = 2
)

# Legend with exact reference value
legend(
  "topleft",
  legend = sprintf("Reference MAE = %.4f", MAE_norm_ref),
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)


# =============================================================================
# MAE density - Non-normalized
# =============================================================================

mae_density <- density(error_by_run$MAE, na.rm = TRUE)

plot(
  mae_density,
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "Distribution of MAE across Monte Carlo runs (Non-normalized)",
  xlab = "MAE",
  ylab = "Density"
)

# Reference MAE
abline(
  v = MAE_ref,
  col = "red",
  lwd = 2,
  lty = 2
)

# Legend
legend(
  "topright",
  legend = sprintf("Reference MAE = %.4f", MAE_ref),
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)



# =============================================================================
# MAE density - Normalized
# =============================================================================

mae_norm_density <- density(error_by_run$MAE_norm, na.rm = TRUE)

plot(
  mae_norm_density,
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "Distribution of MAE across Monte Carlo runs (Normalized)",
  xlab = "Normalized MAE",
  ylab = "Density"
)

# Reference normalized MAE
abline(
  v = MAE_norm_ref,
  col = "red",
  lwd = 2,
  lty = 2
)

# Legend
legend(
  "topright",
  legend = sprintf("Reference MAE = %.4f", MAE_norm_ref),
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)


write.csv(all_errors_output_by_tech, 'all_errors_output_by_tech_allRuns.csv')
