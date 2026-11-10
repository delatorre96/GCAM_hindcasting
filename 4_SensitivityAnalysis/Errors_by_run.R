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
hindcasting_values <- read.csv('../2. Extraction/Data/all_errors_output_by_tech_norm.csv') %>%
  filter(Units %in% c('EJ', 'Mt N', 'Mt NH3'))

MAE_ref <- mean(hindcasting_values$abs_error, na.rm = TRUE)
RMSE_ref <- sqrt(mean(hindcasting_values$error^2, na.rm = TRUE))
pct_ch_ref <- mean(hindcasting_values$pct_ch, na.rm = TRUE)
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
         pct_ch = abs((value_norm - value_ref)/value_ref),
         net_pct_ch = (value_norm - value_ref)/value_ref)





#errors per run#
error_by_run <- all_errors_output_by_tech %>%
  group_by(run_id) %>%
  summarise(MAE = mean(error_abs, na.rm = TRUE),
            RMSE = sqrt(mean(error^2, na.rm = TRUE)),
            pct_ch_mean = mean(pct_ch, na.rm = TRUE),
            net_pct_ch_mean = mean(net_pct_ch, na.rm = TRUE))

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

plot(
  sort(error_by_run$net_pct_ch_mean *100),
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "Net % of cahnge per Monte Carlo run",
  xlab = "Monte Carlo runs (sorted by Net % of cahnge)",
  ylab = "Net % of cahnge",
  cex.main = 1.1
)

# Reference RMSE
abline(
  h = net_pct_ch_ref*100,
  col = "red",
  lwd = 2,
  lty = 2
)

# Legend with exact reference value
legend(
  "topleft",
  legend = sprintf("Reference net %% of change = %.2f%%", net_pct_ch_ref*100),
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)







# =============================================================================
# MAE density
# =============================================================================

mae_density <- density(error_by_run$MAE, na.rm = TRUE)

plot(
  mae_density,
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "Distribution of MAE across Monte Carlo runs",
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
# % of change density
# =============================================================================

net_pct_ch_mean_density <- density(error_by_run$net_pct_ch_mean *100, na.rm = TRUE)

plot(
  net_pct_ch_mean_density,
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "Distribution of % of change across Monte Carlo runs",
  xlab = "% of change",
  ylab = "Density"
)

# Reference MAE
abline(
  v = net_pct_ch_ref*100,
  col = "red",
  lwd = 2,
  lty = 2
)

# Legend with exact reference value
legend(
  "topleft",
  legend = sprintf("Reference net %% of change = %.2f%%", net_pct_ch_ref*100),
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)





write.csv(all_errors_output_by_tech, 'all_errors_output_by_tech_allRuns.csv')
