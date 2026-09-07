library(DBI)
library(RSQLite)
source('functions_.correlation_screening.R')

con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis/gcam_sensitivity.sqlite"
)

experiments <- dbReadTable(con, "Experiments")



corr_satiation_delta <- initial_correlation_screeming(experiment_id = 'EXP_89083094', con = con, param = "satiation_level") %>%
  summary_corr_df()


corr_priceElasticity_delta <- initial_correlation_screeming(experiment_id = 'EXP_9e51df1d', con = con, param = "price_elasticity") %>%
  summary_corr_df()


corr_logit_delta <- initial_correlation_screeming(experiment_id = c('EXP_5a2e3191','EXP_07b59f0c'), con = con, param = 'logit') %>%
  summary_corr_df()


corr_logit_mc <- initial_correlation_screeming(experiment_id = 'EXP_2e96286f', con = con, param = 'logit') %>%
  summary_corr_df()






