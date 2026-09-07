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



###Extract outputs ####
outputs_by_tech <- read_experiment_output(
  con = con,
  query_name = query_name,
  execution_errors = TRUE,
  experiment_id
)


# Reference #
ref_values <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% 
  select(-query, -year, -rel_error, -value_chY, -error, -abs_error)

#errors#
all_errors_output_by_tech <- outputs_by_tech %>% 
  left_join(ref_values, by = c('region', 'technology', 'subsector', 'output', 'sector'))  %>% 
  filter(!is.na(value_ref) | !is.na(`2021`) ) %>%
  mutate(error = value_ref -`2021`,
         error_abs = abs(value_ref -`2021`)) %>%
  select(-`2021`, -value_ref)  %>% drop_na()

#errors per run#
error_per_run <- all_errors_output_by_tech %>% 
  group_by(run_id) %>%
  summarise(MAE = mean(error_abs),
            RMSE = sqrt(mean(error^2))) %>%
  mutate(MAE_log = log1p(MAE),
         RMSE_log = log1p(RMSE))



###Extract and transform inputs in a wide form ####

inputs <- read_experiment_inputs(
  con = con,
  execution_errors = TRUE,
  experiment_id
)  %>% 
  select(-fillout, -year) %>% select(xml_file, xpath, logit, run_id)


input_keys <- inputs %>%
  select(
    xpath,
    xml_file
  ) %>%
  distinct() %>%
  mutate(input_id = row_number())


inputs <- inputs %>%
  left_join(
    input_keys,
    by = c(
      "xml_file",
      'xpath'
    )
  )


inputs_wide <- inputs %>%
  select(run_id, input_id, logit) %>%
  pivot_wider(
    id_cols = run_id,
    names_from = input_id,
    values_from = logit,
    names_prefix = "input_"
  )

# Merge errors per run with its inputs #

df_merge <- error_per_run %>% left_join(inputs_wide, by = 'run_id')

######## Correlacion inputs con MAE ########

input_cols <- grep("^input_", names(df_merge), value = TRUE)

# Calcular correlación de Spearman
cor_results <- df_merge %>%
  select(MAE_log, all_of(input_cols)) %>%
  summarise(
    across(
      all_of(input_cols),
      ~ cor(.x, MAE_log, method = "spearman", use = "complete.obs")
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "input",
    values_to = "correlation"
  ) %>%
  mutate(
    abs_correlation = abs(correlation)
  ) %>%
  arrange(desc(abs_correlation))

