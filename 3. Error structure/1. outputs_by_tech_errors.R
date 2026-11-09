library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ranger)
library(readxl)

regions_eur <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czech Republic",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Netherlands",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Albania",
  "Bosnia and Herzegovina",
  "Iceland",
  "Macedonia",
  "Moldova",
  "Norway",
  "Serbia and Montenegro",
  "Turkey",
  "UK",
  "Ukraine",
  "Appenine_Peninsula",
  "British_Isles",
  "Central_Eastern_Europe",
  "Central_Western_Europe",
  "Iberian_Peninsula",
  "Northern_Europe",
  "South_Eastern_Europe",
  "Ukraine_Moldova"
)


all_errors <- read.csv('../2. Extraction/Data/all_errors_output_by_tech_norm.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% filter (region %in% regions_eur)


calculate_error_by_dim <- function(df, dim){

  df_sum <- df %>%
    mutate(
      rel_error = if_else(rel_error > 0, 1, -1),
      increase_error = (value_chY - value_ref) / value_ref
    ) %>%
    group_by(across(all_of(dim))) %>%
    summarise(
      total_abs_error = sum(abs_error, na.rm = TRUE),
      pct_ch_mean = mean(pct_ch, na.rm = TRUE),
      net_pct_ch_mean = mean(net_pct_ch, na.rm = TRUE),
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(error^2, na.rm = TRUE)),

      positive_error_share = if_else(
        sum(abs_error, na.rm = TRUE) > 0,
        sum(abs_error[rel_error > 0], na.rm = TRUE) /
          sum(abs_error, na.rm = TRUE) * 100,
        NA_real_
      ),

      negative_error_share = if_else(
        sum(abs_error, na.rm = TRUE) > 0,
        sum(abs_error[rel_error < 0], na.rm = TRUE) /
          sum(abs_error, na.rm = TRUE) * 100,
        NA_real_
      ),

      pct_positive = mean(rel_error == 1, na.rm = TRUE) * 100,
      pct_negative = mean(rel_error == -1, na.rm = TRUE) * 100,

      .groups = "drop"
    ) %>%
    arrange(desc(MAE))

  return(df_sum)
}



all_errors_by_region <- calculate_error_by_dim (all_errors, dim = 'region')
all_errors_by_region %>%
  write.csv('Data/errors_by_region.csv', row.names = FALSE)

all_errors_by_sector <- calculate_error_by_dim (all_errors, dim = 'sector')
all_errors_by_sector %>%
  write.csv('Data/errors_by_sector.csv', row.names = FALSE)

all_errors_by_subsector <- calculate_error_by_dim (all_errors, dim = 'subsector')
all_errors_by_subsector %>%
  write.csv('Data/errors_by_subsector.csv', row.names = FALSE)

all_errors_by_output <- calculate_error_by_dim (all_errors, dim = 'output')
all_errors_by_output %>%
  write.csv('Data/errors_by_output.csv', row.names = FALSE)

all_errors_by_technology <- calculate_error_by_dim (all_errors, dim = 'technology')
all_errors_by_technology %>%
  write.csv('Data/errors_by_technology.csv', row.names = FALSE)

all_errors_by_regiontechnology <- calculate_error_by_dim (all_errors, dim = c('region', 'technology'))
all_errors_by_regiontechnology %>%
  write.csv('Data/errors_by_region_technology.csv', row.names = FALSE)

all_errors_by_regionsector <- calculate_error_by_dim (all_errors, dim = c('region', 'sector'))
all_errors_by_regionsector %>%
  write.csv('Data/errors_by_region_sector.csv', row.names = FALSE)




