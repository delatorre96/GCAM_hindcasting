library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ranger)


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


all_errors <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% filter (region %in% regions_eur)


all_errors_by_region <- all_errors %>%
  mutate(rel_error = if_else(rel_error > 0, 1, -1),
         increase_error = value_chY - value_ref / value_ref) %>%
  group_by(region) %>%
  summarise(
    total_abs_error = sum(abs_error, na.rm = TRUE),
    MAE = mean(abs_error, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    norm_total_abs_error = sum(abs_error, na.rm = TRUE),
    norm_MAE = mean(abs_error, na.rm = TRUE),
    norm_RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    pct_positive = mean(rel_error == 1, na.rm = TRUE) * 100,
    pct_negative = mean(rel_error == -1, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(norm_MAE))
all_errors_by_region %>% write.csv('errors_by_region.csv', row.names = FALSE)

all_errors_by_sector <- all_errors %>%
  mutate(rel_error = if_else(rel_error > 0, 1, -1)) %>%
  group_by(sector) %>%
  summarise(
    total_abs_error = sum(abs_error, na.rm = TRUE),
    MAE = mean(abs_error, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    norm_total_abs_error = sum(abs_error, na.rm = TRUE),
    norm_MAE = mean(abs_error, na.rm = TRUE),
    norm_RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    pct_positive = mean(rel_error == 1, na.rm = TRUE) * 100,
    pct_negative = mean(rel_error == -1, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(MAE))
all_errors_by_sector %>% write.csv('errors_by_sector.csv', row.names = FALSE)

all_errors_by_subsector <- all_errors %>%
  mutate(rel_error = if_else(rel_error > 0, 1, -1)) %>%
  group_by(subsector) %>%
  summarise(
    total_abs_error = sum(abs_error, na.rm = TRUE),
    MAE = mean(abs_error, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    norm_total_abs_error = sum(abs_error, na.rm = TRUE),
    norm_MAE = mean(abs_error, na.rm = TRUE),
    norm_RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    pct_positive = mean(rel_error == 1, na.rm = TRUE) * 100,
    pct_negative = mean(rel_error == -1, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(MAE))
all_errors_by_subsector %>% write.csv('errors_by_subsector.csv', row.names = FALSE)

all_errors_by_output <- all_errors %>%
  mutate(rel_error = if_else(rel_error > 0, 1, -1)) %>%
  group_by(output) %>%
  summarise(
    total_abs_error = sum(abs_error, na.rm = TRUE),
    MAE = mean(abs_error, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    norm_total_abs_error = sum(abs_error, na.rm = TRUE),
    norm_MAE = mean(abs_error, na.rm = TRUE),
    norm_RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    pct_positive = mean(rel_error == 1, na.rm = TRUE) * 100,
    pct_negative = mean(rel_error == -1, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(MAE))
all_errors_by_output %>% write.csv('errors_by_output.csv', row.names = FALSE)

all_errors_by_technology <- all_errors %>%
  mutate(rel_error = if_else(rel_error > 0, 1, -1)) %>%
  group_by(technology) %>%
  summarise(
    total_abs_error = sum(abs_error, na.rm = TRUE),
    MAE = mean(abs_error, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    norm_total_abs_error = sum(abs_error, na.rm = TRUE),
    norm_MAE = mean(abs_error, na.rm = TRUE),
    norm_RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    pct_positive = mean(rel_error == 1, na.rm = TRUE) * 100,
    pct_negative = mean(rel_error == -1, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(MAE))
all_errors_by_technology %>% write.csv('errors_by_technology.csv', row.names = FALSE)

all_errors_by_regiontechnology <- all_errors %>%
  mutate(rel_error = if_else(rel_error > 0, 1, -1)) %>%
  group_by(region, technology) %>%
  summarise(
    total_abs_error = sum(abs_error, na.rm = TRUE),
    MAE = mean(abs_error, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    norm_total_abs_error = sum(abs_error, na.rm = TRUE),
    norm_MAE = mean(abs_error, na.rm = TRUE),
    norm_RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    pct_positive = mean(rel_error == 1, na.rm = TRUE) * 100,
    pct_negative = mean(rel_error == -1, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(MAE))
all_errors_by_regiontechnology %>% write.csv('errors_by_region_technology.csv', row.names = FALSE)

