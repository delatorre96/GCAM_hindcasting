library(rgcam)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(patchwork)
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

prj1 <- loadProject(proj = "BaseYear2015_outputsByTech.dat")




query = "outputs by tech"




### normalizing ###

normalize_data <- read_excel ('C:/Users/ignacio.delatorre/Downloads/Dirk_conv_inputsoutputsbytech.xlsx') %>%
  select("Region","Supplysector", "subsector","output...5","technology", "scalar" ) %>%
  # drop_na() %>%
  rename('output' = 'output...5',
         'region' = 'Region',
         'sector' = 'Supplysector')


df <- getQuery(prj1, query) %>% filter(year == 2021) %>%
  filter(region %in% regions_eur) %>%
  left_join(normalize_data , by = c("region","sector", "subsector","output","technology")) %>%
  mutate(value = value * scalar, value) %>%
  select(-scalar) %>% drop_na()


key_cols <- colnames(df)
key_cols <- key_cols[!key_cols %in% c("scenario", "value")]


scenario_2015 <- 'BaseYear2015_policy_test_noEEAtradetax'

# separar escenarios
df_ref <- df %>%
  filter(scenario == "Reference") %>%
  select(-scenario) %>%
  rename(value_ref = value)

df_chY <- df %>%
  filter(scenario == scenario_2015) %>%
  select(-scenario) %>%
  rename(value_chY = value)

df_comp <- df_ref %>%
  inner_join(df_chY, by = key_cols)

# error base
df_comp <- df_comp %>%
  mutate(
    error = value_ref - value_chY,
    abs_error = abs(error),
    sq_error = error^2,
    bias_ratio =  ifelse(
      value_chY == 0 & value_ref == 0,
      0,
      value_chY / value_ref
    ),
    rel_error = ifelse(error == 0 & value_ref == 0, 0,error / value_ref),
    query = query
  )

all_errors_output_by_tech <- df_comp %>%
  select(
    region,
    sector,
    subsector,
    technology,
    output,
    value_ref,
    value_chY,
    error,
    abs_error,
    rel_error
  )

MAE_norm <- mean(all_errors_output_by_tech$abs_error)
RMSE_norm  <- sqrt(mean(all_errors_output_by_tech$error^2))
TAE_norm  <- sum(all_errors_output_by_tech$abs_error)
sdAE_norm  <- sd(all_errors_output_by_tech$abs_error)
prop_error_positivo_norm  <- mean(all_errors_output_by_tech$error > 0, na.rm = TRUE)
cat("### Errors of y_2021_hist vs y_2021_pred ###\n","MAE (Mean Absolute Error):", MAE_norm, "\n",
"RMSE (Root Mean Squared Error):", RMSE_norm, "\n",
"TAE (Total Absolute Error):", TAE_norm, "\n",
"Desviación estándar del error absoluto (sdAE):", sdAE_norm, "\n",
"Proporción de errores positivos:", prop_error_positivo_norm * 100, "%\n")

if (!dir.exists("Data")) {
  dir.create("Data")
}
write.csv(all_errors_output_by_tech, 'Data/all_errors_output_by_tech_norm.csv', row.names = FALSE)

############### MAE ingenuo
### Sobre año anterior
### sacar el reference del año anterior  y hacer la misma normalización ###

df_ref_2015 <- getQuery(prj1, query) %>% filter(year == 2015,
                                                scenario == 'Reference') %>%
  filter(region %in% regions_eur) %>%
  left_join(normalize_data , by = c("region","sector", "subsector","output","technology")) %>%
  mutate(value = value * scalar, value) %>%
  select(-scalar) %>% drop_na()


scenario_2015 <- 'BaseYear2015_policy_test_noEEAtradetax'


all_errors_output_by_tech_ref2015 <- df %>%
  rename(value_chY_2021 = value) %>%
  filter(scenario == scenario_2015) %>%
  select(-scenario, -year, -Units) %>%
  left_join(df_ref_2015 %>%
              rename(value_ref_2015 = value) %>%
              select(-scenario, -year, -Units), by = c("region","sector", "subsector","output","technology")) %>%
  mutate(
    error = value_ref_2015 - value_chY_2021,
    abs_error = abs(error),
    sq_error = error^2,
    bias_ratio =  ifelse(
      value_chY_2021 == 0 & value_ref_2015 == 0,
      0,
      value_chY_2021 / value_ref_2015
    ),
    rel_error = ifelse(error == 0 & value_ref_2015 == 0, 0,error / value_ref_2015)
  ) %>% drop_na()


MAE_norm_ref2015 <- mean(all_errors_output_by_tech_ref2015$abs_error)
RMSE_norm_ref2015  <- sqrt(mean(all_errors_output_by_tech_ref2015$error^2))
TAE_norm_ref2015  <- sum(all_errors_output_by_tech_ref2015$abs_error)
sdAE_norm_ref2015  <- sd(all_errors_output_by_tech_ref2015$abs_error)
prop_error_positivo_norm_ref2015  <- mean(all_errors_output_by_tech_ref2015$error > 0, na.rm = TRUE)
cat("### Errors of y_2015_hist vs y_2021_pred  ###\n","MAE (Mean Absolute Error):", MAE_norm_ref2015, "\n",
"RMSE (Root Mean Squared Error):", RMSE_norm_ref2015, "\n",
"TAE (Total Absolute Error):", TAE_norm_ref2015, "\n",
"Desviación estándar del error absoluto (sdAE):", sdAE_norm_ref2015, "\n",
"Proporción de errores positivos:", prop_error_positivo_norm_ref2015 * 100, "%\n")

write.csv(all_errors_output_by_tech_ref2015, 'Data/all_errors_output_by_tech_norm_ref2015.csv', row.names = FALSE)

### Sobre media de años anteriores

df_ref_mean <- getQuery(prj1, query) %>%
  filter(scenario == "Reference",
         region %in% regions_eur,
         year < 2021) %>%
  group_by(
    Units, scenario, region, sector, subsector,
    technology, output
  ) %>%
  summarise(value_mean = mean(value)) %>%
  ungroup() %>%
  left_join(normalize_data , by = c("region","sector", "subsector","output","technology")) %>%
  mutate(value_mean = value_mean * scalar, value_mean) %>%
  select(-scalar) %>% drop_na()


scenario_mean <- 'BaseYear2015_policy_test_noEEAtradetax'


all_errors_output_by_tech_refmean <- df %>%
  rename(value_chY_2021 = value) %>%
  filter(scenario == scenario_mean) %>%
  select(-scenario, -Units) %>%
  left_join(df_ref_mean %>%
              rename(value_ref_mean = value_mean) %>%
              select(-scenario, -Units), by = c("region","sector", "subsector","output","technology")) %>%
  mutate(
    error = value_ref_mean - value_chY_2021,
    abs_error = abs(error),
    sq_error = error^2,
    bias_ratio =  ifelse(
      value_chY_2021 == 0 & value_ref_mean == 0,
      0,
      value_chY_2021 / value_ref_mean
    ),
    rel_error = ifelse(error == 0 & value_ref_mean == 0, 0,error / value_ref_mean)
  ) %>% drop_na()


MAE_norm_refmean <- mean(all_errors_output_by_tech_refmean$abs_error)
RMSE_norm_refmean  <- sqrt(mean(all_errors_output_by_tech_refmean$error^2))
TAE_norm_refmean  <- sum(all_errors_output_by_tech_refmean$abs_error)
sdAE_norm_refmean  <- sd(all_errors_output_by_tech_refmean$abs_error)
prop_error_positivo_norm_refmean  <- mean(all_errors_output_by_tech_refmean$error > 0, na.rm = TRUE)
cat("### Errors of y_mean_hist vs y_2021_pred ###\n",
    "MAE (Mean Absolute Error):", MAE_norm_refmean, "\n",
    "RMSE (Root Mean Squared Error):", RMSE_norm_refmean, "\n",
    "TAE (Total Absolute Error):", TAE_norm_refmean, "\n",
    "Desviación estándar del error absoluto (sdAE):", sdAE_norm_refmean, "\n",
    "Proporción de errores positivos:", prop_error_positivo_norm_refmean * 100, "%\n")

write.csv(all_errors_output_by_tech_refmean, 'Data/all_errors_output_by_tech_norm_refmean.csv', row.names = FALSE)





#
# ### Without normalize ###
#
#
# df <- getQuery(prj1, query) %>% filter(year == 2021) %>%
#   filter(region %in% regions_eur)
#
# scenario_2015 <- 'BaseYear2015_policy_test_noEEAtradetax'
#
# # separar escenarios
# df_ref <- df %>%
#   filter(scenario == "Reference") %>%
#   select(-scenario) %>%
#   rename(value_ref = value)
#
# df_chY <- df %>%
#   filter(scenario == scenario_2015) %>%
#   select(-scenario) %>%
#   rename(value_chY = value)
#
# df_comp <- df_ref %>%
#   inner_join(df_chY, by = key_cols)
#
# # error base
# df_comp <- df_comp %>%
#   mutate(
#     error = value_ref - value_chY,
#     abs_error = abs(error),
#     sq_error = error^2,
#     bias_ratio =  ifelse(
#       value_chY == 0 & value_ref == 0,
#       0,
#       value_chY / value_ref
#     ),
#     rel_error = ifelse(error == 0 & value_ref == 0, 0,error / value_ref),
#     query = query
#   )
#
# all_errors_output_by_tech <- df_comp %>%
#   select(
#     region,
#     sector,
#     subsector,
#     technology,
#     output,
#     value_ref,
#     value_chY,
#     error,
#     abs_error,
#     rel_error
#   )
#
# MAE <- mean(all_errors_output_by_tech$abs_error)
# RMSE <- sqrt(mean(all_errors_output_by_tech$error^2))
# TAE <- sum(all_errors_output_by_tech$abs_error)
# sdAE <- sd(all_errors_output_by_tech$abs_error)
# prop_error_positivo <- mean(all_errors_output_by_tech$error > 0, na.rm = TRUE)
# cat("MAE (Mean Absolute Error):", MAE, "\n")
# cat("RMSE (Root Mean Squared Error):", RMSE, "\n")
# cat("TAE (Total Absolute Error):", TAE, "\n")
# cat("Desviación estándar del error absoluto (sdAE):", sdAE, "\n")
# cat("Proporción de errores positivos:", prop_error_positivo * 100, "%\n")
#
# if (!dir.exists("Data")) {
#   dir.create("Data")
# }
# write.csv(all_errors_output_by_tech, 'Data/all_errors_output_by_tech_withoutNorm.csv', row.names = FALSE)
#
#
