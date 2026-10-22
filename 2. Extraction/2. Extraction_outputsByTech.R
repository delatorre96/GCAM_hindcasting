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





### normalizing ###

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
cat("MAE (Mean Absolute Error):", MAE_norm, "\n")
cat("RMSE (Root Mean Squared Error):", RMSE_norm, "\n")
cat("TAE (Total Absolute Error):", TAE_norm, "\n")
cat("Desviación estándar del error absoluto (sdAE):", sdAE_norm, "\n")
cat("Proporción de errores positivos:", prop_error_positivo_norm * 100, "%\n")

if (!dir.exists("Data")) {
  dir.create("Data")
}
write.csv(all_errors_output_by_tech, 'Data/all_errors_output_by_tech_norm.csv', row.names = FALSE)


### Without normalize ###


df <- getQuery(prj1, query) %>% filter(year == 2021) %>%
  filter(region %in% regions_eur)

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

MAE <- mean(all_errors_output_by_tech$abs_error)
RMSE <- sqrt(mean(all_errors_output_by_tech$error^2))
TAE <- sum(all_errors_output_by_tech$abs_error)
sdAE <- sd(all_errors_output_by_tech$abs_error)
prop_error_positivo <- mean(all_errors_output_by_tech$error > 0, na.rm = TRUE)
cat("MAE (Mean Absolute Error):", MAE, "\n")
cat("RMSE (Root Mean Squared Error):", RMSE, "\n")
cat("TAE (Total Absolute Error):", TAE, "\n")
cat("Desviación estándar del error absoluto (sdAE):", sdAE, "\n")
cat("Proporción de errores positivos:", prop_error_positivo * 100, "%\n")

if (!dir.exists("Data")) {
  dir.create("Data")
}
write.csv(all_errors_output_by_tech, 'Data/all_errors_output_by_tech_withoutNorm.csv', row.names = FALSE)



######Using scale #####

#
#
# df <- getQuery(prj1, query) %>% filter(year == 2021) %>%
#   filter(region %in% regions_eur)
#
#
# scenario_2015 <- 'BaseYear2015_policy_test_noEEAtradetax'
#
# # separar escenarios
# df_ref <- df %>%
#   filter(scenario == "Reference") %>%
#   select(-scenario) %>%
#   rename(value_ref = value) %>%
#   mutate(value_ref = scale(value_ref))
#
# df_chY <- df %>%
#   filter(scenario == scenario_2015) %>%
#   select(-scenario) %>%
#   rename(value_chY = value) %>%
#   mutate(value_chY = scale(value_chY))
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
# MAE_std <- mean(all_errors_output_by_tech$abs_error)
# RMSE_std <- sqrt(mean(all_errors_output_by_tech$error^2))
# TAE_std <- sum(all_errors_output_by_tech$abs_error)
# sdAE_std <- sd(all_errors_output_by_tech$abs_error)
# prop_error_positivo <- mean(all_errors_output_by_tech$error > 0, na.rm = TRUE)
# cat("SMAE (Standarized Mean Absolute Error):", MAE_std, "\n")
# cat("SRMSE (Standarized Root Mean Squared Error):", RMSE_std, "\n")
# cat("STAE (Standarized Total Absolute Error):", TAE_std, "\n")
# cat("Desviación estándar del error absoluto (sdAE):", sdAE_std, "\n")
# cat("Proporción de errores positivos:", prop_error_positivo * 100, "%\n")
#
# if (!dir.exists("Data")) {
#   dir.create("Data")
# }
# # write.csv(all_errors_output_by_tech, 'Data/all_errors_output_by_tech_standar.csv', row.names = FALSE)
#
