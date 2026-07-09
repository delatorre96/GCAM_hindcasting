library(dplyr)
library(tidyr)

library(randomForest)

all_errors <- read.csv('../2. Extraction/Data/all_errors.csv')
key_names <- c("query","region","year","value_ref", "value_chY","error","abs_error","rel_error")
vars <- setdiff(names(all_errors), key_names)
df_long <- all_errors %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "val",
    values_drop_na = TRUE
  )


query_errors_highVar <- read.csv('Data/query_errors_highVar.csv')

#### Determine if the consultation has more variability by region or by other factors ####

df_profit_rate <- df_long %>%
  filter(query == "profit rate",
         abs_error > 1) %>%
  select(region, variable, val, abs_error, error)

df_region <- df_profit_rate %>% group_by(region) %>%
  summarise(
    RMSE = sqrt(mean(error^2)),
    MAE = mean(abs(error)),
    ratio_RMSE_MAE = RMSE / MAE,
    .groups = "drop"
  )

df_val <- df_profit_rate %>% group_by(val) %>%
  summarise(
    RMSE = sqrt(mean(error^2)),
    MAE = mean(abs(error)),
    ratio_RMSE_MAE = RMSE / MAE,
    .groups = "drop"
  )
df_variable <- df_profit_rate %>% group_by(variable) %>%
  summarise(
    RMSE = sqrt(mean(error^2)),
    MAE = mean(abs(error)),
    ratio_RMSE_MAE = RMSE / MAE,
    .groups = "drop"
  )









##################### profit rate ####################
df_profit_rate <- df_long %>%
  filter(query == "profit rate",
         abs_error > 1) %>%
  select(region, variable, val, abs_error)

df_profit_rate <- df_profit_rate %>%
  mutate(across(where(is.character), as.factor))


error_val_share <- df_profit_rate %>%
  group_by(val) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))

error_region_share <- df_profit_rate %>%
  group_by(region) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))


error_variable_share <- df_profit_rate %>%
  group_by(variable) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))

################# costs by tech ###################

df_costs_by_tech<- df_long %>%
  filter(query == "costs by tech",
         abs_error > 1) %>%
  select(region, variable, val, abs_error)


error_val_share <- df_costs_by_tech %>%
  group_by(val) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))

error_region_share <- df_costs_by_tech %>%
  group_by(region) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))


error_variable_share <- df_costs_by_tech %>%
  group_by(variable) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))




############# prices by sector #################

df_prices <- df_long %>%
  filter(query == "prices by sector",
         abs_error > 1) %>%
  select(region, variable, val, abs_error)


error_val_share <- df_prices %>%
  group_by(val) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))

error_region_share <- df_prices %>%
  group_by(region) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))


error_variable_share <- df_prices %>%
  group_by(variable) %>%
  summarise(
    total_abs_error = sum(abs_error),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = total_abs_error / sum(total_abs_error)
  ) %>%
  arrange( desc(share))






