library(dplyr)
library(tidyr)

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

#### Per Query ####


query_errors <- df_long %>%
  select(query, error) %>%
  group_by(query) %>%
  summarise(
    RMSE = sqrt(mean(error^2)),
    MAE = mean(abs(error)),
    ratio_RMSE_MAE = RMSE / MAE,
    .groups = "drop"
  ) %>%
  mutate(
    error_profile = case_when(
      ratio_RMSE_MAE <= 1.1 ~ "Very uniform",
      ratio_RMSE_MAE <= 1.3 ~ "Uniform",
      ratio_RMSE_MAE <= 1.7 ~ "Moderate variability",
      ratio_RMSE_MAE <= 2.5 ~ "High variability",
      TRUE ~ "Extreme outliers"
    )
  ) %>%
  arrange(desc(RMSE))

#### Those queries that has low error can be deleted
min_error <- 1
query_errors <- query_errors %>%
  filter(RMSE > min_error)

#### Those queries that has high  variability (RMSE>>MAE) has to be explore what explains the total error query

query_errors_highVar <- query_errors %>%
  filter(ratio_RMSE_MAE >= 1.7)

#### After this filters, we can now study each query

if (!dir.exists("Data")) {
  dir.create("Data")
}


write.csv(query_errors, "Data/query_errors.csv", row.names = FALSE)
write.csv(query_errors_highVar, "Data/query_errors_highVar.csv", row.names = FALSE)




