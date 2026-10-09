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
  select(query, value_ref, error) %>%
  group_by(query) %>%
  summarise(
    RMSE = sqrt(mean(error^2)),
    MAE = mean(abs(error)),
    MaxAE = max(abs(error)),
    MeanRef = mean(abs(value_ref)),

    NRMSE = if_else(
      MeanRef == 0,
      RMSE,
      RMSE / MeanRef
    ),

    NMAE = if_else(
      MeanRef == 0,
      MAE,
      MAE / MeanRef
    ),

    NMaxAE = if_else(
      MeanRef == 0,
      MaxAE,
      MaxAE / MeanRef
    ),

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
  arrange(desc(NRMSE)) %>%
  mutate(NRMSE_cumsum =  cumsum(NRMSE)/sum(NRMSE))


#### Those queries that has low error can be deleted
min_max_error  <- 1000
min_mean_error <- 80

worst_query_errors <- query_errors %>%
  filter(
    NMaxAE > min_max_error |
      NMAE > min_mean_error
  )

#### Those queries that has high  variability (RMSE>>MAE) has to be explore what explains the total error query
#
# query_errors_highVar <- query_errors %>%
#   filter(ratio_RMSE_MAE >= 1.7)

#### After this filters, we can now study each query

if (!dir.exists("Data")) {
  dir.create("Data")
}
write.csv(query_errors, "Data/query_errors.csv", row.names = FALSE)

# 
# for (query_i in unique(worst_query_errors$query)) {
#   query_errors <- all_errors[all_errors$query == query_i, ] %>%
#     select(where(~ !all(is.na(.)))) %>%
#     write.csv(paste0('Data/',query_i,'.csv'))
# }

write.csv(worst_query_errors, "Data/worst_query_errors.csv", row.names = FALSE)




