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


worst_query_errors <- read.csv('Data/worst_query_errors.csv')

#### Determine if the consultation has more variability by region or by other factors ####

################## Shares ####################
# df_profit_rate <- df_long %>%
#   filter(query == "outputs by sector",
#          abs_error > 1) %>%
#   select(region, variable, val, abs_error) %>%
#   mutate(across(where(is.character), as.factor))
#
# calculate_shares <- function (df_query, by){
#   error_share <- df_query  %>%
#     group_by(get(by)) %>%
#     summarise(
#       total_abs_error = sum(abs_error),
#       n = n(),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       share = total_abs_error / sum(total_abs_error)
#     ) %>%
#     arrange( desc(share)) %>%
#     mutate(cum_share = cumsum(share))
# }
#
# error_val_share <- calculate_shares (df_profit_rate, 'val')
# error_region_share <- calculate_shares(df_profit_rate, 'region')
# error_variable_share <-  calculate_shares(df_profit_rate, 'variable')
#
# library(ineq)
#
# N80_val <- which(error_val_share$cum_share >= 0.8)[1]
# N80_variable <- which(error_variable_share$cum_share >= 0.8)[1]
# N80_reg <- which(error_region_share$cum_share >= 0.8)[1]
# gini_val <-Gini(error_val_share$share)
# gini_variable <- Gini(error_region_share$share)
# gini_reg <- Gini(error_variable_share$share)
################################################

calculate_summary <- function(df_query, by){

  df <- df_query %>%
    group_by(across(all_of(by))) %>%
    summarise(
      total_abs_error = sum(abs_error),
      .groups = "drop"
    ) %>%
    arrange(desc(total_abs_error)) %>%
    mutate(
      share = total_abs_error / sum(total_abs_error),
      cum_share = cumsum(share)
    )

  tibble(
    dimension = paste(by, collapse = " × "),
    n_categories = nrow(df),
    total_error = sum(df$total_abs_error),
    gini = Gini(df$share),
    top1 = df$cum_share[1],
    top5 = df$cum_share[min(5, nrow(df))],
    top10 = df$cum_share[min(10, nrow(df))],
    N80 = which(df$cum_share >= 0.8)[1],
    N80_prop = which(df$cum_share >= 0.8)[1] / nrow(df)
  )
}

df_summary_all <- data.frame()

for (query in unique(worst_query_errors$query)) {

  df_query <- df_long %>%
    filter(
      query == !!query,
      abs_error > 1
    ) %>%
    select(region, variable, val, abs_error) %>%
    mutate(across(where(is.character), as.factor))


  df_summary <- bind_rows(
    calculate_summary(df_query, "region"),
    calculate_summary(df_query, "val"),
    calculate_summary(df_query, c("region", "val"))
  ) %>%
    mutate(query = query, .before = 1)

  df_summary_all <- bind_rows(df_summary_all, df_summary) %>%
    arrange( N80)
}

############## Querys with specific regions

################# costs by subsector ###################

regions_costs_by_subsector <- df_long %>%
  filter(query == "costs by subsector") %>%
  select(query, region, value_ref, error) %>%
  group_by(region) %>%
  summarise(
    RMSE = sqrt(mean(error^2)),
    MAE = mean(abs(error)),

    ratio_RMSE_MAE = RMSE / MAE,
    .groups = "drop"
  ) %>%
  arrange(desc(RMSE)) %>%
  filter(RMSE > mean(RMSE)) %>% select(region)



################# costs by subsector ###################

regions_prices_by_sector <- df_long %>%
  filter(query == "prices by sector") %>%
  select(query, region, value_ref, error) %>%
  group_by(region) %>%
  summarise(
    RMSE = sqrt(mean(error^2)),
    MAE = mean(abs(error)),

    ratio_RMSE_MAE = RMSE / MAE,
    .groups = "drop"
  ) %>%
  arrange(desc(RMSE)) %>%
  filter(RMSE > mean(RMSE))  %>% select(region)


###### FINAL

querys_to_take <- worst_query_errors %>%
  select(query) %>%
  mutate(
    regions = case_when(
      query == "costs by subsector" ~ paste(regions_costs_by_subsector$region, collapse = ", "),
      query == "prices by sector" ~ paste(regions_prices_by_sector$region, collapse = ", "),
      TRUE ~ "All"
    )
  )

if (!dir.exists("Data")) {
  dir.create("Data")
}


write.csv(querys_to_take, "Data/querys_to_take.csv", row.names = FALSE)


############## Querys with specific vals

#
# df_prices_by_sector <- df_long %>%
#   filter(query == "prices by sector") %>%
#   select(query, val, value_ref, error) %>%
#   group_by(val) %>%
#   summarise(
#     RMSE = sqrt(mean(error^2)),
#     MAE = mean(abs(error)),
#
#     ratio_RMSE_MAE = RMSE / MAE,
#     .groups = "drop"
#   ) %>%
#   arrange(desc(RMSE))  %>%
#   filter(RMSE > mean(RMSE)) %>% select(region)
#


