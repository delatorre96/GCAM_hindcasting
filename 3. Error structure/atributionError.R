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


query_errors <- read.csv('Data/query_errors.csv')

### nivel 1
query_weights <- query_errors %>%
  mutate(
    w_query = RMSE / sum(RMSE)
  )

### nivel 2

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
    attribution = (df$share)
  )
}

df_query <- df_long %>%
  filter(query == "outputs by sector",
         abs_error > 1) %>%
  select(region, variable, val, abs_error) %>%
  mutate(across(where(is.character), as.factor))

df_summary <- bind_rows(
  calculate_summary(df_query, "region"),
  calculate_summary(df_query, "val"),
  calculate_summary(df_query, c("region", "val"))
)

