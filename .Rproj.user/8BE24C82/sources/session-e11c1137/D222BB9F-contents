library(dplyr)
library(tidyr)

all_errors_output_by_tech <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% 
  select(-query, -year, -rel_error)

error_summary_by_criteria <- function(df_all_errors, criteria) {
  
  df_all_errors %>%
    group_by(.data[[criteria]]) %>%
    summarise(
      RMSE = sqrt(mean(error^2, na.rm = TRUE)),
      MAE = mean(abs(error), na.rm = TRUE),
      MaxAE = max(abs(error), na.rm = TRUE),
      MeanRef = mean(abs(value_ref), na.rm = TRUE),
      
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
      
      .groups = "drop"
    ) %>%
    arrange(desc(NRMSE)) %>%
    mutate(
      NRMSE_share = NRMSE / sum(NRMSE),
      NRMSE_cumsum = cumsum(NRMSE_share)
    ) %>%
    select(-RMSE, -MAE, -MaxAE, -MeanRef)
}

key_names <- c("year","value_ref", "value_chY","error","abs_error","rel_error")
vars <- setdiff(names(all_errors_output_by_tech), key_names)

all_errors_summaries <-
  setNames(
    lapply(vars, \(x) error_summary_by_criteria(all_errors_output_by_tech, x)),
    vars
  )


output_dir <- "Data"

for (name in names(all_errors_summaries)) {
  write.csv(
    all_errors_summaries[[name]],
    file = file.path(output_dir,
                     paste0("all_errors_output_by_tech_", name, ".csv")),
    row.names = FALSE
  )
}




