library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)

con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis/gcam_sensitivity.sqlite"
)

#See interested experiments:
experiments <- dbReadTable(con, "Experiments")
experiment_id <- "EXP_5a2e3191"

runs <- dbReadTable(con, "Runs")

datasets <- dbReadTable(con, "Datasets")

query_name <- "outputs_by_tech"



source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

inputs <- read_experiment_inputs(
  con = con
)  %>% 
  select(-xml_file, -xpath, -fillout, -year)


outputs_by_tech <- read_experiment_output(
  con = con,
  query_name = query_name
)


outputs_by_tech_wider <- outputs_by_tech %>%
  pivot_wider(
    id_cols = c(region, sector, subsector, output, technology),
    names_from = run_id,
    values_from = `2021`
  )

####### Reference #######
all_errors_output_by_tech <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% 
  select(-query, -year, -rel_error, -value_chY, -error, -abs_error)

####### metrics_per_run per run to delete outliers #######
metrics_per_run <- outputs_by_tech_wider %>%
  left_join(
    all_errors_output_by_tech,
    by = c("region", "technology", "subsector", "output", "sector")
  ) %>%
  pivot_longer(
    cols = starts_with("RUN_"),
    names_to = "run_id",
    values_to = "prediction"
  ) %>%
  mutate(
    error = value_ref - prediction
  ) %>%
  group_by(run_id) %>%
  summarise(
    MAE  = mean(abs(error), na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    .groups = "drop"
  )  


Q3 <- quantile(metrics_per_run$MAE, 0.75)
IQR_mae <- IQR(metrics_per_run$MAE)

rmse_outliers <- metrics_per_run %>%
  filter(
    MAE > Q3 + 1.5 * IQR_mae
  )
iters_remove <- rmse_outliers$run_id
metrics_per_run <- metrics_per_run %>% 
  filter(!(run_id %in% iters_remove))


## Density MAE by run (without GCAM execution errors or huge MAE)
plot(density(metrics_per_run$MAE))

### Plot Delta vs MAE

metrics_per_run <- metrics_per_run %>% 
  left_join(runs %>% select(run_id, delta))
  
plot(metrics_per_run$delta, metrics_per_run$MAE)





################## Variation summary ################## 
variation_iter <- outputs_error %>% 
  drop_na() %>%
  select(-value_ref) %>%
  rowwise() %>%
  mutate(
    mean = mean(c_across(starts_with("RUN_"))),
    sd = sd(c_across(starts_with("RUN_"))),
    cv = sd / mean,
    min = min(c_across(starts_with("RUN_"))),
    max = max(c_across(starts_with("RUN_"))),
    range = max - min
  ) %>%
  ungroup() %>%
  select( "region", "sector", "subsector", "output", "technology", "mean",
          "sd", "cv", "min", "max", "range")

############## input param with output in the same dimension ################## 


df_merge <- inputs %>% 
  rename('sector' = 'supplysector') %>%
  left_join(outputs_by_tech , by = c('region', 'sector', 'subsector', 'run_id')) %>% 
  left_join(all_errors_output_by_tech, by = c("region", "technology" ,"subsector",  "output",     "sector" ) ) %>%
  mutate(error = abs(value_ref - `2021`)) %>% select(-nesting_subsector) %>% drop_na()



df_merge_example <- df_merge %>% filter(region == 'Austria',
                                                       sector == 'regional biomass',
                                                       subsector == 'regional biomass') 
Q3 <- quantile(df_merge_example$error, 0.75)
IQR_value <- IQR(df_merge_example$error)

run_outliers <- df_merge_example %>%
  filter(
    `error` > Q3 + 10 * IQR_value
  )
iters_remove <- run_outliers$run_id       

df_merge_example <- df_merge_example %>% 
  filter(!run_id %in% iters_remove)


# Índice del error mínimo
i_min <- which.min(df_merge_example$error)

plot(
  df_merge_example$logit,
  df_merge_example$error,
  #type = "b",                # puntos y líneas
  pch = 19,                  # puntos sólidos
  col = "steelblue",
  xlab = "Logit value",
  ylab = "Error",
  main = "Error as a function of the logit parameter",
  sub = "Austria - Regional biomass"
)

# Resaltar el mínimo
points(
  df_merge_example$logit[i_min],
  df_merge_example$error[i_min],
  pch = 19,
  col = "red",
  cex = 1.5
)

text(
  df_merge_example$logit[i_min],
  df_merge_example$error[i_min],
  labels = paste("Minimum\nlogit =", round(df_merge_example$logit[i_min], 2)),
  pos = 3,
  col = "red"
)

grid()




## what dimension variates more 
### Normalize
variation_reg <- variation_iter %>%
  group_by(region) %>%
  summarise(mean_dim = mean(mean),
            sd_dim = sd(sd),
            cv_dim = sd_dim / mean_dim,
            min_dim = min(min),
            max_dim = max(max),
            range_dim = max_dim - min_dim) %>%
  arrange(desc(range_dim))

variation_sector <- variation_iter %>%
  group_by(sector) %>%
  summarise(mean_dim = mean(mean),
            sd_dim = sd(sd),
            cv_dim = sd_dim / mean_dim,
            min_dim = min(min),
            max_dim = max(max),
            range_dim = max_dim - min_dim) %>%
  arrange(desc(range_dim))

variation_subsector<- variation_iter %>%
  group_by(subsector) %>%
  summarise(mean_dim = mean(mean),
            sd_dim = sd(sd),
            cv_dim = sd_dim / mean_dim,
            min_dim = min(min),
            max_dim = max(max),
            range_dim = max_dim - min_dim) %>%
  arrange(desc(range_dim))

variation_output<- variation_iter %>%
  group_by(output) %>%
  summarise(mean_dim = mean(mean),
            sd_dim = sd(sd),
            cv_dim = sd_dim / mean_dim,
            min_dim = min(min),
            max_dim = max(max),
            range_dim = max_dim - min_dim) %>%
  arrange(desc(range_dim))

variation_technology<- variation_iter %>%
  group_by(technology) %>%
  summarise(mean_dim = mean(mean),
            sd_dim = sd(sd),
            cv_dim = mean_dim / mean_dim,
            min_dim = min(min),
            max_dim = max(max),
            range_dim = max_dim - min_dim) %>%
  arrange(desc(range_dim))




