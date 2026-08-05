library(dplyr)
library(tidyr)
library(readr)

#simulation data
sim <- read_csv('simulation_log.csv',
                col_types = cols(
                  run_id = col_character()
                ))

#Reference
all_errors_output_by_tech <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% 
  select(-query, -year, -rel_error, -value_chY, -error, -abs_error)

#inputs
df_params <- read_csv(
  "Data/inputs/df_params.csv",
  col_types = cols(
    run_id = col_character()
  )
) %>%
  pivot_wider(
    id_cols = c(region, supplysector, subsector, nesting_subsector),
    names_from = run_id,
    values_from = logit,
    values_fn = dplyr::first
  )




#outputs
outputs_by_tech <- read.csv('Data/outputs_by_tech.csv')  






outputs_by_tech_wider <- outputs_by_tech %>%
  pivot_wider(
    id_cols = c(region, sector, subsector, output, technology),
    names_from = iteration,
    values_from = X2021,
    names_prefix = "iter_"
  )

metrics <- outputs_by_tech_wider %>%
  left_join(
    all_errors_output_by_tech,
    by = c("region", "technology", "subsector", "output", "sector")
  ) %>%
  pivot_longer(
    cols = starts_with("iter_"),
    names_to = "iteration",
    values_to = "prediction"
  ) %>%
  mutate(
    error = value_ref - prediction
  ) %>%
  group_by(iteration) %>%
  summarise(
    MAE  = mean(abs(error), na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    .groups = "drop"
  )  #%>% filter(!(iteration %in% c('iter_54','iter_55', 'iter_43', 'iter_7', 'iter_47', 'iter_19', 'iter_15')))

Q3 <- quantile(metrics$MAE, 0.75)
IQR_mae <- IQR(metrics$MAE)

rmse_outliers <- metrics %>%
  filter(
    MAE > Q3 + 2 * IQR_mae
  )
iters_remove <- rmse_outliers$iteration


#Calculate error each iter
outputs_error <- outputs_by_tech_wider %>%
  left_join(all_errors_output_by_tech, by = c("region", "technology", "subsector", "output", "sector")) %>%
  mutate(
    abs(across(starts_with("iter_"), ~ (value_ref - .x)/ value_ref))
  ) %>% 
  select(-all_of(iters_remove))

####Variation
iter_cols <- grep("^iter_", names(outputs_error), value = TRUE)

variation_iter <- outputs_error %>% 
  drop_na() %>%
  select(-value_ref) %>%
  rowwise() %>%
  mutate(
    mean = mean(c_across(starts_with("iter_"))),
    sd = sd(c_across(starts_with("iter_"))),
    cv = sd / mean,
    min = min(c_across(starts_with("iter_"))),
    max = max(c_across(starts_with("iter_"))),
    range = max - min
  ) %>%
  ungroup() %>%
  select( "region", "sector", "subsector", "output", "technology", "mean",
          "sd", "cv", "min", "max", "range")


## what dimension variates more 
### Normalizar
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


