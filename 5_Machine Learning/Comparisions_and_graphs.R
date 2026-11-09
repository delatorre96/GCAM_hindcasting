library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ranger)
source("../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")


con <- dbConnect(
  SQLite(),
  "../GCAM_sensitivity_analysis/gcam_sensitivity.sqlite"
)

query_name <- "outputs_by_tech"

sql <- "
  SELECT
    d.filepath,
    d.run_id,
    r.experiment_id
  FROM Datasets d
  INNER JOIN Runs r
    ON d.run_id = r.run_id
  WHERE d.dataset_type = 'output'
    AND d.dataset_name = ?
    AND r.execution_errors = 0
"

datasets <- DBI::dbGetQuery(
  con,
  sql,
  params = list(query_name)
)

outputs_by_tech <- purrr::map2_dfr(
  datasets$filepath,
  datasets$experiment_id,
  function(filepath, experiment_id) {
    arrow::read_parquet(filepath) |>
      dplyr::mutate(experiment_id = experiment_id)
  }
)


####### Reference #######
ref_values_withoutPolicy <- read.csv('2. Extraction/Data/all_errors_output_by_tech_withoutPolicy.csv')  %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(region %in% unique(outputs_by_tech$region))
ref_values_with_policy <- read.csv('2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(region %in% unique(outputs_by_tech$region))
### Calculate errors ###
all_errors_output_by_tech <- outputs_by_tech %>%
  left_join(ref_values_withoutPolicy, by = c('region', 'technology', 'subsector', 'output', 'sector'))   %>%
  filter(!is.na(value_ref) & !is.na(`2021`)) %>%
  mutate(error_abs = abs(value_ref -`2021`),
         error = value_ref -`2021`)%>%
  select(-`2021`, -value_ref)

################ Delete run outliers ################
#errors per run#
error_by_run <- all_errors_output_by_tech %>%
  group_by(run_id) %>%
  summarise(MAE = mean(error_abs),
            RMSE = sqrt(mean(error^2)),
            MAE_log = log1p(MAE),
            RMSE_log = log1p(RMSE))
cat(nrow(error_by_run))

MAE_ <- error_by_run %>% select(run_id, MAE)
Q1 <- quantile(error_by_run$MAE, 0.25, na.rm = TRUE)
Q3 <- quantile(error_by_run$MAE, 0.75, na.rm = TRUE)

IQR_val <- Q3 - Q1

upper_limit <- Q3 +  20*IQR_val

error_by_run <- error_by_run %>%
  filter(MAE <= upper_limit)


cat(nrow(error_by_run))

################ Join Errors_by_run to reference hindcasting errors and experiment_id ################

runs <- dbReadTable(con, "Runs")

##Ref hindcasting errors
MAE_ref_withPolicy <- mean(ref_values_with_policy$abs_error)
RMSE_ref_withPolicy <-  sqrt(mean((ref_values_with_policy$error)^2))
MAE_log_ref_withPolicy <- log1p(MAE_ref_withPolicy)
RMSE_log_ref_withPolicy <- log1p(RMSE_ref_withPolicy)


MAE_ref_withoutPolicy <- mean(ref_values_withoutPolicy$abs_error)
RMSE_ref_withoutPolicy <-  sqrt(mean((ref_values_withoutPolicy$error)^2))
MAE_log_ref_withoutPolicy <- log1p(MAE_ref_withoutPolicy)
RMSE_log_ref_withoutPolicy <- log1p(RMSE_ref_withoutPolicy)

## Join
error_by_run <- error_by_run %>%
  mutate(MAE_ref = MAE_ref_withoutPolicy,
         RMSE_ref = RMSE_ref_withoutPolicy,
         MAE_dec = (MAE - MAE_ref) / MAE_ref,
         RMSE_ref_dec =  (RMSE - RMSE_ref) / RMSE_ref) %>%
  left_join (runs, by = 'run_id')





experiments <- dbReadTable(con, "Experiments")
#EXP_5a2e3191 -> delta
#EXP_2e96286f -> monte carlo logits (aditive)
#EXP_07b59f0c -> latin hypercube delta
#EXP_89083094 -> monte carlo satiation level (aditive)
#EXP_9e51df1d -> monte carlo price-elaticity (aditive)
#EXP_aa55ea10 -> monte carlo price-elas, satiation and logits
#EXP_4962b73b -> monte carlo price-elas, satiation and logits

error_by_run_logit_ranomdelta <- error_by_run %>%
  filter(experiment_id %in% c('EXP_5a2e3191'))

error_by_run_logit_cubedelta <- error_by_run %>%
  filter(experiment_id %in% c('EXP_07b59f0c'))

error_by_run_sat_delta <- error_by_run %>%
  filter(experiment_id %in% c('EXP_89083094'))

error_by_run_logit_monteCarlo <- error_by_run %>%
  filter(experiment_id %in% c('EXP_2e96286f'))

error_by_run_price_monteCarlo <- error_by_run %>%
  filter(experiment_id %in% c('EXP_9e51df1d'))

error_by_run_together <- error_by_run %>%
  filter(experiment_id %in% c('EXP_2e96286f', 'EXP_89083094','EXP_9e51df1d','EXP_5a2e3191', 'EXP_07b59f0c'))

error_by_run_optimazed_monteCarlo <- error_by_run %>%
  filter(experiment_id %in% c('EXP_844fcd55'))

############## Plots ##############



##################### Delta #####################

####### Random #######
###
error_by_run_logit_ranomdelta <- error_by_run_logit_ranomdelta %>% filter(MAE < 0.6)
plot(error_by_run_logit_ranomdelta$MAE,
     ylab = "MAE",
     xlab = "Run",
     main = "Variación simultánea aleatoria con hiperparámetro delta sobre logits")

abline(h = MAE_ref, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = paste("MAE de referencia =", round(MAE_ref, 3)),
       col = "red",
       lwd = 2,
       lty = 2)
## Variación de delta sobre MAE

plot(error_by_run_logit_ranomdelta$delta, error_by_run_logit_ranomdelta$MAE,
     ylab = "MAE",
     xlab = "Delta",
     main = "Variación de delta sobre MAE")
abline(h = MAE_ref, col = "red", lwd = 2, lty = 2)
legend("topright",
       legend = paste("MAE de referencia =", round(MAE_ref, 3)),
       col = "red",
       lwd = 2,
       lty = 2)

hist(
  error_by_run_logit_ranomdelta$delta,
  breaks = 100,
  main = "Histograma de valores de delta",
  xlab = "Delta",
  ylab = "Frecuencia",
  col = "lightblue",
  border = "white"
)

### Satitation ###

error_by_run_sat_delta <- error_by_run_sat_delta %>% filter(MAE < 0.6)
plot(error_by_run_sat_delta$MAE,
     ylab = "MAE",
     xlab = "Run",
     main = "Variación simultánea aleatoria con hiperparámetro delta sobre Satitation level")

abline(h = MAE_ref, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = paste("MAE de referencia =", round(MAE_ref, 3)),
       col = "red",
       lwd = 2,
       lty = 2)
## Variación de delta sobre MAE

plot(error_by_run_sat_delta$delta, error_by_run_sat_delta$MAE,
     ylab = "MAE",
     xlab = "Delta",
     main = "Variación de delta sobre MAE")
abline(h = MAE_ref, col = "red", lwd = 2, lty = 2)
legend("topright",
       legend = paste("MAE de referencia =", round(MAE_ref, 3)),
       col = "red",
       lwd = 2,
       lty = 2)

hist(
  error_by_run_logit_ranomdelta$delta,
  breaks = 100,
  main = "Histograma de valores de delta",
  xlab = "Delta",
  ylab = "Frecuencia",
  col = "lightblue",
  border = "white"
)


##################### Monte Carlo #####################
###  logits
error_by_run_logit_monteCarlo <- error_by_run_logit_monteCarlo %>% filter (MAE < 1)
plot(error_by_run_logit_monteCarlo$MAE,
     ylab = "MAE",
     xlab = "Run",
     main = "Variación en logits Monte Carlo")

abline(h = MAE_ref, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = paste("MAE de referencia =", round(MAE_ref, 3)),
       col = "red",
       lwd = 2,
       lty = 2)


### Monte Carlo price elasticity
plot(error_by_run_price_monteCarlo$MAE,
     ylab = "MAE",
     xlab = "Run",
     main = "Variación en price elasticity por Monte Carlo")

abline(h = MAE_ref, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = paste("MAE de referencia =", round(MAE_ref, 3)),
       col = "red",
       lwd = 2,
       lty = 2)

### All
MAE_min <- min(error_by_run_together$MAE, na.rm = TRUE)
MAE_max <- max(error_by_run_together$MAE, na.rm = TRUE)

#----- gráfico -----

plot(error_by_run_together$MAE,
     ylab = "MAE",
     xlab = "Run",
     main = "MAE por simulación: todas",
     pch = 16)

abline(h = MAE_ref, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = c(
         paste("MAE de referencia =", round(MAE_ref, 3)),
         paste("MAE mínimo =", round(MAE_min, 3)),
         paste("MAE máximo =", round(MAE_max, 3))
       ),
       col = c("red", "yellow", "green"),
       lwd = c(2, NA, NA),
       lty = c(2, NA, NA),
       pch = c(NA, 16, 16),
       bty = "n")

i_min <- which.min(error_by_run_together$MAE)
i_max <- which.max(error_by_run_together$MAE)

points(i_min, MAE_min, col = "yellow", pch = 19, cex = 1.3)
points(i_max, MAE_max, col = "green", pch = 19, cex = 1.3)

### Optimazed

#----- calcular minimo y maximo -----

MAE_min <- min(error_by_run_optimazed_monteCarlo$MAE, na.rm = TRUE)
MAE_max <- max(error_by_run_optimazed_monteCarlo$MAE, na.rm = TRUE)

i_min <- which.min(error_by_run_optimazed_monteCarlo$MAE)
i_max <- which.max(error_by_run_optimazed_monteCarlo$MAE)

#----- grafico -----

plot(error_by_run_optimazed_monteCarlo$MAE,
     ylab = "MAE",
     xlab = "Run",
     main = "MAE tras derivar sobre algoritmo de ML",
     ylim = c(0.4376, 0.4589),
     pch = 16)

abline(h = round(MAE_ref, 4), col = "red", lwd = 2, lty = 2)

#----- marcar minimo y maximo -----

points(i_min, MAE_min, col = "blue", pch = 19, cex = 1.3)
points(i_max, MAE_max, col = "darkgreen", pch = 19, cex = 1.3)


legend("right",
       legend = c(
         paste("MAE de referencia =", round(MAE_ref, 4)),
         paste("MAE mínimo =", round(MAE_min, 5)),
         paste("MAE máximo =", round(MAE_max, 5))
       ),
       col = c("red", "blue", "darkgreen"),
       lwd = c(2, NA, NA),
       lty = c(2, NA, NA),
       pch = c(NA, 19, 19),
       bty = "n")

incremento_MAE_min_pct <- (MAE_min - MAE_ref) / MAE_ref
incremento_MAE_min_pct

df_mostSensParams <- read_csv('4_SensitivityAnalysis/df_mostSensParams.csv')


#### Plot about simulations ####
optimazed_input_combination <- read_csv('df_params_new_inputs_GAM.csv')
inputs <- optimazed_input_combination %>% select(xml_file, region, xpath,type_of_param) %>% distinct()
input_2 <- optimazed_input_combination %>%
  filter(input_id == 2)
plot(density(input_2$param))



###### Experiments MonteCarlo total reinforcement ######



con <- dbConnect(
  SQLite(),
  "../Sensitivity-Analysis-reinforce/gcam_sensitivity.sqlite"
)


experiments <- dbReadTable(con, "Experiments")
runs <- dbReadTable(con, "Runs")




monteCarloExp <- read_experiment_output(con = con,
                                        query_name =  "outputs_by_tech",
                                        experiment_id = 'EXP_e5d0e3b2',
                                        execution_errors = TRUE)  %>%
  select('region', 'technology', 'subsector', 'output', 'sector', '2021', 'run_id') %>%
  left_join(ref_values_withoutPolicy, by = c('region', 'technology', 'subsector', 'output', 'sector'))   %>%
  filter(!is.na(value_ref) & !is.na(`2021`)) %>%
  mutate(error_abs = abs(value_ref -`2021`),
         error = value_ref -`2021`)%>%
  select(-`2021`, -value_ref)


error_by_run_MC <- monteCarloExp %>%
  group_by(run_id) %>%
  summarise(MAE = mean(error_abs),
            RMSE = sqrt(mean(error^2)),
            MAE_log = log1p(MAE),
            RMSE_log = log1p(RMSE))
