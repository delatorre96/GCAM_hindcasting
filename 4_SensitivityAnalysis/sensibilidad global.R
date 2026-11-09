library(RSQLite)
library(dplyr)
library(tidyr)
library(arrow)

source("mc_link_sensitivity.R")

source("../../GCAM_sensitivity_analysis/R/Storage/explore_dataBase.R")

con <- dbConnect(
  SQLite(),
  "../../GCAM_sensitivity_analysis_dipc/gcam_sensitivity.sqlite"
)


###### Load input Runs #######
query_name <- "outputs_by_tech"
experiment_id <- 'EXP_02a66a90'
inputs_run <- read_experiment_inputs(con,
                                     experiment_id,
                                     execution_errors = NULL)


all_errors_output_by_tech <- read_parquet('all_errors_output_by_tech.parquet') %>% filter(Units %in% c('EJ', 'Mt N', 'Mt NH3'))



relevance <- read_csv("create_dfParams/df_allParams_relevance.csv")
ptab <- mc_param_table(inputs_run, relevance)

ptab_ <- ptab %>%
  select(type_of_param, param, region, param_default, run_id, supplysector, subsector, demand_sector, input_name_csv) %>%
  rename('sector' = 'supplysector')

params_outputs <- all_errors_output_by_tech %>%
  inner_join(ptab_, by = c("region", "sector","subsector", "run_id"))


most_var <- params_outputs %>%
  group_by(region, sector, subsector, output, technology) %>%
  summarise(
    variance = sd(param, na.rm = TRUE),
    .groups = "drop"
  )

region = 'Spain'
sector = 'agricultural energy use'
subsector = 'stationary'
output = 'agricultural energy use'
technology = 'biomass'

params_outputs_most_var <- params_outputs %>%
  filter (region == 'Spain',
          sector == 'agricultural energy use',
          subsector == 'stationary',
          output == 'agricultural energy use',
          technology == 'biomass')

plot(params_outputs_most_var$param, params_outputs_most_var$value_run)

plot(params_outputs_most_var$param, params_outputs_most_var$error)












ptab <- mc_param_table(inputs_run, relevance)

ptab_ <- ptab %>%
  transmute(run_id, param_id, type_of_param, region, param, param_default,
            sector         = coalesce(supplysector, demand_sector),
            nest_subsector = ifelse(type_of_param == "logit", subsector, NA_character_),
            level          = ifelse(is.na(nest_subsector), "sector", "subsector"))

# Outputs: solo output primario
out <- all_errors_output_by_tech %>%
  filter(output == sector)


# 3. Join en dos pasadas
p_sub <- ptab_ %>% filter(level == "subsector")
p_sec <- ptab_ %>% filter(level == "sector")

params_outputs <- bind_rows(
  out %>% inner_join(p_sub, by = c("run_id", "region", "sector", "subsector" = "nest_subsector"),
                     keep = FALSE, relationship = "many-to-many") %>%
    mutate(nest_subsector = subsector),
  out %>% inner_join(p_sec, by = c("run_id", "region", "sector"),
                     relationship = "many-to-many")
) %>%
  # cuota de cada tecnología dentro del nido que controla el parámetro
  group_by(run_id, param_id) %>%
  mutate(share_run = value_run / sum(value_run),
         share_ref = value_ref / sum(value_ref)) %>%
  ungroup()






# ------------------------------------------------------------------
# 0. Diagnóstico: cuántos parámetros tocan cada tecnología (en un run)
# ------------------------------------------------------------------
r1 <- params_outputs$run_id[1]
params_outputs %>%
  filter(run_id == r1) %>%
  count(region, sector, subsector, technology, name = "n_params") %>%
  count(n_params, name = "n_tecnologias")

# ------------------------------------------------------------------
# 1. X: una columna por parámetro individual (run x param_id)
# ------------------------------------------------------------------
X <- ptab %>%
  distinct(run_id, param_id, param) %>%
  pivot_wider(names_from = param_id, values_from = param)

# ------------------------------------------------------------------
# 2. Y: output (o error) de cada tecnología en cada run
#    y_type = "output": sensibilidad del output  (igual con value_run o error)
#    y_type = "abs_error": qué parámetro puede reducir el error
# ------------------------------------------------------------------
y_type <- "output"

Y <- out %>%
  mutate(tech_key = paste(region, sector, subsector, technology, sep = "|"),
         y = if (y_type == "output") value_run else abs(value_run - value_ref)) %>%
  select(run_id, tech_key, y)

tech_ref <- out %>%
  distinct(tech_key = paste(region, sector, subsector, technology, sep = "|"), value_ref, Units)

# ------------------------------------------------------------------
# 3. Qué parámetros tocan cada tecnología (del join anterior)
# ------------------------------------------------------------------
links <- params_outputs %>%
  distinct(tech_key = paste(region, sector, subsector, technology, sep = "|"), param_id)

# ------------------------------------------------------------------
# 4. PRCC: efecto de cada parámetro descontando los demás que tocan
#    la misma tecnología
# ------------------------------------------------------------------
prcc <- function(y, X) {
  ry <- rank(y)
  RX <- apply(X, 2, rank)
  if (ncol(RX) == 1) return(cor(ry, RX[, 1]))
  sapply(seq_len(ncol(RX)), function(j) {
    ey <- resid(lm(ry ~ RX[, -j]))
    ex <- resid(lm(RX[, j] ~ RX[, -j]))
    cor(ey, ex)
  })
}

Ys <- split(Y[c("run_id", "y")], Y$tech_key)
Ls <- split(links$param_id, links$tech_key)

sens_tech <- bind_rows(lapply(names(Ls), function(tk) {
  d <- Ys[[tk]]
  if (is.null(d)) return(NULL)
  d  <- inner_join(d, X[c("run_id", Ls[[tk]])], by = "run_id")
  xm <- as.matrix(d[Ls[[tk]]])
  keep <- apply(xm, 2, function(v) isTRUE(sd(v, na.rm = TRUE) > 0))   # solo parámetros que variaron
  ok   <- complete.cases(xm) & !is.na(d$y)
  if (!any(keep) || sum(ok) < sum(keep) + 10 || sd(d$y[ok]) == 0) return(NULL)
  tibble(tech_key = tk,
         param_id = colnames(xm)[keep],
         prcc     = prcc(d$y[ok], xm[ok, keep, drop = FALSE]),
         n_runs   = sum(ok))
}))

# ------------------------------------------------------------------
# 5. Sensibilidad de cada parámetro individual (resumen sobre las
#    tecnologías que toca, ponderado por su output de referencia)
# ------------------------------------------------------------------
param_info <- ptab %>%
  distinct(param_id, type_of_param, region, supplysector, subsector, demand_sector, param_default)

sens_param <- sens_tech %>%
  left_join(tech_ref, by = "tech_key") %>%
  group_by(param_id) %>%
  summarise(n_techs        = n(),
            max_abs_prcc   = max(abs(prcc), na.rm = TRUE),
            w_mean_abs_prcc = weighted.mean(abs(prcc), value_ref, na.rm = TRUE),
            tech_max       = tech_key[which.max(abs(prcc))],
            .groups = "drop") %>%
  left_join(param_info, by = "param_id") %>%
  arrange(desc(w_mean_abs_prcc))

sens_param

# ------------------------------------------------------------------
# 6. (Complemento) sensibilidad individual sobre el error global del run
# ------------------------------------------------------------------
L <- out %>% group_by(run_id) %>% summarise(L = mean(error_abs_norm), .groups = "drop")

sens_global <- X %>%
  inner_join(L, by = "run_id") %>%
  summarise(across(-c(run_id, L), ~ suppressWarnings(cor(.x, L, method = "spearman")))) %>%
  pivot_longer(everything(), names_to = "param_id", values_to = "rho_global") %>%
  left_join(param_info, by = "param_id") %>%
  arrange(desc(abs(rho_global)))







# 3 parámetros más sensibles y la tecnología donde más pegan
top3 <- sens_param %>%
  slice_max(w_mean_abs_prcc, n = 3, with_ties = FALSE) %>%
  select(param_id, tech_key = tech_max)

# un punto por run: valor del parámetro + output/error de su tecnología
plot_df <- ptab %>%
  filter(param_id %in% top3$param_id) %>%
  distinct(run_id, param_id, param) %>%
  left_join(top3, by = "param_id") %>%
  inner_join(out %>%
               mutate(tech_key = paste(region, sector, subsector, technology, sep = "|")) %>%
               select(run_id, tech_key, value_run, value_ref),
             by = c("run_id", "tech_key")) %>%
  mutate(error = value_run - value_ref,
         panel = paste0(param_id, "\n→ ", tech_key))

# Output vs parámetro
ggplot(plot_df, aes(x = param, y = value_run)) +
  geom_point() +
  facet_wrap(~ panel, scales = "free") +
  labs(x = "Valor del parámetro", y = "Output (value_run)")

# Error vs parámetro
ggplot(plot_df, aes(x = param, y = error)) +
  geom_point() +
  facet_wrap(~ panel, scales = "free") +
  labs(x = "Valor del parámetro", y = "Error (value_run - value_ref)")











#una fila por run x parámetro: el nido agregado

params_nest <- params_outputs %>%
  group_by(run_id, param_id, type_of_param, level, region, sector, nest_subsector,
           param, param_default, Units) %>%
  summarise(value_run   = sum(value_run),
            value_ref   = sum(value_ref),
            error_abs   = sum(abs(value_run - value_ref)),                         # error por tecnología, sumado
            error_share = sum(value_ref * abs(log(share_run / share_ref)), na.rm = TRUE),  # error de reparto
            .groups = "drop") %>%
  mutate(error_level = log(value_run / value_ref))




# X    <- mc_design_matrix(ptab, level = "multiplier_group")
#
# ref  <- mc_reference(dat_file)
# err  <- mc_output_errors(bind_rows(outputs_by_tech, mc_baseline_run(dat_file)), ref)
# err$run %>% arrange(L_local_sector_EJ)        # ¿qué runs mejoran el BASELINE?
#
# ds   <- mc_build_dataset(X, err, mc_nest_targets(err, c("Poland|regional coal", "UK|regional oil")))
# own  <- sa_own_nest(ptab, err)                # ¿cada candidato mueve su nido?
# sa_global(ds, target = "L_local_sector_EJ", method = "spearman")
# sa_global(ds, target = "Lnest_Poland.regional.coal", method = "rf")



