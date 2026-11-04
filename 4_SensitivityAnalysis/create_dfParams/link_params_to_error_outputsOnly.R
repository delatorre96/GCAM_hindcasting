# =============================================================================
# Vinculación parámetros <-> error de hindcast usando SOLO "outputs by tech"
# -----------------------------------------------------------------------------
# No necesita el grafo (inputs by tech + market-name). Da los mismos resultados
# que link_params_to_error.R para la puntuación de los parámetros, porque todos
# los nidos que controlan logit, satiation y elasticidades están definidos por
# las claves region | sector | subsector | technology de outputs by tech.
#
# Lo que NO da (para eso hace falta el grafo): si el error de un nodo es
# heredado de la demanda aguas abajo o nace en él, ni el comercio entre regiones.
#
# MÉTRICA
#   e = ln(chY / ref)           (> 0: la ejecución base 2015 sobreestima)
#   %  = 100 * (exp(e) - 1)
#
# IDENTIDAD (para cada tecnología t de región r, sector s, subsector u):
#   q_t = Q_rs * (Q_rsu / Q_rs) * (q_t / Q_rsu)
#   ln(q_t^chY/q_t^ref) = e_sector_level + e_subsector_share + e_tech_share
#     e_sector_level    = ln(Q_rs^chY / Q_rs^ref)             -> nivel (demanda)
#     e_subsector_share = ln(cuota_u^chY / cuota_u^ref)        -> logit de SECTOR
#     e_tech_share      = ln(cuota_t|u^chY / cuota_t|u^ref)    -> logit de SUBSECTOR
#
# PUNTUACIÓN DE CADA PARÁMETRO
#   logit de sector (r, s):        sum_t q_t^ref * |e_subsector_share_t|
#   logit de subsector (r, s, u):  sum_t q_t^ref * |e_tech_share_t|
#   satiation (r, servicio):       error de nivel del servicio (e_sector_level)
#   price / income elasticity:     error de nivel del servicio de transporte
#                                  + crecimiento 2015->2021 en ambas ejecuciones
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(rgcam)

# ---- rutas y escenarios -------------------------------------------------------
dat_file    <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/2. Extraction/BaseYear2015_outputsByTech.dat"
params_dir  <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/4_SensitivityAnalysis/create_dfParams"
params_file <- file.path(params_dir, "df_allParams.csv")
out_file    <- file.path(params_dir, "df_allParams_relevance_outputsOnly.csv")

scen_ref  <- "Reference"                                # año base 2021 (calibrado)
scen_chY  <- "BaseYear2015_policy_test_noEEAtradetax"   # año base 2015 (hindcast)
year_eval <- 2021
year_base <- 2015

lnr <- function(a, b) {
  ok <- !is.na(a) & !is.na(b) & a > 0 & b > 0
  out <- rep(NA_real_, length(a)); out[ok] <- log(a[ok] / b[ok]); out
}
pct <- function(e) 100 * (exp(e) - 1)
first_non_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA }

# =============================================================================
# PASO 1. Leer outputs by tech y dejar una fila por tecnología con q_ref y q_chY
# =============================================================================
prj <- loadProject(dat_file)
obt <- getQuery(prj, "outputs by tech") %>%
  filter(scenario %in% c(scen_ref, scen_chY))

# unidades por región|sector (de cualquiera de los dos escenarios)
units_rs <- obt %>%
  filter(year == year_eval, output == sector) %>%
  group_by(region, sector) %>%
  summarise(Units = first_non_na(Units), .groups = "drop")

tech <- obt %>%
  filter(year == year_eval, output == sector) %>%          # solo output primario
  group_by(scenario, region, sector, subsector, technology) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(scenario = ifelse(scenario == scen_ref, "q_ref", "q_chY")) %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  left_join(units_rs, by = c("region", "sector"))

# =============================================================================
# PASO 2. Cuotas anidadas y descomposición del error
# =============================================================================
tech <- tech %>%
  group_by(region, sector) %>%
  mutate(Q_rs_ref = sum(q_ref), Q_rs_chY = sum(q_chY)) %>%
  group_by(region, sector, subsector) %>%
  mutate(Q_rsu_ref = sum(q_ref), Q_rsu_chY = sum(q_chY)) %>%
  ungroup() %>%
  mutate(
    e_total           = lnr(q_chY, q_ref),
    e_sector_level    = lnr(Q_rs_chY, Q_rs_ref),
    e_subsector_share = lnr(Q_rsu_chY / Q_rs_chY, Q_rsu_ref / Q_rs_ref),
    e_tech_share      = lnr(q_chY / Q_rsu_chY, q_ref / Q_rsu_ref)
  )

chk <- with(tech, e_total - (e_sector_level + e_subsector_share + e_tech_share))
cat("Comprobación de la identidad, residuo máximo:", max(abs(chk), na.rm = TRUE), "\n")

# =============================================================================
# PASO 3. Error por nido de elección
# =============================================================================
nest_sector <- tech %>%                      # compiten subsectores de región|sector
  group_by(region, sector) %>%
  summarise(n_alternatives_active = n_distinct(subsector[q_ref > 0]),
            output_ref_2021 = sum(q_ref), output_chY_2021 = sum(q_chY),
            local_error_abs = sum(q_ref * abs(e_subsector_share), na.rm = TRUE),
            Units = first(Units), .groups = "drop")

nest_subsector <- tech %>%                   # compiten tecnologías de región|sector|subsector
  group_by(region, sector, subsector) %>%
  summarise(n_alternatives_active = n_distinct(technology[q_ref > 0]),
            output_ref_2021 = sum(q_ref), output_chY_2021 = sum(q_chY),
            local_error_abs = sum(q_ref * abs(e_tech_share), na.rm = TRUE),
            Units = first(Units), .groups = "drop")

nest_sector    <- nest_sector    %>% mutate(level_error_pct = pct(lnr(output_chY_2021, output_ref_2021)))
nest_subsector <- nest_subsector %>% mutate(level_error_pct = pct(lnr(output_chY_2021, output_ref_2021)))

# =============================================================================
# PASO 4. Emparejar cada parámetro con su nido o su servicio
# =============================================================================
params <- read_csv(params_file, guess_max = 50000, show_col_types = FALSE)

# 4a. logits
logits <- params %>% filter(type_of_param == "logit")
logit_rel <- bind_rows(
  logits %>% filter(is.na(subsector)) %>%
    left_join(nest_sector, by = c("region", "supplysector" = "sector")) %>% mutate(nest = "sector"),
  logits %>% filter(!is.na(subsector)) %>%
    left_join(nest_subsector, by = c("region", "supplysector" = "sector", "subsector")) %>% mutate(nest = "subsector")
) %>%
  mutate(status = case_when(
           is.na(output_ref_2021)     ~ "sin datos (anidado/no en query)",
           n_alternatives_active <= 1 ~ "inerte (1 alternativa)",
           TRUE                       ~ "activo"),
         local_error_rel = local_error_abs / output_ref_2021,
         error_metric = "local (reparto): sum q_ref*|e_share|")

# 4b. servicios (nivel) y crecimiento 2015 -> 2021
service <- nest_sector %>%
  transmute(region, sector, Units, output_ref_2021, output_chY_2021,
            e_level = lnr(output_chY_2021, output_ref_2021),
            error_pct = pct(e_level),
            demand_error_abs = output_chY_2021 - output_ref_2021)

growth <- obt %>%
  filter(year %in% c(year_base, year_eval), output == sector) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(key = paste0(ifelse(scenario == scen_ref, "ref", "chY"), "_", year)) %>%
  select(region, sector, key, value) %>%
  pivot_wider(names_from = key, values_from = value) %>%
  transmute(region, sector,
            growth_ref_2015_2021 = lnr(.data[[paste0("ref_", year_eval)]], .data[[paste0("ref_", year_base)]]),
            growth_chY_2015_2021 = lnr(.data[[paste0("chY_", year_eval)]], .data[[paste0("chY_", year_base)]]))

trn_map <- c("1" = "trn_pass", "2" = "trn_freight", "3" = "trn_aviation_intl", "4" = "trn_shipping_intl")
is_elas <- function(tp) tp %in% c("price_elasticity", "income_elasticity")

demand_rel <- params %>%
  filter(type_of_param %in% c("satiation_level", "price_elasticity", "income_elasticity")) %>%
  mutate(efd_index = ifelse(is_elas(type_of_param),
                            sub(".*energy-final-demand\\[([0-9]+)\\].*", "\\1", xpath), NA_character_),
         demand_sector = ifelse(type_of_param == "satiation_level", input_name, unname(trn_map[efd_index]))) %>%
  left_join(service, by = c("region", "demand_sector" = "sector")) %>%
  left_join(growth,  by = c("region", "demand_sector" = "sector")) %>%
  mutate(status = ifelse(is.na(output_ref_2021), "sin datos (sin output en query)", "activo"),
         growth_ref_2015_2021 = ifelse(is_elas(type_of_param), growth_ref_2015_2021, NA),
         growth_chY_2015_2021 = ifelse(is_elas(type_of_param), growth_chY_2015_2021, NA),
         growth_sign_mismatch = ifelse(is_elas(type_of_param) & !is.na(growth_ref_2015_2021),
                                       sign(growth_ref_2015_2021) != sign(growth_chY_2015_2021), NA),
         error_metric = "nivel del servicio: ln(chY/ref)")

# =============================================================================
# PASO 5. Rankings y candidatos
# =============================================================================
relevance <- bind_rows(logit_rel, demand_rel)

relevance$rank_within_family <- NA_real_
elig <- relevance$type_of_param == "logit" & relevance$status == "activo" & relevance$Units %in% "EJ"
relevance$rank_within_family[elig] <- rank(-relevance$local_error_abs[elig], ties.method = "first")
for (tp in c("satiation_level", "price_elasticity", "income_elasticity")) {
  elig <- relevance$type_of_param == tp & relevance$status == "activo"
  relevance$rank_within_family[elig] <- rank(-abs(relevance$demand_error_abs[elig]), ties.method = "first")
}

strip_decile <- function(x) gsub(" EUR", "", sub("\\s*(EUR)?_d[0-9]+$", "", x))
cum_share <- function(x) { o <- order(-x); r <- numeric(length(x)); r[o] <- cumsum(x[o]) / sum(x); r }

relevance <- relevance %>%
  group_by(type_of_param, Units, status) %>%
  mutate(cum = ifelse(type_of_param == "logit", cum_share(coalesce(local_error_abs, 0)),
                      cum_share(abs(coalesce(demand_error_abs, 0))))) %>%
  ungroup() %>%
  mutate(
    priority = case_when(
      type_of_param == "logit" & status == "activo" & Units == "EJ" &
        cum <= 0.80 & local_error_rel >= 0.05 ~ 1,
      type_of_param == "logit" & status == "activo" &
        cum <= 0.95 & local_error_rel >= 0.05 ~ 2,
      type_of_param == "satiation_level" & status == "activo" &
        cum <= 0.80 & abs(error_pct) >= 5 ~ 1,
      type_of_param == "satiation_level" & status == "activo" &
        cum <= 0.95 & abs(error_pct) >= 5 ~ 2,
      type_of_param == "price_elasticity" & status == "activo" &
        abs(error_pct) >= 10 ~ 3,
      type_of_param == "income_elasticity" & status == "activo" &
        abs(error_pct) >= 10 & !(growth_sign_mismatch %in% TRUE) ~ 3,
      TRUE ~ NA_real_),
    candidate = !is.na(priority),
    multiplier_group = case_when(
      candidate & type_of_param == "logit" ~
        paste("logit", region, strip_decile(supplysector), nest, sep = "|"),
      candidate & type_of_param == "satiation_level" ~
        paste("satiation", region, strip_decile(demand_sector), sep = "|"),
      candidate & is_elas(type_of_param) ~
        paste(type_of_param, region, demand_sector, sep = "|"),
      TRUE ~ NA_character_),
    family_group = ifelse(candidate, sub("^([^|]*)\\|[^|]*\\|", "\\1|", multiplier_group), NA_character_),
    reason = case_when(
      type_of_param == "logit" & grepl("^inerte", status) ~ "inerte: nido con <=1 alternativa",
      grepl("^sin datos", status) ~ "sin datos: no visible en outputs by tech",
      type_of_param == "logit" & priority == 1 ~ "logit: 80% del error local (EJ), >=5% mal repartido",
      type_of_param == "logit" & priority == 2 ~ "logit: 95% del error local de su unidad, >=5% mal repartido",
      type_of_param == "logit" ~ "logit activo con error local pequeño",
      type_of_param == "satiation_level" & priority %in% c(1, 2) & group %in% "heating" ~
        "satiation: error de demanda relevante; OJO grados-día constantes",
      type_of_param == "satiation_level" & priority %in% c(1, 2) ~ "satiation: error de demanda relevante",
      type_of_param == "satiation_level" ~ "satiation activo con error pequeño",
      type_of_param == "income_elasticity" & status == "activo" & abs(error_pct) >= 10 &
        growth_sign_mismatch %in% TRUE ~
        "no candidato: crecimiento 2015-2021 real y del modelo de signo opuesto; una elasticidad renta positiva no puede corregirlo",
      is_elas(type_of_param) & candidate & demand_sector == "trn_aviation_intl" ~
        "baja prioridad: +83% sistemático (COVID)",
      is_elas(type_of_param) & candidate & demand_sector %in% c("trn_shipping_intl", "trn_freight") ~
        "baja prioridad: error de reparto entre regiones (neto europeo pequeño)",
      is_elas(type_of_param) & candidate ~ "baja prioridad: error de nivel del transporte (parcialmente COVID)",
      TRUE ~ "error del servicio pequeño"),
    reason = ifelse(candidate & type_of_param == "logit" & param == 0,
                    paste0(reason, "; OJO logit=0 (cuotas fijas)"), reason),
    reason = ifelse(candidate & type_of_param == "price_elasticity" & growth_sign_mismatch %in% TRUE,
                    paste0(reason, "; crecimiento real y del modelo de signo opuesto"), reason)
  ) %>%
  select(-cum) %>%
  arrange(type_of_param, rank_within_family)

write_csv(relevance, out_file)

# =============================================================================
# PASO 6. Resúmenes
# =============================================================================
print(count(relevance, type_of_param, status))

relevance %>% filter(candidate) %>%
  group_by(type_of_param, priority) %>%
  summarise(filas = n(), mult_region = n_distinct(multiplier_group),
            mult_familia = n_distinct(family_group), .groups = "drop") %>% print()

cat("\nEjemplo paso a paso: Polonia, regional coal (logit de sector)\n")
tech %>% filter(region == "Poland", sector == "regional coal") %>%
  transmute(subsector, q_ref, q_chY,
            cuota_ref = Q_rsu_ref / Q_rs_ref, cuota_chY = Q_rsu_chY / Q_rs_chY,
            e_subsector_share, contrib = q_ref * abs(e_subsector_share)) %>% print(width = Inf)

# Comprobación opcional frente a la versión basada en el grafo
graph_csv <- file.path(params_dir, "df_allParams_relevance.csv")
if (file.exists(graph_csv)) {
  g <- read_csv(graph_csv, guess_max = 50000, show_col_types = FALSE) %>%
    select(xml_file, xpath, type_of_param, local_error_abs_graph = local_error_abs, candidate_graph = candidate)
  cmp <- relevance %>% select(xml_file, xpath, type_of_param, local_error_abs, candidate) %>%
    inner_join(g, by = c("xml_file", "xpath", "type_of_param"))
  cat("\nMáx. diferencia en local_error_abs frente al grafo:",
      max(abs(cmp$local_error_abs - cmp$local_error_abs_graph), na.rm = TRUE),
      "| candidatos coincidentes:", mean(cmp$candidate == cmp$candidate_graph), "\n")
}
