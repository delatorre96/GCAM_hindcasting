# =============================================================================
# Vinculación de los parámetros de df_allParams.csv con el error de hindcast
# -----------------------------------------------------------------------------
# Entradas
#   - gcam_graph_nodes.csv / gcam_graph_edges.csv  (grafo de "Error propagation graph")
#       value_ref = Reference (año base 2021), value_chY = BaseYear2015 (año 2021)
#       Cada nodo tecnología = una fila de outputs by tech; cada arista "supplies"
#       = tecnología -> mercado al que vende.
#   - df_allParams.csv  (logit, satiation_level, price_elasticity, income_elasticity)
#   - BaseYear2015_outputsByTech.dat (rgcam) para el crecimiento 2015->2021 del transporte
#
# Métrica de error:  e = ln(value_chY / value_ref)     (% = 100*(exp(e)-1))
#
# Dos componentes (identidad exacta para cada tecnología t que vende en M):
#   e_t = e_market + e_region_share + e_subsector_share + e_tech_share
#         \______/   \______________________________________________/
#         heredado              local (reparto logit)
#
# Vinculación
#   LOGIT de sector    (subsector vacío): nido = subsectores de región|sector
#        error asignado = sum( q_ref * |e_subsector_share| )
#   LOGIT de subsector (subsector lleno): nido = tecnologías de región|sector|subsector
#        error asignado = sum( q_ref * |e_tech_share| )
#        status: "sin datos" (nido no aparece en outputs by tech),
#                "inerte" (<= 1 alternativa con producción), "activo" (>= 2)
#   SATIATION LEVEL: input_name = servicio de edificios -> error de nivel de ese
#        sector en la región (output total)
#   PRICE / INCOME ELASTICITY: energy-final-demand[k] -> 1 trn_pass, 2 trn_freight,
#        3 trn_aviation_intl, 4 trn_shipping_intl -> error de nivel del servicio
#        + crecimiento 2015->2021 en la referencia y en la ejecución base 2015:
#        si tienen signo opuesto, una elasticidad renta positiva no puede
#        corregir el error (p. ej. caída por COVID).
# =============================================================================

library(dplyr)
library(readr)
library(rgcam)

graph_dir  <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/3. Error structure/Error propagation graph"
params_dir <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/4_SensitivityAnalysis/create_dfParams"
dat_file   <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/2. Extraction/BaseYear2015_outputsByTech.dat"
scen_ref   <- "Reference"
scen_chY   <- "BaseYear2015_policy_test_noEEAtradetax"

edges_file <- file.path(graph_dir, "gcam_graph_edges.csv")
if (!file.exists(edges_file)) edges_file <- paste0(edges_file, ".gz")

nodes  <- read_csv(file.path(graph_dir, "gcam_graph_nodes.csv"), guess_max = 200000, show_col_types = FALSE)
edges  <- read_csv(edges_file, guess_max = 200000, show_col_types = FALSE)
params <- read_csv(file.path(params_dir, "df_allParams.csv"), guess_max = 50000, show_col_types = FALSE)

lnr <- function(a, b) {
  ok <- !is.na(a) & !is.na(b) & a > 0 & b > 0
  out <- rep(NA_real_, length(a)); out[ok] <- log(a[ok] / b[ok]); out
}
pct <- function(e) 100 * (exp(e) - 1)

# =============================================================================
# 1. Descomposición del error de cada tecnología (heredado vs local)
# =============================================================================
tech_info <- nodes %>%
  filter(node_type == "technology") %>%
  select(node_id, region, sector, subsector, technology, output_kind, Units)

sup <- edges %>%
  filter(edge_type == "supplies") %>%
  transmute(node_id = from, market_id = to,
            q_ref = coalesce(flow_ref, 0), q_chY = coalesce(flow_chY, 0)) %>%
  inner_join(tech_info, by = "node_id") %>%
  # totales para las cuotas anidadas dentro de cada mercado
  group_by(market_id) %>%
  mutate(S_ref = sum(q_ref), S_chY = sum(q_chY)) %>%
  group_by(market_id, region, sector) %>%
  mutate(R_ref = sum(q_ref), R_chY = sum(q_chY)) %>%
  group_by(market_id, region, sector, subsector) %>%
  mutate(U_ref = sum(q_ref), U_chY = sum(q_chY)) %>%
  ungroup() %>%
  mutate(
    e_total           = lnr(q_chY, q_ref),
    e_market          = lnr(S_chY, S_ref),                         # heredado
    e_region_share    = lnr(R_chY / S_chY, R_ref / S_ref),         # local
    e_subsector_share = lnr(U_chY / R_chY, U_ref / R_ref),         # local  <- logit de sector
    e_tech_share      = lnr(q_chY / U_chY, q_ref / U_ref),         # local  <- logit de subsector
    e_local           = e_region_share + e_subsector_share + e_tech_share
  ) %>%
  filter(output_kind == "primary")

# comprobación de la identidad
chk <- with(sup, e_total - e_market - e_local)
cat("Residuo máximo de la identidad e_total = e_market + e_local:",
    max(abs(chk), na.rm = TRUE), "\n")

# =============================================================================
# 2. Error por NIDO de elección
# =============================================================================
# 2a. Nido de sector: compiten los subsectores de región|sector
nest_sector <- sup %>%
  group_by(region, sector) %>%
  summarise(n_alternatives_active = n_distinct(subsector[q_ref > 0]),
            output_ref_2021 = sum(q_ref),
            output_chY_2021 = sum(q_chY),
            local_error_abs = sum(q_ref * abs(e_subsector_share), na.rm = TRUE),
            Units = first(Units),
            .groups = "drop") %>%
  mutate(level_error_pct = pct(lnr(output_chY_2021, output_ref_2021)))   # error neto del nido

# 2b. Nido de subsector: compiten las tecnologías de región|sector|subsector
nest_subsector <- sup %>%
  group_by(region, sector, subsector) %>%
  summarise(n_alternatives_active = n_distinct(technology[q_ref > 0]),
            output_ref_2021 = sum(q_ref),
            output_chY_2021 = sum(q_chY),
            local_error_abs = sum(q_ref * abs(e_tech_share), na.rm = TRUE),
            Units = first(Units),
            .groups = "drop") %>%
  mutate(level_error_pct = pct(lnr(output_chY_2021, output_ref_2021)))

# =============================================================================
# 3. LOGITS: emparejar cada fila con su nido
# =============================================================================
logits <- params %>% filter(type_of_param == "logit")

logit_sector <- logits %>%
  filter(is.na(subsector)) %>%
  left_join(nest_sector, by = c("region", "supplysector" = "sector")) %>%
  mutate(nest = "sector")

logit_subsector <- logits %>%
  filter(!is.na(subsector)) %>%
  left_join(nest_subsector, by = c("region", "supplysector" = "sector", "subsector")) %>%
  mutate(nest = "subsector")

logit_rel <- bind_rows(logit_sector, logit_subsector) %>%
  mutate(
    status = case_when(
      is.na(output_ref_2021)      ~ "sin datos (anidado/no en query)",
      n_alternatives_active <= 1  ~ "inerte (1 alternativa)",
      TRUE                        ~ "activo"),
    local_error_rel = local_error_abs / output_ref_2021,
    error_metric    = "local (reparto): sum q_ref*|e_share|"
  )

# =============================================================================
# 4. SATIATION LEVEL y PRICE ELASTICITY: error de nivel del servicio
# =============================================================================
service <- sup %>%
  group_by(region, sector) %>%
  summarise(output_ref_2021 = sum(q_ref), output_chY_2021 = sum(q_chY),
            Units = first(Units),
            e_market_service = first(e_market),   # todas las tec. del servicio venden al mismo mercado
            .groups = "drop") %>%
  mutate(e_level = lnr(output_chY_2021, output_ref_2021),
         error_pct = pct(e_level),
         demand_error_abs = output_chY_2021 - output_ref_2021)

trn_map <- c("1" = "trn_pass", "2" = "trn_freight", "3" = "trn_aviation_intl", "4" = "trn_shipping_intl")

demand_rel <- params %>%
  filter(type_of_param %in% c("satiation_level", "price_elasticity", "income_elasticity")) %>%
  mutate(efd_index = ifelse(type_of_param %in% c("price_elasticity", "income_elasticity"),
                            sub(".*energy-final-demand\\[([0-9]+)\\].*", "\\1", xpath), NA_character_),
         demand_sector = ifelse(type_of_param == "satiation_level", input_name, unname(trn_map[efd_index]))) %>%
  left_join(service, by = c("region", "demand_sector" = "sector")) %>%
  mutate(status = ifelse(is.na(output_ref_2021), "sin datos (sin output en query)", "activo"),
         error_metric = "nivel del servicio: ln(chY/ref)")

# ---- 4b. Crecimiento 2015 -> 2021 de los servicios de transporte --------------
#   growth_ref = ln(ref_2021 / ref_2015)   (lo que "pasó", según la referencia calibrada)
#   growth_chY = ln(chY_2021 / chY_2015)   (lo que proyecta el modelo desde 2015)
#   Si el signo es opuesto, la elasticidad renta (> 0) no puede cerrar el error.
prj <- loadProject(dat_file)
obt <- getQuery(prj, "outputs by tech") %>%
  filter(scenario %in% c(scen_ref, scen_chY), year %in% c(2015, 2021),
         sector %in% unname(trn_map), output == sector) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value), .groups = "drop")

trn_growth <- obt %>%
  mutate(key = paste0(ifelse(scenario == scen_ref, "ref", "chY"), "_", year)) %>%
  select(region, sector, key, value) %>%
  tidyr::pivot_wider(names_from = key, values_from = value) %>%
  mutate(growth_ref_2015_2021 = lnr(ref_2021, ref_2015),
         growth_chY_2015_2021 = lnr(chY_2021, chY_2015)) %>%
  select(region, demand_sector = sector, growth_ref_2015_2021, growth_chY_2015_2021)

demand_rel <- demand_rel %>%
  left_join(trn_growth, by = c("region", "demand_sector")) %>%
  mutate(growth_sign_mismatch = ifelse(
    type_of_param %in% c("price_elasticity", "income_elasticity") & !is.na(growth_ref_2015_2021),
    sign(growth_ref_2015_2021) != sign(growth_chY_2015_2021), NA))

# =============================================================================
# 5. Rankings y salida
# =============================================================================
# ranking de logits: solo activos en EJ (unidades comparables)
elig <- logit_rel$status == "activo" & logit_rel$Units %in% "EJ"
logit_rel$rank_within_family <- NA_real_
logit_rel$rank_within_family[elig] <- rank(-logit_rel$local_error_abs[elig], ties.method = "first")

demand_rel$rank_within_family <- NA_real_
for (tp in c("satiation_level", "price_elasticity", "income_elasticity")) {
  elig <- demand_rel$type_of_param == tp & demand_rel$status == "activo"
  demand_rel$rank_within_family[elig] <- rank(-abs(demand_rel$demand_error_abs[elig]), ties.method = "first")
}

relevance <- bind_rows(logit_rel, demand_rel)

# =============================================================================
# 6. Candidatos a muestrear en el Monte Carlo
# -----------------------------------------------------------------------------
#  Logits (solo "activo"), por unidad:
#    prioridad 1: EJ, dentro del 80 % acumulado de local_error_abs y local_error_rel >= 5 %
#    prioridad 2: resto dentro del 95 % acumulado de su unidad y local_error_rel >= 5 %
#  Satiation (solo "activo", EJ):
#    prioridad 1: dentro del 80 % acumulado de |demand_error_abs| y |error_pct| >= 5 %
#    prioridad 2: dentro del 95 % acumulado y |error_pct| >= 5 %
#  Price / income elasticity (solo "activo"): prioridad 3 si |error_pct| >= 10 %
#    (baja: el error de transporte es sobre todo COVID / reparto entre regiones).
#    Income elasticity: además se excluye si el crecimiento 2015-2021 real y el
#    del modelo tienen signo opuesto.
#  multiplier_group: un multiplicador por tipo|región|sector(sin decil)|nivel
#  family_group:     un multiplicador por tipo|sector(sin decil)|nivel (todas las regiones)
# =============================================================================
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
        cum <= 0.80 & local_error_rel >= 0.05                                  ~ 1,
      type_of_param == "logit" & status == "activo" &
        cum <= 0.95 & local_error_rel >= 0.05                                  ~ 2,
      type_of_param == "satiation_level" & status == "activo" &
        cum <= 0.80 & abs(error_pct) >= 5                                      ~ 1,
      type_of_param == "satiation_level" & status == "activo" &
        cum <= 0.95 & abs(error_pct) >= 5                                      ~ 2,
      type_of_param == "price_elasticity" & status == "activo" &
        abs(error_pct) >= 10                                                   ~ 3,
      type_of_param == "income_elasticity" & status == "activo" &
        abs(error_pct) >= 10 & !(growth_sign_mismatch %in% TRUE)              ~ 3,
      TRUE ~ NA_real_),
    candidate = !is.na(priority),
    multiplier_group = case_when(
      candidate & type_of_param == "logit" ~
        paste("logit", region, strip_decile(supplysector), nest, sep = "|"),
      candidate & type_of_param == "satiation_level" ~
        paste("satiation", region, strip_decile(demand_sector), sep = "|"),
      candidate & type_of_param %in% c("price_elasticity", "income_elasticity") ~
        paste(type_of_param, region, demand_sector, sep = "|"),
      TRUE ~ NA_character_),
    family_group = ifelse(candidate, sub("^([^|]*)\\|[^|]*\\|", "\\1|", multiplier_group), NA_character_),
    reason = case_when(
      type_of_param == "logit" & grepl("^inerte", status) ~ "inerte: nido con <=1 alternativa",
      grepl("^sin datos", status) ~ "sin datos: no visible en outputs by tech",
      type_of_param == "logit" & priority == 1 ~ "logit: 80% del error local (EJ), >=5% mal repartido",
      type_of_param == "logit" & priority == 2 ~ "logit: 95% del error local de su unidad, >=5% mal repartido",
      type_of_param == "logit" ~ "logit activo con error local pequeño",
      type_of_param == "satiation_level" & priority %in% c(1, 2) ~
        paste0("satiation: error de demanda relevante",
               ifelse(group == "heating", "; OJO grados-día constantes", "")),
      type_of_param == "satiation_level" ~ "satiation activo con error pequeño",
      type_of_param == "income_elasticity" & status == "activo" & abs(error_pct) >= 10 &
        growth_sign_mismatch %in% TRUE ~
        "no candidato: crecimiento 2015-2021 real y del modelo de signo opuesto; una elasticidad renta positiva no puede corregirlo (probable COVID / shock)",
      type_of_param %in% c("price_elasticity", "income_elasticity") &
        demand_sector == "trn_aviation_intl" & candidate ~
        "baja prioridad: +83% sistemático (COVID)",
      type_of_param %in% c("price_elasticity", "income_elasticity") &
        demand_sector %in% c("trn_shipping_intl", "trn_freight") & candidate ~
        "baja prioridad: error de reparto entre regiones (neto europeo pequeño)",
      type_of_param %in% c("price_elasticity", "income_elasticity") & candidate ~
        "baja prioridad: error de nivel del transporte (parcialmente COVID)",
      TRUE ~ "error del servicio pequeño"),
    reason = ifelse(candidate & type_of_param == "logit" & param == 0,
                    paste0(reason, "; OJO logit=0 (cuotas fijas)"), reason),
    reason = ifelse(candidate & type_of_param == "price_elasticity" & growth_sign_mismatch %in% TRUE,
                    paste0(reason, "; crecimiento real y del modelo de signo opuesto"), reason)
  ) %>%
  select(-cum) %>%
  arrange(type_of_param, rank_within_family)

cat("\nCandidatos:\n")
relevance %>% filter(candidate) %>%
  group_by(type_of_param, priority) %>%
  summarise(filas = n(), multiplicadores_region = n_distinct(multiplier_group),
            multiplicadores_familia = n_distinct(family_group), .groups = "drop") %>%
  print()

write_csv(relevance, file.path(params_dir, "df_allParams_relevance.csv"))

# ---- resúmenes ----------------------------------------------------------------
print(count(relevance, type_of_param, status))

cat("\nLogits activos (EJ) con más error local:\n")
logit_rel %>% filter(status == "activo", Units == "EJ") %>%
  arrange(desc(local_error_abs)) %>%
  select(region, supplysector, subsector, nest, param, n_alternatives_active,
         output_ref_2021, local_error_abs, level_error_pct) %>%
  head(20) %>% print(width = Inf)

cat("\nCompensación: nidos con mucho error local pero poco error neto de nivel:\n")
logit_rel %>% filter(status == "activo", Units == "EJ", nest == "sector") %>%
  mutate(local_pct_of_output = 100 * local_error_rel) %>%
  arrange(desc(local_error_abs)) %>%
  select(region, supplysector, output_ref_2021, local_pct_of_output, level_error_pct) %>%
  head(10) %>% print(width = Inf)

cat("\nSatiation: servicios con más error absoluto:\n")
demand_rel %>% filter(type_of_param == "satiation_level", status == "activo") %>%
  arrange(desc(abs(demand_error_abs))) %>%
  select(region, demand_sector, group, param, output_ref_2021, output_chY_2021, error_pct) %>%
  head(15) %>% print(width = Inf)

cat("\nTransporte: error por servicio (neto europeo vs suma de |error|) y crecimiento 2015-2021:\n")
demand_rel %>% filter(type_of_param == "income_elasticity", status == "activo") %>%
  group_by(demand_sector) %>%
  summarise(n = n(), ref = sum(output_ref_2021), chY = sum(output_chY_2021),
            net_pct = 100 * (chY / ref - 1),
            sum_abs_error = sum(abs(demand_error_abs)),
            median_abs_pct = median(abs(error_pct)),
            growth_ref_med = median(growth_ref_2015_2021, na.rm = TRUE),
            growth_chY_med = median(growth_chY_2015_2021, na.rm = TRUE),
            share_sign_mismatch = mean(growth_sign_mismatch, na.rm = TRUE), .groups = "drop") %>%
  print(width = Inf)
