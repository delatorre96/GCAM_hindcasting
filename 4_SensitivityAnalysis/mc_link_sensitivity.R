# =============================================================================
# Monte Carlo GCAM-Europe: enlazar parámetros muestreados (inputs_run) con las
# salidas (outputs_by_tech) para hacer análisis de sensibilidad
# -----------------------------------------------------------------------------
# Flujo:
#   relevance <- read_csv(".../df_allParams_relevance.csv")
#   ptab      <- mc_param_table(inputs_run, relevance)          # run x parámetro (largo)
#   X         <- mc_design_matrix(ptab, level = "multiplier_group")  # run x factor (ancho)
#   ref       <- mc_reference(dat_file)                         # Reference 2021 (base 2021)
#   err       <- mc_output_errors(outputs_by_tech, ref)         # errores por run
#   ds        <- mc_build_dataset(X, err)                       # tabla para SA
#   own       <- sa_own_nest(ptab, err)                         # ¿mueve cada parámetro SU nido?
#   glob      <- sa_global(ds, target = "L_local_sector_EJ")    # importancia global
#
# Clave de enlace parámetro <-> df_allParams: xml_file + tag + occ (única).
#
# Métrica de error (la misma que en link_params_to_error*.R):
#   e = ln(value_run / value_ref)
#   error local de un nido = sum(q_ref * |ln(cuota_run / cuota_ref)|)
#   error de nivel de un sector = ln(Q_run / Q_ref)
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)

lnr <- function(a, b) {
  ok <- !is.na(a) & !is.na(b) & a > 0 & b > 0
  out <- rep(NA_real_, length(a)); out[ok] <- log(a[ok] / b[ok]); out
}
first_non_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA }

regions_eur <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia",
                 "Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania",
                 "Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia",
                 "Spain","Sweden","Albania","Bosnia and Herzegovina","Iceland","Macedonia","Moldova",
                 "Norway","Serbia and Montenegro","Turkey","UK","Ukraine")

trn_map <- c("1" = "trn_pass", "2" = "trn_freight", "3" = "trn_aviation_intl", "4" = "trn_shipping_intl")

# =============================================================================
# 1. Parámetros por run (formato largo) con su metadato de df_allParams_relevance
# =============================================================================
# Devuelve una fila por run x parámetro con:
#   param_id, type_of_param, region, supplysector, subsector, nest, demand_sector,
#   multiplier_group, family_group, candidate, priority, status,
#   param, param_default,
#   x_ratio = param / param_default        (multiplicador; NA si default = 0)
#   x_delta = param - param_default        (desplazamiento)
#   x       = log(x_ratio) si existe, si no x_delta   <- variable para la SA
mc_param_table <- function(inputs_run, relevance) {
  meta <- relevance %>%
    mutate(demand_sector = if ("demand_sector" %in% names(.)) demand_sector else NA_character_) %>%
    select(any_of(c("xml_file", "tag", "occ", "region", "supplysector", "subsector", "nest",
                    "demand_sector", "input_name", "group", "multiplier_group", "family_group",
                    "candidate", "priority", "status", "local_error_abs", "error_pct")),
           param_csv = param) %>%
    rename(region_csv = region, input_name_csv = input_name)

  ptab <- inputs_run %>%
    left_join(meta, by = c("xml_file", "tag", "occ")) %>%
    mutate(param_id = paste(xml_file, tag, occ, sep = "#"),
           x_ratio  = ifelse(!is.na(param_default) & param_default != 0 &
                               sign(param) == sign(param_default), param / param_default, NA_real_),
           x_delta  = param - param_default,
           x        = ifelse(!is.na(x_ratio), log(x_ratio), x_delta),
           x_scale  = ifelse(!is.na(x_ratio), "log_ratio", "delta"))

  # comprobaciones de enlace
  n_nomatch <- sum(is.na(ptab$param_csv))
  n_regdiff <- sum(!is.na(ptab$region_csv) & ptab$region != ptab$region_csv)
  n_defdiff <- sum(!is.na(ptab$param_csv) & abs(ptab$param_default - ptab$param_csv) > 1e-9)
  message(sprintf("mc_param_table: %d filas, %d runs, %d parámetros distintos",
                  nrow(ptab), n_distinct(ptab$run_id), n_distinct(ptab$param_id)))
  if (n_nomatch) warning(n_nomatch, " filas de inputs_run sin correspondencia en df_allParams_relevance (xml_file+tag+occ)")
  if (n_regdiff) warning(n_regdiff, " filas con región distinta entre inputs_run y df_allParams_relevance")
  if (n_defdiff) warning(n_defdiff, " filas donde param_default != param de df_allParams (¿XML distinto?)")

  ptab %>% select(-region_csv)
}

# =============================================================================
# 2. Matriz de diseño: run x factor
# =============================================================================
# level = "param"            -> una columna por fila de df_allParams (param_id)
#         "multiplier_group" -> una columna por tipo|región|sector|nivel
#         "family_group"     -> una columna por tipo|sector|nivel (todas las regiones)
# Si muestreaste un multiplicador común por grupo, x es igual dentro del grupo;
# la función devuelve también la dispersión interna para comprobarlo.
mc_design_matrix <- function(ptab, level = c("multiplier_group", "family_group", "param"),
                             only_candidates = FALSE, drop_constant = TRUE) {
  level <- match.arg(level)
  key <- switch(level, param = "param_id", multiplier_group = "multiplier_group", family_group = "family_group")
  d <- ptab
  if (only_candidates) d <- d %>% filter(candidate %in% TRUE)
  if (level != "param") {
    # parámetros sin grupo (no candidatos) -> su propio id
    d <- d %>% mutate(!!key := ifelse(is.na(.data[[key]]), param_id, .data[[key]]))
  }
  long <- d %>%
    group_by(run_id, factor = .data[[key]]) %>%
    summarise(x_sd = sd(x, na.rm = TRUE), n_rows = n(), x = mean(x, na.rm = TRUE), .groups = "drop")

  within <- long %>% group_by(factor) %>%
    summarise(max_within_sd = suppressWarnings(max(x_sd, na.rm = TRUE)), n_rows = first(n_rows), .groups = "drop")
  n_het <- sum(is.finite(within$max_within_sd) & within$max_within_sd > 1e-8)
  if (level != "param" && n_het > 0)
    message(n_het, " grupos con valores distintos dentro del grupo en un mismo run: ",
            "se usa la media de x (revisa attr(X, 'within_group'))")

  X <- long %>% select(run_id, factor, x) %>%
    pivot_wider(names_from = factor, values_from = x)
  if (drop_constant) {
    keep <- vapply(X[-1], function(v) isTRUE(sd(v, na.rm = TRUE) > 0), logical(1))
    X <- X[, c(TRUE, keep)]
  }
  attr(X, "within_group") <- within
  attr(X, "level") <- level
  message(sprintf("mc_design_matrix: %d runs x %d factores (%s)", nrow(X), ncol(X) - 1, level))
  X
}

# =============================================================================
# 3. Referencia (Reference, año base 2021) desde el .dat
# =============================================================================
mc_reference <- function(dat_file, scen_ref = "Reference", year = 2021, regions = regions_eur) {
  prj <- rgcam::loadProject(dat_file)
  rgcam::getQuery(prj, "outputs by tech") %>%
    filter(scenario == scen_ref, .data$year == !!year, region %in% regions) %>%
    group_by(region, sector, subsector, technology, output) %>%
    summarise(value_ref = sum(value), Units = first_non_na(Units), .groups = "drop")
}

# Ejecución por defecto del hindcast (base 2015 sin perturbar) en el formato de
# outputs_by_tech, para añadirla como run "BASELINE"
mc_baseline_run <- function(dat_file, scen = "BaseYear2015_policy_test_noEEAtradetax", year = 2021,
                            regions = regions_eur) {
  prj <- rgcam::loadProject(dat_file)
  rgcam::getQuery(prj, "outputs by tech") %>%
    filter(scenario == scen, .data$year == !!year, region %in% regions) %>%
    group_by(region, sector, subsector, output, technology) %>%
    summarise(`2021` = sum(value), .groups = "drop") %>%
    mutate(run_id = "BASELINE")
}

# =============================================================================
# 4. Errores de cada run frente a la referencia
# =============================================================================
# Devuelve una lista:
#   $tech           run x tecnología: q_ref, q_run, e_total, e_sector_level,
#                   e_subsector_share, e_tech_share
#   $nest_sector    run x región|sector: local_error_abs (logit de sector), e_level
#   $nest_subsector run x región|sector|subsector: local_error_abs (logit de subsector)
#   $run            una fila por run con pérdidas globales:
#       L_local_sector_EJ    suma del error local de los nidos de sector en EJ
#       L_local_subsector_EJ suma del error local de los nidos de subsector en EJ
#       L_level_EJ           sum |Q_run - Q_ref| de los sectores en EJ
#       L_level_log_med      mediana de |e_level| de los sectores (todas las unidades)
#       L_tech_wlog_EJ       media de |e_total| ponderada por q_ref (tecnologías en EJ)
#       n_tech_missing       tecnologías de la referencia sin output en el run
mc_output_errors <- function(outputs_by_tech, ref, regions = regions_eur, value_col = "2021") {
  ref_p <- ref %>% filter(output == sector, region %in% regions) %>%
    group_by(region, sector, subsector, technology) %>%
    summarise(q_ref = sum(value_ref), Units = first_non_na(Units), .groups = "drop")

  run <- outputs_by_tech %>%
    filter(output == sector, region %in% regions) %>%
    group_by(run_id, region, sector, subsector, technology) %>%
    summarise(q_run = sum(.data[[value_col]]), .groups = "drop")

  runs <- unique(run$run_id)
  # todas las tecnologías de la referencia en todos los runs (0 si el run no la produce)
  tech <- tidyr::crossing(run_id = runs, ref_p) %>%
    left_join(run, by = c("run_id", "region", "sector", "subsector", "technology")) %>%
    mutate(q_run = coalesce(q_run, 0))
  # tecnologías que el run produce y la referencia no (q_ref = 0)
  extra <- run %>% anti_join(ref_p, by = c("region", "sector", "subsector", "technology")) %>%
    mutate(q_ref = 0, Units = NA_character_)
  tech <- bind_rows(tech, extra)

  tech <- tech %>%
    group_by(run_id, region, sector) %>%
    mutate(Q_rs_ref = sum(q_ref), Q_rs_run = sum(q_run)) %>%
    group_by(run_id, region, sector, subsector) %>%
    mutate(Q_rsu_ref = sum(q_ref), Q_rsu_run = sum(q_run)) %>%
    ungroup() %>%
    mutate(e_total           = lnr(q_run, q_ref),
           e_sector_level    = lnr(Q_rs_run, Q_rs_ref),
           e_subsector_share = lnr(Q_rsu_run / Q_rs_run, Q_rsu_ref / Q_rs_ref),
           e_tech_share      = lnr(q_run / Q_rsu_run, q_ref / Q_rsu_ref))

  nest_sector <- tech %>%
    group_by(run_id, region, sector) %>%
    summarise(Units = first_non_na(Units),
              output_ref = sum(q_ref), output_run = sum(q_run),
              local_error_abs = sum(q_ref * abs(e_subsector_share), na.rm = TRUE),
              .groups = "drop") %>%
    mutate(e_level = lnr(output_run, output_ref))

  nest_subsector <- tech %>%
    group_by(run_id, region, sector, subsector) %>%
    summarise(Units = first_non_na(Units),
              output_ref = sum(q_ref), output_run = sum(q_run),
              local_error_abs = sum(q_ref * abs(e_tech_share), na.rm = TRUE),
              .groups = "drop")

  run_sum <- nest_sector %>%
    group_by(run_id) %>%
    summarise(L_local_sector_EJ = sum(local_error_abs[Units %in% "EJ"]),
              L_level_EJ        = sum(abs(output_run - output_ref)[Units %in% "EJ"]),
              L_level_log_med   = median(abs(e_level), na.rm = TRUE),
              .groups = "drop") %>%
    left_join(nest_subsector %>% group_by(run_id) %>%
                summarise(L_local_subsector_EJ = sum(local_error_abs[Units %in% "EJ"]), .groups = "drop"),
              by = "run_id") %>%
    left_join(tech %>% filter(Units %in% "EJ", q_ref > 0) %>% group_by(run_id) %>%
                summarise(L_tech_wlog_EJ = weighted.mean(abs(e_total), q_ref, na.rm = TRUE),
                          n_tech_missing = sum(q_run == 0), .groups = "drop"),
              by = "run_id")

  message(sprintf("mc_output_errors: %d runs, %d tecnologías de referencia, %d nidos de sector",
                  length(runs), nrow(ref_p), n_distinct(paste(nest_sector$region, nest_sector$sector))))
  list(tech = tech, nest_sector = nest_sector, nest_subsector = nest_subsector, run = run_sum)
}

# =============================================================================
# 5. Dataset para SA: X (factores) + Y (pérdidas por run)
# =============================================================================
mc_build_dataset <- function(X, err, extra_targets = NULL) {
  ds <- X %>% inner_join(err$run, by = "run_id")
  if (!is.null(extra_targets)) ds <- ds %>% left_join(extra_targets, by = "run_id")
  lost_x <- setdiff(X$run_id, err$run$run_id)
  lost_y <- setdiff(err$run$run_id, X$run_id)
  if (length(lost_x)) warning(length(lost_x), " runs con parámetros pero sin salidas (¿fallidos?): ",
                              paste(head(lost_x, 5), collapse = ", "))
  if (length(lost_y)) message(length(lost_y), " runs con salidas pero sin parámetros (p. ej. BASELINE)")
  ds
}

# Pérdida de nidos concretos como objetivo adicional (ancho: una columna por nido)
mc_nest_targets <- function(err, nests, type = c("sector", "subsector")) {
  type <- match.arg(type)
  d <- if (type == "sector") err$nest_sector %>% mutate(nest_key = paste(region, sector, sep = "|"))
       else err$nest_subsector %>% mutate(nest_key = paste(region, sector, subsector, sep = "|"))
  d %>% filter(nest_key %in% nests) %>%
    transmute(run_id, name = paste0("Lnest_", make.names(nest_key)), local_error_abs) %>%
    pivot_wider(names_from = name, values_from = local_error_abs)
}

# =============================================================================
# 6. ¿Mueve cada parámetro SU propio nido / servicio?
# =============================================================================
# Para cada parámetro (o grupo) candidato correlaciona, entre runs, su x con:
#   - logit de sector    -> local_error_abs de región|sector
#   - logit de subsector -> local_error_abs de región|sector|subsector
#   - satiation / elasticidades -> |e_level| del servicio
# Spearman (monótono, robusto). Responde a: "¿el nido que señalamos como
# candidato responde de verdad a su parámetro?"
sa_own_nest <- function(ptab, err, level = c("multiplier_group", "param"), only_candidates = TRUE) {
  level <- match.arg(level)
  key <- if (level == "param") "param_id" else "multiplier_group"
  d <- ptab
  if (only_candidates) d <- d %>% filter(candidate %in% TRUE)
  d <- d %>% filter(!is.na(.data[[key]])) %>%
    mutate(demand_sector = ifelse(is.na(demand_sector) & type_of_param %in% c("satiation_level"),
                                  input_name_csv, demand_sector),
           target_sector = ifelse(type_of_param == "logit", supplysector, demand_sector),
           target_sub    = ifelse(type_of_param == "logit" & nest %in% "subsector", subsector, NA_character_))

  xg <- d %>% group_by(run_id, factor = .data[[key]], type_of_param, region, target_sector, target_sub, nest) %>%
    summarise(x = mean(x, na.rm = TRUE), .groups = "drop")

  ys <- err$nest_sector %>%
    transmute(run_id, region, target_sector = sector, y_local = local_error_abs, y_level = abs(e_level))
  ysub <- err$nest_subsector %>%
    transmute(run_id, region, target_sector = sector, target_sub = subsector, y_sub = local_error_abs)

  xy <- xg %>%
    left_join(ys, by = c("run_id", "region", "target_sector")) %>%
    left_join(ysub, by = c("run_id", "region", "target_sector", "target_sub")) %>%
    mutate(y = case_when(type_of_param == "logit" & nest %in% "subsector" ~ y_sub,
                         type_of_param == "logit"                         ~ y_local,
                         TRUE                                             ~ y_level),
           y_metric = case_when(type_of_param == "logit" ~ "error local del nido",
                                TRUE ~ "|error de nivel| del servicio"))

  xy %>% group_by(factor, type_of_param, region, target_sector, target_sub, y_metric) %>%
    summarise(n_runs = sum(!is.na(x) & !is.na(y)),
              rho_spearman = suppressWarnings(cor(x, y, method = "spearman", use = "complete.obs")),
              p_value = tryCatch(suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE)$p.value),
                                 error = function(e) NA_real_),
              y_min = suppressWarnings(min(y, na.rm = TRUE)), y_median = median(y, na.rm = TRUE),
              y_max = suppressWarnings(max(y, na.rm = TRUE)),
              x_at_min_y = if (all(is.na(y))) NA_real_ else x[which.min(y)],
              .groups = "drop") %>%
    arrange(desc(abs(rho_spearman)))
}

# =============================================================================
# 7. Sensibilidad global de una pérdida a todos los factores
# =============================================================================
# method = "spearman": correlación de rangos de cada factor con la pérdida
#          "src":      coeficientes de regresión estandarizados (lineal; R2 indica
#                      cuánto explica un modelo lineal)
#          "rf":       importancia por permutación con ranger (no lineal,
#                      interacciones); requiere el paquete ranger
sa_global <- function(ds, target = "L_local_sector_EJ", method = c("spearman", "src", "rf"),
                      factors = NULL, num.trees = 500) {
  method <- match.arg(method)
  ycols <- c("L_local_sector_EJ", "L_local_subsector_EJ", "L_level_EJ", "L_level_log_med",
             "L_tech_wlog_EJ", "n_tech_missing")
  if (is.null(factors)) factors <- setdiff(names(ds), c("run_id", ycols, grep("^Lnest_", names(ds), value = TRUE)))
  d <- ds %>% select(all_of(c(target, factors))) %>% filter(!is.na(.data[[target]]))
  y <- d[[target]]
  Xm <- as.data.frame(d[factors])

  if (method == "spearman") {
    res <- tibble(factor = factors,
                  rho = vapply(Xm, function(v) suppressWarnings(cor(v, y, method = "spearman", use = "complete.obs")), 0))
    return(res %>% arrange(desc(abs(rho))))
  }
  if (method == "src") {
    Z <- as.data.frame(scale(Xm)); Z$y <- as.numeric(scale(y))
    fit <- lm(y ~ ., data = Z)
    res <- tibble(factor = names(coef(fit))[-1], src = unname(coef(fit))[-1]) %>%
      mutate(factor = gsub("`", "", factor)) %>% arrange(desc(abs(src)))
    attr(res, "R2") <- summary(fit)$r.squared
    message("SRC: R2 del modelo lineal = ", round(summary(fit)$r.squared, 3),
            if (nrow(d) <= length(factors) + 1) " (OJO: más factores que runs)" else "")
    return(res)
  }
  if (method == "rf") {
    if (!requireNamespace("ranger", quietly = TRUE)) stop("Instala el paquete ranger")
    names(Xm) <- make.names(names(Xm), unique = TRUE)
    fit <- ranger::ranger(x = Xm, y = y, num.trees = num.trees, importance = "permutation")
    res <- tibble(factor = factors, importance = unname(fit$variable.importance)) %>% arrange(desc(importance))
    attr(res, "oob_R2") <- fit$r.squared
    message("Random forest: R2 OOB = ", round(fit$r.squared, 3))
    return(res)
  }
}

# =============================================================================
# 8. Ejemplo de uso
# =============================================================================
if (FALSE) {
  params_dir <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/4_SensitivityAnalysis/create_dfParams"
  dat_file   <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/2. Extraction/BaseYear2015_outputsByTech.dat"

  relevance <- read_csv(file.path(params_dir, "df_allParams_relevance.csv"), guess_max = 50000, show_col_types = FALSE)

  # 1-2. parámetros
  ptab <- mc_param_table(inputs_run, relevance)
  X    <- mc_design_matrix(ptab, level = "multiplier_group")
  attr(X, "within_group")                      # comprueba si el grupo se muestreó como un solo multiplicador

  # 3-4. errores (referencia + opcionalmente el hindcast sin perturbar)
  ref  <- mc_reference(dat_file)
  base <- mc_baseline_run(dat_file)
  err  <- mc_output_errors(bind_rows(outputs_by_tech, base), ref)
  err$run %>% arrange(L_local_sector_EJ)       # runs ordenados por error local; compara con BASELINE

  # 5. dataset
  hot <- c("Poland|regional coal", "Czech Republic|regional coal", "UK|regional oil", "Germany|total biomass")
  ds  <- mc_build_dataset(X, err, extra_targets = mc_nest_targets(err, hot, "sector"))

  # 6. validación de los candidatos: ¿su parámetro mueve su nido?
  own <- sa_own_nest(ptab, err)
  own %>% filter(p_value < 0.05) %>% print(n = 30)

  # 7. sensibilidad global
  sa_global(ds, target = "L_local_sector_EJ", method = "spearman") %>% print(n = 20)
  sa_global(ds, target = "L_level_EJ",        method = "src")
  sa_global(ds, target = "Lnest_Poland.regional.coal", method = "rf")
}
