# =============================================================================
# GCAM-Europe hindcast: dónde nace el error y cómo se propaga
# -----------------------------------------------------------------------------
# Entradas (misma carpeta que este script):
#   gcam_graph_nodes.csv           nodos: tecnologías, mercados, recursos, sumideros
#   gcam_graph_edges.csv           aristas: supplies (tec -> mercado),
#                                  consumes (mercado -> tec),
#                                  unrecorded_final_demand (mercado -> sumidero)
#   gcam_graph_sector_edges.csv    grafo colapsado región|sector -> región|sector
#   gcam_graph_feedback_loops.csv  componentes fuertemente conexas (bucles)
#   ../2. Extraction/Data/all_errors_output_by_tech.csv   tu base de errores
#
# LIMITACIÓN CONOCIDA: la query "outputs/inputs by tech" usa el xPath
#   *[@type='sector']/*[@type='subsector']/*[@type='technology']
# que no baja a los nesting-subsector. En los países europeos las centrales
# eléctricas (base load / intermediate / subpeak / peak generation) están
# anidadas y NO aparecen: la electricidad se ve sólo desde la red regional.
#
# Año evaluado: 2021. value_ref = Reference (base 2021), value_chY = base 2015.
# Convención de error en este script:  e = ln(value_chY / value_ref)
#   e > 0  -> la ejecución base 2015 sobreestima respecto a la referencia.
#   (OJO: en all_errors_output_by_tech.csv  error = value_ref - value_chY,
#    es decir, el signo contrario.)
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(igraph)
library(Matrix)

base_dir <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/5. Error propagation graph"
err_file <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/2. Extraction/Data/all_errors_output_by_tech.csv"

nodes <- read_csv(file.path(base_dir, "gcam_graph_nodes.csv"), guess_max = 200000, show_col_types = FALSE)
edges <- read_csv(file.path(base_dir, "gcam_graph_edges.csv"), guess_max = 200000, show_col_types = FALSE)
sector_edges <- read_csv(file.path(base_dir, "gcam_graph_sector_edges.csv"), guess_max = 200000, show_col_types = FALSE)
loops <- read_csv(file.path(base_dir, "gcam_graph_feedback_loops.csv"), show_col_types = FALSE)
errors <- read_csv(err_file, show_col_types = FALSE)

# ---- helpers ---------------------------------------------------------------
lnr <- function(a, b) {                       # log-ratio; NA si algún valor <= 0
  ok <- !is.na(a) & !is.na(b) & a > 0 & b > 0
  out <- rep(NA_real_, length(a)); out[ok] <- log(a[ok] / b[ok]); out
}
lmean <- function(a, b) {                     # media logarítmica L(a,b) (LMDI)
  ok <- !is.na(a) & !is.na(b) & a > 0 & b > 0
  out <- rep(NA_real_, length(a))
  same <- ok & abs(a - b) <= 1e-12 * pmax(abs(a), abs(b))
  diff <- ok & !same
  out[same] <- a[same]
  out[diff] <- (a[diff] - b[diff]) / log(a[diff] / b[diff])
  out
}
make_id <- function(region, sector, subsector, technology, output)
  paste(region, sector, subsector, technology, output, sep = "|")

# =============================================================================
# 1. Cruce con tu base de errores (outputs by tech)
# =============================================================================
errors <- errors %>%
  mutate(node_id = make_id(region, sector, subsector, technology, output))

err_nodes <- errors %>%
  left_join(nodes %>% select(node_id, node_type, sells_to_market, value_ref_raw = value_ref,
                             value_chY_raw = value_chY),
            by = "node_id")

cat("Filas de error emparejadas con un nodo:", mean(!is.na(err_nodes$node_type)), "\n")

# =============================================================================
# 2. Objeto igraph y funciones de navegación
# =============================================================================
g <- graph_from_data_frame(
  d = edges %>% select(from, to, everything()),
  vertices = nodes %>% select(node_id, everything()),
  directed = TRUE
)

# Aguas arriba = proveedores (sentido contrario a las aristas)
upstream <- function(node, order = 2) {
  v <- ego(g, order = order, nodes = node, mode = "in")[[1]]
  nodes %>% filter(node_id %in% names(v))
}
# Aguas abajo = consumidores
downstream <- function(node, order = 2) {
  v <- ego(g, order = order, nodes = node, mode = "out")[[1]]
  nodes %>% filter(node_id %in% names(v))
}
# Ejemplo:
# upstream("Spain|delivered gas|delivered gas|delivered gas|delivered gas", order = 2)
# downstream("MKT|Spain|delivered gas", order = 1)

# Subgrafo europeo (nodos europeos + mercados de los que compran/venden)
eur_ids <- nodes$node_id[nodes$in_europe %in% TRUE]
eur_nb <- unique(unlist(lapply(adjacent_vertices(g, eur_ids, mode = "all"), names)))
g_eur  <- induced_subgraph(g, unique(c(eur_ids, eur_nb)))

# =============================================================================
# 3. Descomposición exacta del error de cada tecnología (nivel de oferta)
#    Q_t = S_M * s_region * s_subsector * s_tecnologia
#    ln(Q^/Q) = e_market + e_region_share + e_subsector_share + e_tech_share
#    e_market  = error heredado de la demanda del mercado al que vende
#    resto     = error "local": reparto logit (región en mercados comunes,
#                subsector, tecnología)
# =============================================================================
sup <- edges %>%
  filter(edge_type == "supplies") %>%
  mutate(flow_ref = coalesce(flow_ref, 0), flow_chY = coalesce(flow_chY, 0)) %>%
  separate_wider_delim(from, delim = "|", names = c("region", "sector", "subsector", "technology", "output"),
                       cols_remove = FALSE, too_many = "merge") %>%
  group_by(to) %>%
  mutate(S_ref = sum(flow_ref), S_chY = sum(flow_chY)) %>%
  group_by(to, region, sector) %>%
  mutate(R_ref = sum(flow_ref), R_chY = sum(flow_chY)) %>%
  group_by(to, region, sector, subsector) %>%
  mutate(U_ref = sum(flow_ref), U_chY = sum(flow_chY)) %>%
  ungroup() %>%
  mutate(
    e_total           = lnr(flow_chY, flow_ref),
    e_market          = lnr(S_chY, S_ref),
    e_region_share    = lnr(R_chY / S_chY, R_ref / S_ref),
    e_subsector_share = lnr(U_chY / R_chY, U_ref / R_ref),
    e_tech_share      = lnr(flow_chY / U_chY, flow_ref / U_ref),
    e_local           = e_region_share + e_subsector_share + e_tech_share,
    check             = e_total - e_market - e_local          # ~ 0 (identidad)
  ) %>%
  rename(node_id = from, market_id = to) %>%
  left_join(nodes %>% select(node_id, output_kind, in_europe), by = "node_id")

stopifnot(max(abs(sup$check), na.rm = TRUE) < 1e-8)

decomp <- sup %>%
  select(node_id, region, sector, subsector, technology, output, output_kind, in_europe, market_id,
         flow_ref, flow_chY, e_total, e_market, e_region_share, e_subsector_share, e_tech_share, e_local) %>%
  mutate(share_local = abs(e_local) / (abs(e_local) + abs(e_market)))

# Resumen por sector (Europa), ponderado por cantidad dentro de cada sector
decomp_sector <- decomp %>%
  filter(in_europe %in% TRUE, !is.na(e_total), output_kind == "primary") %>%
  group_by(sector) %>%
  summarise(n = n(),
            abs_e_total  = weighted.mean(abs(e_total), flow_ref),
            abs_e_market = weighted.mean(abs(e_market), flow_ref),
            abs_e_local  = weighted.mean(abs(e_local), flow_ref),
            share_local  = abs_e_local / (abs_e_local + abs_e_market),
            .groups = "drop") %>%
  arrange(desc(abs_e_total))

# =============================================================================
# 4. Atribución del error de demanda de cada mercado a sus consumidores
#    d ln S_M = sum_c (F^_c - F_c) / L(S^_M, S_M)       (exacto)
# =============================================================================
mk <- nodes %>% filter(node_type == "market") %>% select(node_id, supply_ref, supply_chY)

dem <- edges %>%
  filter(edge_type %in% c("consumes", "unrecorded_final_demand")) %>%
  mutate(flow_ref = coalesce(flow_ref, 0), flow_chY = coalesce(flow_chY, 0)) %>%
  inner_join(mk, by = c("from" = "node_id")) %>%
  mutate(LS      = lmean(supply_chY, supply_ref),
         contrib = (flow_chY - flow_ref) / LS,          # aportación al error del mercado
         alpha   = lmean(flow_chY, flow_ref) / LS,      # peso del error del flujo
         dlnF    = lnr(flow_chY, flow_ref)) %>%
  rename(market_id = from, consumer_id = to)

market_attribution <- dem %>%
  group_by(market_id) %>%
  mutate(e_market = lnr(first(supply_chY), first(supply_ref)),
         contrib_share = contrib / sum(contrib, na.rm = TRUE)) %>%
  ungroup() %>%
  select(market_id, consumer_id, edge_type, good, flow_ref, flow_chY, dlnF, contrib, contrib_share, e_market) %>%
  arrange(market_id, desc(abs(contrib)))

# Ejemplo: ¿quién explica el error de la demanda eléctrica de una región?
# market_attribution %>% filter(market_id == "MKT|Iberian_Peninsula|electricity")
# market_attribution %>% filter(market_id == "MKT|Spain|delivered gas")

# =============================================================================
# 5. Sistema de propagación (tipo Leontief) y origen del error
#    Para cada tecnología primaria t que vende en el mercado M(t):
#      e_t = e_local,t + o_M(t) + sum_k alpha_{M(t),k} * (e_k + c_k)
#    e_k  = error de la tecnología consumidora k
#    c_k  = cambio en el coeficiente de insumo de k (eficiencia/intensidad)
#    o_M  = demanda final no registrada + flujos nuevos/desaparecidos
#    En forma matricial:  e = origin + B e   =>   e = (I - B)^-1 origin
#    origin = errores "nacidos" en cada nodo; (I-B)^-1 = cómo se propagan.
# =============================================================================
prim <- decomp %>% filter(output_kind == "primary", !is.na(e_total)) %>% mutate(t = row_number())
N <- nrow(prim)
q_err <- setNames(prim$e_total, prim$node_id)

dem2 <- dem %>%
  mutate(k     = match(consumer_id, prim$node_id),
         coef  = dlnF - q_err[consumer_id],
         endo  = edge_type == "consumes" & !is.na(k) & !is.na(alpha) & !is.na(dlnF),
         o_coef  = ifelse(endo, alpha * coef, 0),
         o_final = ifelse(edge_type == "unrecorded_final_demand", coalesce(contrib, 0), 0),
         o_other = ifelse(!endo & edge_type == "consumes", coalesce(contrib, 0), 0))

oM <- dem2 %>% group_by(market_id) %>%
  summarise(o_coef = sum(o_coef, na.rm = TRUE), o_final = sum(o_final), o_other = sum(o_other),
            .groups = "drop")

prim <- prim %>% left_join(oM, by = "market_id") %>%
  mutate(across(c(o_coef, o_final, o_other), ~ coalesce(.x, 0)),
         origin = e_local + o_coef + o_final + o_other)

Bdf <- dem2 %>% filter(endo) %>%
  inner_join(prim %>% select(market_id, t), by = "market_id", relationship = "many-to-many")
B <- sparseMatrix(i = Bdf$t, j = Bdf$k, x = Bdf$alpha, dims = c(N, N))
A <- Diagonal(N) - B

# Comprobación: e_t == origin + B e  (identidad)
res <- prim$e_total - (prim$origin + as.numeric(B %*% prim$e_total))
cat("Residuo máximo del sistema:", max(abs(res)), "\n")

# Origen del error de un nodo concreto: contribución de cada nodo j
#   e_t = sum_j [(I-B)^-1]_{t,j} * origin_j
error_origins <- function(node, top = 25) {
  t0 <- match(node, prim$node_id)
  if (is.na(t0)) stop("Nodo no encontrado entre las tecnologías primarias")
  u <- numeric(N); u[t0] <- 1
  y <- as.numeric(solve(t(A), u))                  # fila t0 de (I-B)^-1
  prim %>%
    mutate(multiplier = y,
           contrib_local  = y * e_local,
           contrib_coef   = y * o_coef,
           contrib_final  = y * o_final,
           contrib_other  = y * o_other,
           contrib        = y * origin) %>%
    filter(abs(contrib) > 0) %>%
    arrange(desc(abs(contrib))) %>%
    select(node_id, region, sector, technology, multiplier, contrib, contrib_local, contrib_coef,
           contrib_final, contrib_other) %>%
    head(top)
}
# Ejemplo:
# error_origins("Spain|delivered gas|delivered gas|delivered gas|delivered gas")
# error_origins("Iberian_Peninsula|regional electricity|domestic electricity|domestic electricity|regional electricity")

# Mapa europeo de "dónde nace" el error: magnitud del origen en cada nodo y
# cuánto error genera aguas arriba (suma de la columna de (I-B)^-1 * origin)
prim <- prim %>% left_join(loops %>% select(node_id, loop_id), by = "node_id")

origin_eur <- prim %>%
  filter(in_europe %in% TRUE) %>%
  mutate(abs_origin = abs(origin),
         origin_type = c("local_share", "input_coef", "final_demand", "other")[
           max.col(abs(cbind(e_local, o_coef, o_final, o_other)), ties.method = "first")]) %>%
  arrange(desc(abs_origin))

origin_by_sector <- origin_eur %>%
  group_by(sector) %>%
  summarise(n = n(),
            mean_abs_origin = weighted.mean(abs(origin), flow_ref),
            mean_abs_e      = weighted.mean(abs(e_total), flow_ref),
            born_here_ratio = mean_abs_origin / mean_abs_e,     # <1: más heredado que nacido
            main_origin_type = names(which.max(table(origin_type))),
            .groups = "drop") %>%
  arrange(desc(mean_abs_e))

# =============================================================================
# 6. Bucles de retroalimentación
# =============================================================================
loop_summary <- loops %>%
  left_join(nodes %>% select(node_id, node_type, region, sector, in_europe), by = "node_id") %>%
  group_by(loop_id, loop_size) %>%
  summarise(n_eur = sum(in_europe %in% TRUE),
            sectors = paste(head(sort(unique(sector)), 15), collapse = "; "),
            .groups = "drop")

# =============================================================================
# 7. Grafo sectorial (más manejable para visualizar)
# =============================================================================
g_sec <- sector_edges %>%
  filter(in_europe) %>%
  mutate(from = paste(from_region, from_sector, sep = "|"),
         to   = paste(to_region, to_sector, sep = "|"),
         e_flow = lnr(flow_chY, flow_ref)) %>%
  select(from, to, everything()) %>%
  graph_from_data_frame(directed = TRUE)

# Ejemplo de visualización local:
# sub <- make_ego_graph(g_sec, order = 1, nodes = "Iberian_Peninsula|electricity", mode = "all")[[1]]
# plot(sub, vertex.label.cex = 0.6, edge.arrow.size = 0.3,
#      edge.color = ifelse(E(sub)$e_flow > 0, "firebrick", "steelblue"))

# =============================================================================
# 8. Guardar resultados
# =============================================================================
write_csv(decomp,             file.path(base_dir, "out_decomposition_by_node.csv"))
write_csv(decomp_sector,      file.path(base_dir, "out_decomposition_by_sector_EUR.csv"))
write_csv(market_attribution, file.path(base_dir, "out_market_demand_attribution.csv"))
write_csv(origin_eur %>% select(node_id, region, sector, subsector, technology, flow_ref, e_total,
                                e_market, e_local, o_coef, o_final, o_other, origin, origin_type, loop_id),
          file.path(base_dir, "out_error_origins_EUR.csv"))
write_csv(origin_by_sector,   file.path(base_dir, "out_error_origins_by_sector_EUR.csv"))
