# =============================================================================
# Grafo sectorial de GCAM-Europe -> Mermaid, con módulos y traza del error
# -----------------------------------------------------------------------------
# Idea:
#  1. Vecindario de orden k alrededor de un nodo semilla (como make_ego_graph).
#  2. Detección de módulos (Louvain): grupos de sectores muy conectados entre sí
#     y con pocas aristas hacia fuera (p. ej. los 68 sectores residenciales de
#     España cuelgan todos de elect_td_bld). Cada módulo es un `subgraph`.
#  3. Los módulos que contienen la semilla o la traza del error se DESPLIEGAN
#     (se ven sus nodos más calientes; el resto se agrupa en "+N sectores").
#     Los demás se PLIEGAN en una sola caja con su mediana y su máximo.
#  4. Calor: color de nodo según |error| (escala inferno), aristas rojas
#     (sobreestima) o azules (subestima) con grosor según |error| del flujo.
#  5. Traza (naranja grueso): desde la semilla, se sigue en cada paso al
#     consumidor que más aporta al error de demanda del nodo (descomposición
#     LMDI exacta). Es el camino por el que el error llega HEREDADO a la semilla.
#
# Error: e = ln(value_chY / value_ref); se muestra como % = 100*(exp(e)-1).
# "her." = parte del |error| del nodo heredada de la demanda de su mercado.
# =============================================================================

library(dplyr)
library(readr)
library(igraph)

base_dir <- "C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/5. Error propagation graph"

nodes        <- read_csv(file.path(base_dir, "gcam_graph_nodes.csv"), guess_max = 200000, show_col_types = FALSE)
sector_edges <- read_csv(file.path(base_dir, "gcam_graph_sector_edges.csv"), guess_max = 200000, show_col_types = FALSE)

lnr <- function(a, b) {
  ok <- !is.na(a) & !is.na(b) & a > 0 & b > 0
  out <- rep(NA_real_, length(a)); out[ok] <- log(a[ok] / b[ok]); out
}
lmean <- function(a, b) {
  if (is.na(a) || is.na(b) || a <= 0 || b <= 0) return(NA_real_)
  if (abs(a - b) <= 1e-12 * max(a, b)) return(a)
  (a - b) / log(a / b)
}
pct <- function(e) 100 * (exp(e) - 1)

# ---- 1. Error por nodo región|sector -----------------------------------------
mkt <- nodes %>%
  filter(node_type == "market") %>%
  transmute(sells_to_market = node_id, e_market = lnr(supply_chY, supply_ref))

sec_nodes <- nodes %>%
  filter(node_type == "technology", output_kind == "primary") %>%
  group_by(region, sector) %>%
  summarise(q_ref = sum(value_ref, na.rm = TRUE), q_chY = sum(value_chY, na.rm = TRUE),
            sells_to_market = first(sells_to_market), .groups = "drop") %>%
  left_join(mkt, by = "sells_to_market") %>%
  mutate(name = paste(region, sector, sep = "|"),
         e_node = lnr(q_chY, q_ref),
         e_local = e_node - e_market,
         share_inherited = abs(e_market) / (abs(e_market) + abs(e_local)))

e_node_of   <- setNames(sec_nodes$e_node, sec_nodes$name)
inh_of      <- setNames(sec_nodes$share_inherited, sec_nodes$name)

# ---- 2. Agrupación de nombres de sector en familias (para nombrar módulos) ---
family_of <- function(s) {
  s <- sub("\\s*(EUR)?_d[0-9]+$", "", s)
  s <- sub("\\s+EUR$", "", s)
  pats <- c(resid = "residencial", comm = "terciario", elect_td = "distribución eléctrica",
            "trn_" = "transporte", "regional electricity" = "red eléctrica",
            electricity = "electricidad", generation = "generación", gas = "gas",
            refin = "refino/líquidos", liquids = "refino/líquidos", biomass = "biomasa",
            coal = "carbón", H2 = "hidrógeno", industr = "industria", "process heat" = "industria",
            iron = "industria", cement = "industria", chemical = "industria", paper = "industria",
            alumin = "industria", food = "industria")
  out <- s
  done <- rep(FALSE, length(s))
  for (k in names(pats)) {
    hit <- !done & grepl(k, s, fixed = TRUE)
    out[hit] <- pats[[k]]; done[hit] <- TRUE
  }
  out
}
module_name <- function(members) {
  regs <- sort(table(sub("\\|.*$", "", members)), decreasing = TRUE)
  fams <- sort(table(family_of(sub("^[^|]*\\|", "", members))), decreasing = TRUE)
  reg <- if (regs[1] / length(members) >= 0.6) names(regs)[1] else paste(length(regs), "regiones")
  paste0(reg, " · ", paste(head(names(fams), 2), collapse = " / "))
}

esc <- function(x) gsub('"', "'", x, fixed = TRUE)

heat_class <- function(p, bins = c(2, 5, 10, 20, 40)) {
  ifelse(is.na(p), "hna", paste0("h", findInterval(abs(p), bins)))
}

# ---- 3. Generador Mermaid ------------------------------------------------------
graph_to_mermaid <- function(seed,
                             order        = 4,
                             mode         = "all",
                             only_europe  = TRUE,
                             exclude      = "water|desalinated|UnmanagedLand",
                             resolution   = 1,      # >1 = módulos más pequeños
                             max_members  = 10,     # nodos visibles por módulo desplegado
                             small_module = 3,      # módulos de <= n nodos siempre desplegados
                             expand       = NULL,   # vector de nombres de nodo cuyos módulos desplegar
                             trace_min_share = 0.25,
                             trace_depth  = 8,
                             direction    = "LR",
                             file         = NULL,
                             seed_rng     = 1) {

  se <- sector_edges
  if (only_europe) se <- se %>% filter(in_europe)
  se <- se %>%
    filter(!grepl(exclude, from_sector), !grepl(exclude, to_sector)) %>%
    mutate(f = paste(from_region, from_sector, sep = "|"),
           t = paste(to_region, to_sector, sep = "|"),
           e_flow = lnr(flow_chY, flow_ref))

  g <- graph_from_data_frame(se %>% select(f, t), directed = TRUE)
  if (!seed %in% V(g)$name) stop("Semilla no encontrada: ", seed)

  keep <- names(ego(g, order = order, nodes = seed, mode = mode)[[1]])
  E <- se %>% filter(f %in% keep, t %in% keep)
  gs <- induced_subgraph(g, keep)

  # 3a. módulos (Louvain sobre la versión no dirigida)
  gu <- simplify(as.undirected(gs, mode = "collapse"))
  set.seed(seed_rng)
  cl <- cluster_louvain(gu, resolution = resolution)
  memb <- setNames(membership(cl), V(gu)$name)
  comms <- split(names(memb), memb)

  # 3b. traza del error heredado (aguas abajo desde la semilla)
  trace <- data.frame(a = character(0), b = character(0), share = numeric(0))
  cur <- seed; seen <- seed
  for (step in seq_len(trace_depth)) {
    d <- E %>% filter(f == cur)
    if (nrow(d) == 0) break
    LS <- lmean(sum(d$flow_chY, na.rm = TRUE), sum(d$flow_ref, na.rm = TRUE))
    contrib <- (coalesce(d$flow_chY, 0) - coalesce(d$flow_ref, 0)) / LS
    tot <- sum(contrib)
    if (!is.finite(tot) || abs(tot) < 1e-9) break
    sh <- contrib / tot
    j <- which.max(sh)
    if (sh[j] < trace_min_share || d$t[j] %in% seen) break
    trace <- rbind(trace, data.frame(a = cur, b = d$t[j], share = sh[j]))
    seen <- c(seen, d$t[j]); cur <- d$t[j]
  }
  trace_nodes <- unique(c(seed, trace$a, trace$b))

  # 3c. qué se despliega y qué se pliega
  hot <- function(v) { x <- abs(pct(e_node_of[v])); x[is.na(x)] <- -1; x }
  disp <- setNames(character(length(keep)), keep)
  node_id <- setNames(paste0("N", seq_along(keep)), keep)
  groups <- list()
  for (i in seq_along(comms)) {
    m <- comms[[i]]
    gid <- paste0("M", i)
    exp_it <- any(m %in% trace_nodes) || length(m) <= small_module || any(m %in% expand)
    gr <- list(id = gid, members = m, expand = exp_it, name = module_name(m))
    if (exp_it) {
      must <- m[m %in% c(trace_nodes, expand)]
      rest <- setdiff(m, must)
      rest <- rest[order(hot(rest), decreasing = TRUE)]
      shown <- c(must, head(rest, max(0, max_members - length(must))))
      gr$shown <- shown
      gr$others <- setdiff(m, shown)
      disp[shown] <- node_id[shown]
      if (length(gr$others)) disp[gr$others] <- paste0(gid, "x")
    } else {
      disp[m] <- gid
    }
    groups[[i]] <- gr
  }

  # 3d. nodos y subgraphs
  L <- paste0("flowchart ", direction)
  for (gr in groups) {
    ev  <- e_node_of[gr$members]
    med <- if (all(is.na(ev))) NA_real_ else median(abs(pct(ev)), na.rm = TRUE)
    if (gr$expand) {
      L <- c(L, sprintf('  subgraph %s["%s (%d)"]', gr$id, esc(gr$name), length(gr$members)))
      for (v in gr$shown) {
        reg <- sub("\\|.*$", "", v); sec <- sub("^[^|]*\\|", "", v)
        p <- pct(e_node_of[v]); h <- inh_of[v]
        val <- if (is.na(p)) "s/d" else sprintf("<b>%+.1f%%</b> · her. %s", p,
                                                 ifelse(is.na(h), "s/d", sprintf("%.0f%%", 100 * h)))
        txt <- sprintf("%s<br/><small>%s</small><br/>%s", esc(sec), reg, val)
        shp <- if (v == seed) c('{{"', '"}}') else c('["', '"]')
        L <- c(L, sprintf("    %s%s%s%s:::%s", node_id[v], shp[1], txt, shp[2], heat_class(p)))
      }
      if (length(gr$others)) {
        ov <- e_node_of[gr$others]
        om <- if (all(is.na(ov))) NA_real_ else median(abs(pct(ov)), na.rm = TRUE)
        L <- c(L, sprintf('    %sx(["+%d sectores<br/>mediana %s"]):::%s', gr$id, length(gr$others),
                          ifelse(is.na(om), "s/d", sprintf("%.1f%%", om)), heat_class(om)))
      }
      L <- c(L, "  end")
    } else {
      top <- gr$members[which.max(hot(gr$members))]
      L <- c(L, sprintf('  %s[["<b>%s</b><br/>%d sectores<br/>mediana %s · máx %s<br/><small>%s</small>"]]:::%s',
                        gr$id, esc(gr$name), length(gr$members),
                        ifelse(is.na(med), "s/d", sprintf("%.1f%%", med)),
                        ifelse(is.na(e_node_of[top]), "s/d", sprintf("%.0f%%", abs(pct(e_node_of[top])))),
                        esc(sub("^[^|]*\\|", "", top)), heat_class(med)))
    }
  }

  # 3e. aristas agregadas entre lo que se ve
  tr <- trace %>% mutate(a = disp[a], b = disp[b]) %>% group_by(a, b) %>%
    summarise(share = max(share), .groups = "drop")
  agg <- E %>%
    mutate(a = disp[f], b = disp[t]) %>%
    filter(a != b) %>%
    group_by(a, b) %>%
    summarise(n = n(), e = if (all(is.na(e_flow))) NA_real_ else median(e_flow, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(tr, by = c("a", "b"))

  styles <- character(0)
  for (k in seq_len(nrow(agg))) {
    r <- agg[k, ]
    p <- pct(r$e)
    lab <- if (is.na(p)) "" else sprintf("%+.1f%%", p)
    if (!is.na(r$share)) lab <- sprintf("%s · aporta %.0f%%", lab, 100 * r$share)
    if (r$n > 1) lab <- sprintf("%s (%d)", lab, r$n)
    L <- c(L, if (nzchar(lab)) sprintf('  %s -->|"%s"| %s', r$a, lab, r$b) else sprintf("  %s --> %s", r$a, r$b))
    if (!is.na(r$share)) { col <- "#ff8c00"; w <- 5 }
    else if (is.na(p)) { col <- "#777777"; w <- 1 }
    else {
      a <- abs(p)
      w <- if (a < 5) 1 else if (a < 20) 2 else 3.5
      col <- if (p > 0) c("#f4a582", "#d6604d", "#b2182b")[findInterval(a, c(5, 20)) + 1]
             else       c("#92c5de", "#4393c3", "#2166ac")[findInterval(a, c(5, 20)) + 1]
    }
    styles <- c(styles, sprintf("  linkStyle %d stroke:%s,stroke-width:%spx", k - 1, col, w))
  }
  L <- c(L, styles)
  pal <- c("#1b0c41", "#4a0c6b", "#a52c60", "#ed6925", "#fbb61a", "#fcffa4")
  for (i in seq_along(pal))
    L <- c(L, sprintf("  classDef h%d fill:%s,stroke:#222,color:%s", i - 1, pal[i], ifelse(i <= 3, "#fff", "#000")))
  L <- c(L, "  classDef hna fill:#bbbbbb,stroke:#666,color:#000")

  txt <- paste(L, collapse = "\n")
  if (!is.null(file)) {
    writeLines(txt, file, useBytes = TRUE)
    write_mermaid_html(txt, sub("\\.mmd$", ".html", file),
                       title = paste0("Error hindcast 2021 · ", seed, " · orden ", order))
  }
  message(length(keep), " sectores en el vecindario -> ", length(groups), " módulos, ",
          nrow(agg), " aristas dibujadas; traza: ", nrow(trace), " pasos")
  invisible(list(mermaid = txt, modules = comms, trace = trace, edges = agg))
}

# ---- 4. HTML autocontenido para verlo en el navegador -------------------------
write_mermaid_html <- function(txt, file, title = "GCAM error graph") {
  js <- gsub("\\\\", "\\\\\\\\", txt)
  js <- gsub('"', '\\\\"', js)
  js <- gsub("\n", "\\\\n", js)
  html <- c(
    "<!doctype html><html><head><meta charset='utf-8'>",
    sprintf("<title>%s</title>", title),
    "<style>body{margin:0;font-family:sans-serif;background:#fafafa} #out{padding:16px} h3{margin:12px 16px}",
    ".legend{margin:0 16px;font-size:13px;color:#444}</style></head><body>",
    sprintf("<h3>%s</h3>", title),
    "<div class='legend'>Color del nodo = |error| (oscuro &lt;2% · 2–5 · 5–10 · 10–20 · 20–40 · claro &gt;40%).",
    " Aristas: rojo = sobreestima, azul = subestima, grosor = |error| del flujo.",
    " Naranja = traza del error heredado. Cajas dobles = módulos plegados.</div>",
    "<div id='out'>Cargando…</div>",
    "<script type='module'>",
    "import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11.4.1/dist/mermaid.esm.min.mjs';",
    "mermaid.initialize({startOnLoad:false, maxTextSize:500000, maxEdges:3000, securityLevel:'loose',",
    "  flowchart:{htmlLabels:true, curve:'basis'}});",
    sprintf('const def = "%s";', js),
    "const {svg} = await mermaid.render('g', def);",
    "document.getElementById('out').innerHTML = svg;",
    "</script></body></html>")
  writeLines(html, file, useBytes = TRUE)
}

# ---- 5. Ejemplos ----------------------------------------------------------------
res <- graph_to_mermaid("Iberian_Peninsula|electricity", order = 4,
                        file = file.path(base_dir, "mermaid_Iberia_electricity_o4.mmd"))
# browseURL(file.path(base_dir, "mermaid_Iberia_electricity_o4.html"))

# Desplegar además el módulo industrial de Portugal y más detalle por módulo:
# graph_to_mermaid("Iberian_Peninsula|electricity", order = 4, max_members = 14,
#                  expand = "Portugal|iron and steel",
#                  file = file.path(base_dir, "mermaid_Iberia_o4_PT_ind.mmd"))

# Módulos más finos (más subgraphs, más pequeños):
# graph_to_mermaid("Spain|delivered gas", order = 3, resolution = 1.5,
#                  file = file.path(base_dir, "mermaid_Spain_gas_o3.mmd"))

# Inspeccionar qué hay dentro de cada módulo:
# lapply(res$modules, head, 10); res$trace
