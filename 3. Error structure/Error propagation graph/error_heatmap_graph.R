# =============================================================================
# "Ola de calor" del error de hindcast sobre el grafo sectorial de GCAM-Europe
# -----------------------------------------------------------------------------
# Nodo   = región|sector.  Tamaño, brillo y halo = |error| del nodo.
#          Forma: círculo = error nacido en el nodo (reparto local)
#                 triángulo = error mayoritariamente heredado de la demanda
# Arista = flujo físico proveedor -> consumidor, coloreado por |error| del flujo.
#          (El error heredado viaja en sentido contrario a la flecha: la
#           demanda del consumidor "tira" del proveedor.)
# Etiquetas: grandes para los nodos más calientes, pequeñas para los
#            siguientes, ninguna para el resto.
#
# Error: e = ln(value_chY / value_ref)   (> 0 sobreestima la ejecución base 2015)
# =============================================================================

library(dplyr)
library(readr)
library(igraph)
library(ggraph)      # install.packages(c("ggraph", "graphlayouts", "ggrepel"))
library(ggplot2)

base_dir <- "C:/GCAM/Nacho/Hindcasting/3. Error structure/Error propagation graph"

nodes        <- read_csv(file.path(base_dir, "gcam_graph_nodes.csv"), guess_max = 200000, show_col_types = FALSE)
sector_edges <- read_csv(file.path(base_dir, "gcam_graph_sector_edges.csv"), guess_max = 200000, show_col_types = FALSE)

lnr <- function(a, b) {
  ok <- !is.na(a) & !is.na(b) & a > 0 & b > 0
  out <- rep(NA_real_, length(a)); out[ok] <- log(a[ok] / b[ok]); out
}

# ---- 1. Error por nodo región|sector (heredado vs local) ---------------------
#   e_node   = error del output total del sector en la región
#   e_market = error de la oferta total del mercado al que vende (heredado)
#   e_local  = e_node - e_market  (cuota de esa región/sector en su mercado)
mkt <- nodes %>%
  filter(node_type == "market") %>%
  transmute(sells_to_market = node_id, e_market = lnr(supply_chY, supply_ref))

sec_nodes <- nodes %>%
  filter(node_type == "technology", output_kind == "primary") %>%
  group_by(region, sector) %>%
  summarise(q_ref = sum(value_ref, na.rm = TRUE),
            q_chY = sum(value_chY, na.rm = TRUE),
            Units = first(Units),
            sells_to_market = first(sells_to_market),
            .groups = "drop") %>%
  left_join(mkt, by = "sells_to_market") %>%
  mutate(name     = paste(region, sector, sep = "|"),
         e_node   = lnr(q_chY, q_ref),
         e_local  = e_node - e_market,
         abs_e    = abs(e_node),
         share_inherited = abs(e_market) / (abs(e_market) + abs(e_local)))

# ---- 2. Grafo sectorial completo ---------------------------------------------
sec_e <- sector_edges %>%
  mutate(from = paste(from_region, from_sector, sep = "|"),
         to   = paste(to_region, to_sector, sep = "|"),
         e_flow = lnr(flow_chY, flow_ref),
         abs_e_flow = abs(e_flow)) %>%
  select(from, to, good, Units, flow_ref, flow_chY, e_flow, abs_e_flow)

verts <- tibble(name = unique(c(sec_e$from, sec_e$to))) %>%
  mutate(region = sub("\\|.*$", "", name),
         sector = sub("^[^|]*\\|", "", name)) %>%
  left_join(sec_nodes %>% select(name, e_node, e_market, e_local, abs_e, share_inherited, q_ref, Units),
            by = "name")

g_sec <- graph_from_data_frame(sec_e, vertices = verts, directed = TRUE)

# ---- 3. Función de visualización ---------------------------------------------
plot_error_heat <- function(seed          = "Iberian_Peninsula|electricity",
                            order         = 10,
                            mode          = "all",
                            focus_regions = c("Spain", "Iberian_Peninsula"),
                            include_imports = TRUE,     # añade proveedores de otras regiones
                            exclude       = "water|desalinated|UnmanagedLand|^\\[exogenous\\]",
                            max_nodes     = 90,         # poda: se quedan los más calientes
                            n_big_labels  = 8,
                            n_small_labels = 25,
                            cap_quantile  = 0.95,       # satura la escala en este percentil
                            layout        = "stress",   # "stress", "fr", "kk", "sugiyama"
                            signed        = FALSE,      # TRUE: rojo sobre / azul sub-estima
                            title         = NULL,
                            seed_layout   = 42) {

  # 3a. restringir a la región (y, opcionalmente, a sus proveedores directos)
  in_focus <- V(g_sec)$region %in% focus_regions
  keep <- V(g_sec)[in_focus]
  if (include_imports) {
    imp <- unique(unlist(lapply(adjacent_vertices(g_sec, keep, mode = "in"), names)))
    keep <- unique(c(names(keep), imp))
  } else keep <- names(keep)
  keep <- keep[!grepl(exclude, sub("^[^|]*\\|", "", keep))]
  g_f <- induced_subgraph(g_sec, keep)

  if (!seed %in% V(g_f)$name) stop("El nodo semilla no está en el subgrafo: ", seed)

  # 3b. vecindario de orden 'order' alrededor de la semilla
  sub <- make_ego_graph(g_f, order = order, nodes = seed, mode = mode)[[1]]

  # 3c. poda para que sea legible: los max_nodes con más error (+ la semilla)
  if (vcount(sub) > max_nodes) {
    sc <- V(sub)$abs_e; sc[is.na(sc)] <- -1
    top <- V(sub)$name[order(sc, decreasing = TRUE)][seq_len(max_nodes)]
    sub <- induced_subgraph(sub, unique(c(seed, top)))
  }
  sub <- delete_vertices(sub, V(sub)[degree(sub) == 0 & V(sub)$name != seed])

  # 3d. atributos gráficos
  ae  <- V(sub)$abs_e
  cap <- as.numeric(quantile(ae, cap_quantile, na.rm = TRUE))
  if (!is.finite(cap) || cap <= 0) cap <- max(ae, na.rm = TRUE)
  heat <- pmin(ae, cap)
  rk   <- rank(-replace(ae, is.na(ae), -1), ties.method = "first")

  V(sub)$heat    <- heat
  V(sub)$e_plot  <- if (signed) pmax(pmin(V(sub)$e_node, cap), -cap) else heat
  V(sub)$size_pt <- ifelse(is.na(heat), 1.2, 1.5 + 13 * sqrt(heat / cap))
  V(sub)$tier    <- ifelse(rk <= n_big_labels, 1L, ifelse(rk <= n_small_labels, 2L, 3L))
  V(sub)$tipo    <- ifelse(is.na(V(sub)$share_inherited), "sin dato",
                           ifelse(V(sub)$share_inherited >= 0.5, "heredado", "nacido aquí"))
  V(sub)$label   <- ifelse(V(sub)$region %in% focus_regions, V(sub)$sector,
                           paste0(V(sub)$sector, " (", V(sub)$region, ")"))
  V(sub)$label_big <- paste0(V(sub)$label, "\n",
                             sprintf("%+.0f%%", 100 * (exp(V(sub)$e_node) - 1)),
                             "  · heredado ", sprintf("%.0f%%", 100 * V(sub)$share_inherited))
  E(sub)$heat_e  <- pmin(E(sub)$abs_e_flow, cap)

  fill_scale <- if (signed) {
    scale_fill_gradient2(low = "#2c7bb6", mid = "grey85", high = "#d7191c", midpoint = 0,
                         limits = c(-cap, cap), na.value = "grey40", name = "error nodo\n(ln chY/ref)")
  } else {
    scale_fill_viridis_c(option = "inferno", limits = c(0, cap), na.value = "grey40",
                         name = "|error| nodo")
  }

  set.seed(seed_layout)
  p <- ggraph(sub, layout = layout) +
    # aristas: el calor recorre los flujos
    geom_edge_fan(aes(edge_colour = heat_e, edge_alpha = heat_e),
                  edge_width = 0.45,
                  arrow = arrow(length = unit(1.4, "mm"), type = "closed"),
                  end_cap = circle(2.2, "mm")) +
    scale_edge_colour_viridis(option = "inferno", limits = c(0, cap), na.value = "grey35",
                              name = "|error| flujo") +
    scale_edge_alpha(range = c(0.12, 0.85), guide = "none", na.value = 0.08) +
    # halo de calor (tres capas difusas)
    geom_node_point(aes(size = size_pt * 3.0, colour = heat), alpha = 0.05, show.legend = FALSE) +
    geom_node_point(aes(size = size_pt * 2.0, colour = heat), alpha = 0.10, show.legend = FALSE) +
    geom_node_point(aes(size = size_pt * 1.4, colour = heat), alpha = 0.22, show.legend = FALSE) +
    scale_colour_viridis_c(option = "inferno", limits = c(0, cap), na.value = "grey40") +
    # núcleo del nodo
    geom_node_point(aes(size = size_pt, fill = e_plot, shape = tipo), colour = "grey10", stroke = 0.3) +
    scale_size_identity() +
    scale_shape_manual(values = c("nacido aquí" = 21, "heredado" = 24, "sin dato" = 22),
                       name = "origen del error") +
    fill_scale +
    # etiquetas por niveles de calor
    geom_node_text(aes(label = label_big, filter = tier == 1L), repel = TRUE, colour = "white",
                   size = 3.8, fontface = "bold", lineheight = 0.9, max.overlaps = Inf,
                   box.padding = 0.6, segment.colour = "grey70", segment.size = 0.3) +
    geom_node_text(aes(label = label, filter = tier == 2L), repel = TRUE, colour = "grey85",
                   size = 2.5, max.overlaps = Inf, segment.colour = "grey50", segment.size = 0.2) +
    guides(shape = guide_legend(override.aes = list(size = 4, fill = "grey70"))) +
    theme_graph(background = "grey8", text_colour = "white", base_family = "sans") +
    theme(legend.position = "right",
          legend.text = element_text(colour = "white"),
          legend.title = element_text(colour = "white"),
          plot.caption = element_text(colour = "grey70", size = 8)) +
    labs(title = title %||% paste0("Error de hindcast 2021 · ", paste(focus_regions, collapse = " + "),
                                   " · semilla: ", seed),
         subtitle = paste0(vcount(sub), " nodos de ", vcount(g_f),
                           " · escala saturada en el percentil ", 100 * cap_quantile,
                           " (|e| = ", round(cap, 3), ")"),
         caption = paste("Tamaño y brillo = |ln(chY/ref)| del output del sector.",
                         "Triángulo = >50% del error heredado de la demanda de su mercado; círculo = nacido en el reparto local.",
                         "Flechas = flujo físico; el error heredado viaja en sentido contrario.", sep = "\n"))
  p
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- 4. Ejemplos --------------------------------------------------------------
p1 <- plot_error_heat(seed = "Iberian_Peninsula|electricity", order = 10)
print(p1)
ggsave(file.path(base_dir, "heat_Spain_electricity.png"), p1, width = 16, height = 11, dpi = 220, bg = "grey8")

# Disposición jerárquica: proveedores arriba, consumidores abajo (se ve cómo
# el calor sube desde la demanda final hacia los sectores primarios)
p2 <- plot_error_heat(seed = "Iberian_Peninsula|electricity", order = 10,
                      layout = "sugiyama", max_nodes = 60)
ggsave(file.path(base_dir, "heat_Spain_electricity_sugiyama.png"), p2, width = 18, height = 11, dpi = 220, bg = "grey8")

# Con signo (sobre/sub-estimación) en lugar de magnitud
p3 <- plot_error_heat(seed = "Spain|delivered gas", order = 6, signed = TRUE)
ggsave(file.path(base_dir, "heat_Spain_gas_signed.png"), p3, width = 16, height = 11, dpi = 220, bg = "grey8")

# Otra región, p. ej. Francia sin la red regional:
# plot_error_heat(seed = "France|delivered gas", focus_regions = "France", order = 8)

# ---- 5. Tabla de los nodos más calientes del subgrafo (para leer los números) --
hot_table <- function(focus_regions = c("Spain", "Iberian_Peninsula"), n = 30) {
  sec_nodes %>%
    filter(region %in% focus_regions, !is.na(e_node)) %>%
    arrange(desc(abs_e)) %>%
    transmute(region, sector, Units, q_ref, error_pct = 100 * (exp(e_node) - 1),
              e_node, e_market, e_local, share_inherited) %>%
    head(n)
}
print(hot_table(), n = 30)
