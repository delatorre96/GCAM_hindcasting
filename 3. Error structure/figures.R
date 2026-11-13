library(dplyr)
library(tidyr)
library(rmap)
library(ggplot2)
library(sf)
library(rnaturalearth)


if (!dir.exists("Figures")) {
  dir.create("Figures")
}
if (!dir.exists("Figures/iamc")) {
  dir.create("Figures/iamc")
}

#################### By region ####################

all_errors_by_region <- read.csv('Data/errors_by_region.csv')



aggregate_regions <- c(
  "British_Isles",
  "Central_Western_Europe",
  "Appenine_Peninsula",
  "Iberian_Peninsula",
  "Ukraine_Moldova",
  "South_Eastern_Europe",
  "Central_Eastern_Europe",
  "Northern_Europe"
)



#----- crosswalk de nombres geográficos -----

name_crosswalk <- c(
  "UK" = "United Kingdom",
  "Czech Republic" = "Czechia",
  "Macedonia" = "North Macedonia",
  "Turkey" = "Turkey",
  "Bosnia and Herzegovina" = "Bosnia and Herz."
)

#----- preparar datos para el mapa -----

map_data <- all_errors_by_region %>%
  filter(!region %in% aggregate_regions) %>%
  mutate(
    map_name = if_else(
      region %in% names(name_crosswalk),
      unname(name_crosswalk[region]),
      region
    )
  )

#----- resolver regiones que abarcan varios países -----


multi_region_crosswalk <- tibble::tribble(
  ~region,                 ~map_name,
  "Serbia and Montenegro", "Serbia",
  "Serbia and Montenegro", "Montenegro",
  "Serbia and Montenegro", "Kosovo"
)


map_data <- all_errors_by_region %>%
  filter(!region %in% aggregate_regions) %>%
  mutate(
    map_name = if_else(
      region %in% names(name_crosswalk),
      unname(name_crosswalk[region]),
      region
    )
  )


map_data_expanded <- map_data %>%
  filter(!region %in% multi_region_crosswalk$region) %>%
  bind_rows(
    map_data %>%
      inner_join(multi_region_crosswalk, by = "region",
                 suffix = c("", "_crosswalk")) %>%
      mutate(map_name = map_name_crosswalk)
  )




world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)



europe <- world %>%
  filter(
    st_intersects(
      geometry,
      st_as_sfc(st_bbox(
        c(xmin = -25, ymin = 30, xmax = 45, ymax = 72),
        crs = st_crs(world)
      )),
      sparse = FALSE
    )[, 1]
  )

europe <- bind_rows(
  europe,
  world %>%
    filter(name %in% c("Libya", "Egypt"))
)

unmatched <- map_data_expanded %>%
  anti_join(
    st_drop_geometry(europe) %>%
      transmute(map_name = name),
    by = "map_name"
  )

unmatched %>%
  select(region, map_name)


################### Dibujar mapas
map_sf <- europe %>%
  left_join(
    map_data_expanded,
    by = c("name" = "map_name")
  )

#----- mapa 1: MAE por region -----

map_MAE <- ggplot(map_sf) +
  geom_sf(aes(fill = MAE), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey90",
    name = "MAE",
    labels = scales::label_number(big.mark = ",", accuracy = 0.01)
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(30, 72),
    expand = FALSE
  ) +
  labs(
    title = "Mean Absolute Error (MAE) by region",
    subtitle = "GCAM-Europe hindcasting"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )


#----- mapa 2: pct ch por region sin aglunas regiones atípicas -----
map_sf_withoutSomeRegions <- europe %>%
  left_join(
    map_data_expanded,
    by = c("name" = "map_name")
  ) %>%
  mutate(
    across(
      c(total_abs_error, abs_pct_ch_mean, net_pct_ch_mean,
        MAE, RMSE, positive_error_share, negative_error_share,
        pct_positive, weighted_abs_pct_ch_mean),
      ~ ifelse(name %in% c("Albania", "Iceland", "Turkey"), NA, .)
    )
  )

map_pct_change <- ggplot(map_sf_withoutSomeRegions) +
  geom_sf(aes(fill = weighted_abs_pct_ch_mean * 100),
          color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey90",
    name = "Weighted absolute\nchange (%)",
    labels = scales::label_number(
      accuracy = 0.001,
      suffix = "%"
    )
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(30, 72),
    expand = FALSE
  ) +
  labs(
    title = "Weighted Mean Absolute Percentage Change by region",
    subtitle = "GCAM-Europe hindcasting | Weights based on reference values"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )

map_pct_change




library(scales)


plot_data <- all_errors_by_region %>%
  arrange(desc(abs_pct_ch_mean)) %>%
  slice_head(n = 20) %>%
  mutate(
    region = reorder(region, abs_pct_ch_mean)
  ) %>%
  select(
    region,
    abs_pct_ch_mean,
    MAE
  ) %>%
  pivot_longer(
    cols = -region,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(
    normalized_value = scales::rescale(value, to = c(0, 1))
  ) %>%
  ungroup()

bar_plot_regionChars <- ggplot(
  plot_data,
  aes(
    x = normalized_value,
    y = region,
    fill = variable
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "abs_pct_ch_mean" = "#2878B5",
      "MAE" = "#E69F00"
    ),
    labels = c(
      "abs_pct_ch_mean" = "Absolute percentage change",
      "MAE" = "Mean Absolute Error (MAE)"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    title = "Regional error characteristics",
    subtitle = "20 regions with highest absolute percentage change",
    x = "Normalized value (Min-Max)",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


library(ggrepel)

relation_MAE_change <- ggplot(
  all_errors_by_region,
  aes(
    x = MAE,
    y = abs_pct_ch_mean,
    label = region
  )
) +
  geom_point(
    size = 3.2,
    color = "#2878B5",
    alpha = 0.85
  ) +
  geom_text_repel(
    size = 3,
    color = "#37474F",
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "grey75",
    segment.size = 0.3
  ) +
  scale_x_continuous(
    trans = "log10",
    expand = expansion(mult = c(0.08, 0.15))
  ) +
  labs(
    title = "Absolute vs relative regional errors",
    subtitle = "Relationship between MAE and mean absolute percentage change",
    x = "Mean Absolute Error (log scale)",
    y = "Mean absolute percentage change"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = "#263238"
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "grey40",
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 10),
    axis.text = element_text(
      size = 9,
      color = "#59636B"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "grey90",
      linewidth = 0.35
    ),
    plot.margin = margin(15, 20, 10, 10)
  )




#----- guardar mapas -----

ggsave("Figures/map_MAE.png", map_MAE,
       width = 10, height = 8, dpi = 300)

ggsave("Figures/map_pct_change.png", map_pct_change,
       width = 10, height = 8, dpi = 300)

ggsave("Figures/bar_plot_regionChars.png", bar_plot_regionChars,
       width = 10, height = 8, dpi = 300)

ggsave("Figures/relation_MAE_change.png", relation_MAE_change,
       width = 10, height = 8, dpi = 300)

#################### By sector ####################


all_errors_by_sector <- read.csv("Data/errors_by_sector.csv")
#mapping_IAMC



#----- calcular porcentaje del MAE total -----



#----- seleccionar los 20 sectores con mayor MAE -----
top_sector_pct_change <- all_errors_by_sector %>%
  slice_max(
    order_by = abs_pct_ch_mean,
    n = 10,
    with_ties = FALSE
  ) %>%
  mutate(
    sector = factor(
      sector,
      levels = sector[order(abs_pct_ch_mean)]
    )
  ) %>%
  select(
    sector,
    abs_pct_ch_mean,
    MAE
  ) %>%
  pivot_longer(
    cols = c(abs_pct_ch_mean, MAE),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(
    normalized_value = scales::rescale(value, to = c(0, 1))
  ) %>%
  ungroup()

#----- representar MAE y porcentaje -----

plot_top_MAE <- ggplot(
  top_sector_pct_change,
  aes(
    x = normalized_value,
    y = sector,
    fill = variable
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.65
  ) +
  scale_fill_manual(
    values = c(
      "abs_pct_ch_mean" = "#2878B5",
      "MAE" = "#D69A45"
    ),
    labels = c(
      "abs_pct_ch_mean" = "Mean absolute percentage change",
      "MAE" = "Mean Absolute Error (MAE)"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    title = "Top 10 sectors with both highest error",
    subtitle = "Highest MAE and absolute percentage change",
    x = "Normalized value (Min-Max)",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = "#263238"
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "grey40",
      margin = margin(b = 15)
    ),
    axis.text.y = element_text(
      size = 9,
      color = "#37474F"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "grey90",
      linewidth = 0.35
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 9)
  )

#----- mostrar y guardar -----

print(plot_top_MAE)

ggsave(
  "Figures/top_sectors_MAE.png",
  plot_top_MAE,
  width = 12,
  height = 8,
  dpi = 300
)


#################### By sector and region ####################


all_errors_by_regionsector <- read.csv('Data/errors_by_region_sector.csv')


max_MAE_by_region <- all_errors_by_regionsector %>%
  ungroup() %>%
  group_by(region) %>%
  slice_max(order_by = MAE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(region, sector, MAE)



#----- preparar datos cartográficos -----

map_sector_data <- max_MAE_by_region %>%
  filter(!region %in% aggregate_regions) %>%
  mutate(
    map_name = if_else(
      region %in% names(name_crosswalk),
      unname(name_crosswalk[region]),
      region
    )
  )

#----- expandir regiones que abarcan varios países -----

map_sector_data_expanded <- map_sector_data %>%
  filter(!region %in% multi_region_crosswalk$region) %>%
  bind_rows(
    map_sector_data %>%
      inner_join(
        multi_region_crosswalk,
        by = "region",
        suffix = c("", "_crosswalk")
      ) %>%
      mutate(map_name = map_name_crosswalk)
  )

#----- unir geometrías y tecnologías -----

map_sector_sf <- europe %>%
  left_join(
    map_sector_data_expanded,
    by = c("name" = "map_name")
  )

#----- mapa de tecnología dominante -----

map_dominant_sector_MAE <- ggplot(map_sector_sf) +
  geom_sf(
    aes(fill = sector),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_d(
    option = "turbo",
    na.value = "grey90",
    name = "Sector"
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(30, 72),
    expand = FALSE
  ) +
  labs(
    title = "Sector with the highest MAE by region",
    subtitle = "GCAM-Europe hindcasting",
    caption = "Each region is assigned the sector with the highest mean absolute error."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 8)
  )

print(map_dominant_sector_MAE)

ggsave(
  "Figures/map_dominant_sector_MAE.png",
  map_dominant_sector_MAE,
  width = 12,
  height = 8,
  dpi = 300
)




max_abs_pct_ch_mean_by_region <- all_errors_by_regionsector %>%
  ungroup() %>%
  group_by(region) %>%
  slice_max(order_by = abs_pct_ch_mean , n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(region, sector, abs_pct_ch_mean )



#----- preparar datos cartográficos -----

map_sector_data <- max_abs_pct_ch_mean_by_region %>%
  filter(!region %in% aggregate_regions) %>%
  mutate(
    map_name = if_else(
      region %in% names(name_crosswalk),
      unname(name_crosswalk[region]),
      region
    )
  )

#----- expandir regiones que abarcan varios países -----

map_sector_data_expanded <- map_sector_data %>%
  filter(!region %in% multi_region_crosswalk$region) %>%
  bind_rows(
    map_sector_data %>%
      inner_join(
        multi_region_crosswalk,
        by = "region",
        suffix = c("", "_crosswalk")
      ) %>%
      mutate(map_name = map_name_crosswalk)
  )

#----- unir geometrías y tecnologías -----

map_sector_sf <- europe %>%
  left_join(
    map_sector_data_expanded,
    by = c("name" = "map_name")
  )

#----- mapa de tecnología dominante -----

map_dominant_sector_pct_change <- ggplot(map_sector_sf) +
  geom_sf(
    aes(fill = sector),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_d(
    option = "turbo",
    na.value = "grey90",
    name = "Sector"
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(30, 72),
    expand = FALSE
  ) +
  labs(
    title = "Sector with the highest % change by region",
    subtitle = "GCAM-Europe hindcasting",
    caption = "Each region is assigned the sector with the highest % change error."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 8)
  )

print(map_dominant_sector_pct_change)

ggsave(
  "Figures/map_dominant_sector_pct_change.png",
  map_dominant_sector_pct_change,
  width = 12,
  height = 8,
  dpi = 300
)






#
# #################### By techno and region ####################
#
#
#
# all_errors_by_regiontechnology <- read.csv('Data/errors_by_region_technology.csv')
#
#
# max_MAE_by_region <- all_errors_by_regiontechnology %>%
#   ungroup() %>%
#   group_by(region) %>%
#   slice_max(order_by = MAE, n = 1, with_ties = FALSE) %>%
#   ungroup() %>%
#   select(region, technology, MAE)
#
#
#
# #----- preparar datos cartográficos -----
#
# map_technology_data <- max_MAE_by_region %>%
#   filter(!region %in% aggregate_regions) %>%
#   mutate(
#     map_name = if_else(
#       region %in% names(name_crosswalk),
#       unname(name_crosswalk[region]),
#       region
#     )
#   )
#
# #----- expandir regiones que abarcan varios países -----
#
# map_technology_data_expanded <- map_technology_data %>%
#   filter(!region %in% multi_region_crosswalk$region) %>%
#   bind_rows(
#     map_technology_data %>%
#       inner_join(
#         multi_region_crosswalk,
#         by = "region",
#         suffix = c("", "_crosswalk")
#       ) %>%
#       mutate(map_name = map_name_crosswalk)
#   )
#
# #----- unir geometrías y tecnologías -----
#
# map_technology_sf <- europe %>%
#   left_join(
#     map_technology_data_expanded,
#     by = c("name" = "map_name")
#   )
#
# #----- mapa de tecnología dominante -----
#
# map_dominant_technology <- ggplot(map_technology_sf) +
#   geom_sf(
#     aes(fill = technology),
#     color = "white",
#     linewidth = 0.2
#   ) +
#   scale_fill_viridis_d(
#     option = "turbo",
#     na.value = "grey90",
#     name = "Technology"
#   ) +
#   coord_sf(
#     xlim = c(-25, 45),
#     ylim = c(30, 72),
#     expand = FALSE
#   ) +
#   labs(
#     title = "Technology with the highest MAE by region",
#     subtitle = "GCAM-Europe hindcasting",
#     caption = "Each region is assigned the technology with the highest mean absolute error."
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid = element_blank(),
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     plot.title = element_text(face = "bold"),
#     legend.text = element_text(size = 8)
#   )
#
# print(map_dominant_technology)
#
# ggsave(
#   "Figures/map_dominant_technology.png",
#   map_dominant_technology,
#   width = 12,
#   height = 8,
#   dpi = 300
# )
#
#
#
#
