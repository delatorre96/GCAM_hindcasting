library(dplyr)
library(tidyr)
library(rmap)
library(ggplot2)
library(sf)
library(rnaturalearth)



if (!dir.exists("Graphics")) {
  dir.create("Graphics")
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
  ~region,                   ~map_name,
  "Serbia and Montenegro",   "Serbia",
  "Serbia and Montenegro",   "Montenegro"
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
    title = "Mean Absolute Error (MAE) by GCAM region",
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


#----- mapa 2: total_abs_error por region -----

map_total_abs_error <- ggplot(map_sf) +
  geom_sf(
    aes(fill = total_abs_error),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey90",
    name = "Total absolute error",
    labels = scales::label_number(big.mark = ",", accuracy = 1)
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(30, 72),
    expand = FALSE
  ) +
  labs(
    title = "Total Absolute Error by GCAM region",
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



#----- mapa 3: proporcion del error absoluto atribuible a errores positivos -----

map_positive_error_magnitude <- ggplot(map_sf) +
  geom_sf(
    aes(fill = positive_error_share),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(0, 100),
    na.value = "grey90",
    name = "Share of absolute error (%)",
    labels = scales::label_number(accuracy = 1)
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(30, 72),
    expand = FALSE
  ) +
  labs(
    title = "Contribution of positive errors to total absolute error",
    subtitle = "GCAM-Europe hindcasting",
    caption = "Percentage of the total absolute error attributable to positive errors."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )


#----- mapa 4: proporcion de errores positivos por numero de observaciones -----

map_positive_error_frequency <- ggplot(map_sf) +
  geom_sf(
    aes(fill = pct_positive),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(0, 100),
    na.value = "grey90",
    name = "Positive errors (%)",
    labels = scales::label_number(accuracy = 1)
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(30, 72),
    expand = FALSE
  ) +
  labs(
    title = "Frequency of positive errors",
    subtitle = "GCAM-Europe hindcasting",
    caption = "Percentage of observations with a positive relative error."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )


#----- mostrar mapas -----

print(map_positive_error_magnitude)
print(map_positive_error_frequency)

#----- mostrar mapas -----

print(map_MAE)
print(map_total_abs_error)
ggsave("Graphics/map_MAE.png", map_MAE,
       width = 10, height = 8, dpi = 300)

ggsave("Graphics/map_total_abs_error.png", map_total_abs_error,
       width = 10, height = 8, dpi = 300)


ggsave("Graphics/map_positive_error_frequency.png", map_positive_error_frequency,
       width = 10, height = 8, dpi = 300)

ggsave("Graphics/map_positive_error_magnitude.png", map_positive_error_magnitude,
       width = 10, height = 8, dpi = 300)

#################### By sector ####################


all_errors_by_sector <- read.csv("Data/errors_by_sector.csv")

#----- calcular porcentaje del MAE total -----

total_MAE_sectors <- sum(all_errors_by_sector$MAE, na.rm = TRUE)

#----- seleccionar los 20 sectores con mayor MAE -----

top_sector_MAE <- all_errors_by_sector %>%
  mutate(
    pct_MAE = MAE / total_MAE_sectors * 100
  ) %>%
  slice_max(order_by = MAE, n = 20, with_ties = FALSE) %>%
  mutate(
    sector = factor(
      sector,
      levels = sector[order(MAE)]
    ),
    label = sprintf("%.2f (%.2f%%)", MAE, pct_MAE)
  )

#----- representar MAE absoluto y porcentaje -----

plot_top_MAE <- ggplot(
  top_sector_MAE,
  aes(x = MAE, y = sector)
) +
  geom_col(fill = "steelblue4", width = 0.75) +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 3.5
  ) +
  labs(
    title = "Sectors with the highest MAE",
    subtitle = "Top 20 sectors in GCAM-Europe hindcasting",
    x = "Mean Absolute Error (MAE)",
    y = NULL,
    caption = paste(
      "Labels show absolute MAE and percentage of the sum",
      "of sector-level MAEs."
    )
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.30))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

#----- mostrar y guardar -----

print(plot_top_MAE)

ggsave(
  "Graphics/top_sectors_MAE.png",
  plot_top_MAE,
  width = 12,
  height = 8,
  dpi = 300
)



#################### By techno ####################

all_errors_by_technology <- read.csv('Data/errors_by_technology.csv')

total_MAE_technologies <- sum(all_errors_by_technology$MAE, na.rm = TRUE)

top_technology_MAE <- all_errors_by_technology %>%
  mutate(
    pct_MAE = MAE / total_MAE_technologies * 100
  ) %>%
  slice_max(order_by = MAE, n = 20, with_ties = FALSE) %>%
  mutate(
    technology = factor(
      technology,
      levels = technology[order(MAE)]
    ),
    label = sprintf("%.2f (%.2f%%)", MAE, pct_MAE)
  )

#----- representar MAE absoluto y porcentaje -----

plot_techno_MAE <- ggplot(
  top_technology_MAE,
  aes(x = MAE, y = technology)
) +
  geom_col(fill = "steelblue4", width = 0.75) +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 3.5
  ) +
  labs(
    title = "Technologies with the highest MAE",
    subtitle = "Top 20 technologies in GCAM-Europe hindcasting",
    x = "Mean Absolute Error (MAE)",
    y = NULL,
    caption = "Labels show absolute MAE and percentage of the sum of technology-level MAEs."
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.30))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(plot_techno_MAE)

ggsave(
  "Graphics/top_technologies_MAE.png",
  plot_techno_MAE,
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

map_dominant_sector <- ggplot(map_sector_sf) +
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
    title = "Sector with the highest MAE by GCAM region",
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

print(map_dominant_sector)

ggsave(
  "Graphics/map_dominant_sector.png",
  map_dominant_sector,
  width = 12,
  height = 8,
  dpi = 300
)



#################### By techno and region ####################



all_errors_by_regiontechnology <- read.csv('Data/errors_by_region_technology.csv')


max_MAE_by_region <- all_errors_by_regiontechnology %>%
  ungroup() %>%
  group_by(region) %>%
  slice_max(order_by = MAE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(region, technology, MAE)



#----- preparar datos cartográficos -----

map_technology_data <- max_MAE_by_region %>%
  filter(!region %in% aggregate_regions) %>%
  mutate(
    map_name = if_else(
      region %in% names(name_crosswalk),
      unname(name_crosswalk[region]),
      region
    )
  )

#----- expandir regiones que abarcan varios países -----

map_technology_data_expanded <- map_technology_data %>%
  filter(!region %in% multi_region_crosswalk$region) %>%
  bind_rows(
    map_technology_data %>%
      inner_join(
        multi_region_crosswalk,
        by = "region",
        suffix = c("", "_crosswalk")
      ) %>%
      mutate(map_name = map_name_crosswalk)
  )

#----- unir geometrías y tecnologías -----

map_technology_sf <- europe %>%
  left_join(
    map_technology_data_expanded,
    by = c("name" = "map_name")
  )

#----- mapa de tecnología dominante -----

map_dominant_technology <- ggplot(map_technology_sf) +
  geom_sf(
    aes(fill = technology),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_d(
    option = "turbo",
    na.value = "grey90",
    name = "Technology"
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(30, 72),
    expand = FALSE
  ) +
  labs(
    title = "Technology with the highest MAE by GCAM region",
    subtitle = "GCAM-Europe hindcasting",
    caption = "Each region is assigned the technology with the highest mean absolute error."
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

print(map_dominant_technology)

ggsave(
  "Graphics/map_dominant_technology.png",
  map_dominant_technology,
  width = 12,
  height = 8,
  dpi = 300
)




