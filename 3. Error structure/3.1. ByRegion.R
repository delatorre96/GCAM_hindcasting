library(dplyr)
library(tidyr)
library(rmap)
library(ggplot2)
library(sf)
library(rnaturalearth)





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
ggsave("map_MAE.png", map_MAE,
       width = 10, height = 8, dpi = 300)

ggsave("map_total_abs_error.png", map_total_abs_error,
       width = 10, height = 8, dpi = 300)


ggsave("map_positive_error_frequency.png", map_positive_error_frequency,
       width = 10, height = 8, dpi = 300)

ggsave("map_positive_error_magnitude.png", map_positive_error_magnitude,
       width = 10, height = 8, dpi = 300)



