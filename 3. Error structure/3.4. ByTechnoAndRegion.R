library(dplyr)
library(ggplot2)


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
  "map_dominant_technology.png",
  map_dominant_technology,
  width = 12,
  height = 8,
  dpi = 300
)

