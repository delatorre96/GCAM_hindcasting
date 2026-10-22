library(dplyr)
library(ggplot2)

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
  "top_sectors_MAE.png",
  plot_top_MAE,
  width = 12,
  height = 8,
  dpi = 300
)
