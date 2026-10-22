library(dplyr)
library(ggplot2)


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
  "top_technologies_MAE.png",
  plot_techno_MAE,
  width = 12,
  height = 8,
  dpi = 300
)
