library(rgcam)
library(dplyr)
library(tidyr)

regions_eur <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czech Republic",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Netherlands",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Albania",
  "Bosnia and Herzegovina",
  "Iceland",
  "Macedonia",
  "Moldova",
  "Norway",
  "Serbia and Montenegro",
  "Turkey",
  "UK",
  "Ukraine",
  "Appenine_Peninsula",
  "British_Isles",
  "Central_Eastern_Europe",
  "Central_Western_Europe",
  "Iberian_Peninsula",
  "Northern_Europe",
  "South_Eastern_Europe",
  "Ukraine_Moldova"
)

pathToDbs <- "C:/GCAM/Nacho/outputs_gcam"
my_gcamdb_basexdb <- "gcam_EU_macroModule"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "query_outputsByTech_2015.xml"

scenariosAnalyze<-c('KLEN_comprobaciones')

prj1 <- addScenario(conn = conn, proj = 'ref_2040.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)
queries <- listQueries(prj1)

df_ref <- getQuery(prj1, 'outputs by tech') %>% ###Este ha salido de un ref diferente al del hindcating. Habría que actualizarlo al que está vinculado al hindcating
  filter(region %in% regions_eur)
df_chy <- read.csv('Data/outputs_by_tech_extendend2040.csv') %>%
  filter(region %in% regions_eur)
df_ref_long <- df_ref %>%
  filter(
    year >= 1900,
    year <= 2040,
    year == round(year)
  ) %>%
  select(
    region, sector, subsector, output,
    technology, year, value
  ) %>%
  rename(value_ref = value)


df_chy_long <- df_chy %>%
  select(region, sector, subsector, output, technology,
         starts_with("X")) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "value_chy",
    names_prefix = "X"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  filter(
    year >= 1900,
    year <= 2040,
    year == round(year)
  )



df_comparison <- df_chy_long %>%
  left_join(
    df_ref_long,
    by = c("region", "sector", "subsector", "output", "technology", "year")
  ) %>% filter(year != 2020) %>%
  mutate(value_ref = coalesce(value_ref, 0))



################### CORRELACTIONS ###################

df_correlations <- df_comparison %>%
  filter(
    !is.na(year),
    !is.na(value_chy),
    !is.na(value_ref),
    year >= 2021
  ) %>%
  group_by(
    region, sector, subsector, output, technology
  ) %>%
  summarise(
    n_years = n(),
    rho_spearman = if (
      n() >= 3 &&
      sd(value_chy) > 0 &&
      sd(value_ref) > 0
    ) {
      cor(value_chy, value_ref, method = "spearman")
    } else {
      NA_real_
    },
    .groups = "drop"
  )


df_correlations <- df_correlations %>%
  mutate(
    correlation_type = case_when(
      is.na(rho_spearman) ~ "Not calculable",
      rho_spearman < -0.8 ~ "Very strong negative (-1 to -0.8)",
      rho_spearman < -0.6 ~ "Strong negative (-0.8 to -0.6)",
      rho_spearman < -0.3 ~ "Moderate negative (-0.6 to -0.3)",
      rho_spearman < 0    ~ "Weak negative (-0.3 to 0)",
      rho_spearman < 0.3  ~ "Weak positive (0 to 0.3)",
      rho_spearman < 0.6  ~ "Moderate positive (0.3 to 0.6)",
      rho_spearman < 0.8  ~ "Strong positive (0.6 to 0.8)",
      TRUE                ~ "Very strong positive (0.8 to 1)"
    )
  )


#----- Distribución de correlaciones -----

df_correlation_summary <- df_correlations %>%
  filter(!is.na(rho_spearman)) %>%
  count(correlation_type, name = "n_series") %>%
  mutate(
    proportion = n_series / sum(n_series),
    percentage = proportion * 100
  ) %>%
  arrange(desc(n_series))

df_correlation_summary


ggplot(
  df_correlations %>% filter(!is.na(rho_spearman)),
  aes(x = rho_spearman)
) +
  geom_density(fill = "#0072B2", alpha = 0.35, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.2)
  ) +
  labs(
    title = "Distribution of Spearman correlations",
    x = "Spearman's rank correlation",
    y = "Density"
  ) +
  theme_minimal()


ggplot(
  df_correlation_summary,
  aes(
    x = reorder(correlation_type, proportion),
    y = proportion
  )
) +
  geom_col(fill = "#0072B2", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of series by correlation intensity",
    x = "Correlation type",
    y = "Proportion of series"
  ) +
  theme_minimal()

################ Graficos ###############


plot_diffs <- function(df_comparison,
                       sel_region,
                       sel_sector,
                       sel_subsector,
                       sel_output,
                       sel_technology){

  # =============================================================================
  # FILTRAR DATOS
  # =============================================================================

  df_plot <- df_comparison %>%
    filter(
      region == sel_region,
      sector == sel_sector,
      subsector == sel_subsector,
      output == sel_output,
      technology == sel_technology
    ) %>%
    select(year, value_chy, value_ref) %>%
    pivot_longer(
      cols = c(value_chy, value_ref),
      names_to = "source",
      values_to = "value"
    ) %>%
    mutate(
      source = recode(
        source,
        value_chy = "Hindcasting",
        value_ref = "Reference"
      )
    )


  # =============================================================================
  # PLOT
  # =============================================================================

  library(ggplot2)

  graph <- ggplot(df_plot, aes(x = year, y = value, color = source)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c(
        "Hindcasting" = "#D55E00",
        "Reference" = "#0072B2"
      )
    ) +
    labs(
      title = paste(sel_region, sel_sector, sel_subsector, sep = " | "),
      subtitle = paste(sel_output, sel_technology, sep = " | "),
      x = "Year",
      y = "Value",
      color = "Dataset"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  graph
}


sel_region    <- "Portugal"
sel_sector    <- "cement"
sel_subsector <- "cement"
sel_output    <- "cement"
sel_technology <- "cement"

plot_diffs(df_comparison,
           sel_region,
           sel_sector,
           sel_subsector,
           sel_output,
           sel_technology)


sel_region    <- "Spain"
sel_sector    <- "refined liquids transport"
sel_subsector <- "refined liquids transport"
sel_output    <- "refined liquids transport"
sel_technology <- "refined liquids transport"

plot_diffs(df_comparison,
           sel_region,
           sel_sector,
           sel_subsector,
           sel_output,
           sel_technology)
