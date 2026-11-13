# Apply the outputs_by_tech -> IAMC common-definitions mapping
library(dplyr); library(readr); library(tidyr)

map <- read_csv("gcam_outputs_by_tech_to_iamc_mapping.csv", show_col_types = FALSE) |>
  filter(use)                       # only recommended rows (no intermediates or trade components)

# obt: result of the "outputs by tech" query (e.g. rgcam::getQuery(prj, "outputs by tech"))
# expected columns: scenario, region, sector, subsector, output, technology, year, value, Units
iamc <- obt |>
  inner_join(map, by = c("sector", "subsector", "output", "technology"),
             relationship = "many-to-many") |>
  mutate(value = value * unit_conv * sign) |>
  group_by(scenario, region, variable = iamc_variable, unit = iamc_unit, year) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  mutate(model = "GCAM-Europe 7.2.0") |>          # name registered in common-definitions/mappings/GCAM
  pivot_wider(names_from = year, values_from = value)

# Coverage control: technologies in your run that are NOT in the mapping
anti_join(distinct(obt, sector, subsector, output, technology),
          read_csv("gcam_outputs_by_tech_to_iamc_mapping.csv", show_col_types = FALSE),
          by = c("sector", "subsector", "output", "technology"))
