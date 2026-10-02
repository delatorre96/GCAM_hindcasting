library(rgcam)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(patchwork)


prj1 <- loadProject(proj = "BaseYear2015_outputsByTech.dat")



all_results = list()
query = "outputs by tech"

df <- getQuery(prj1, query)
df <- df %>% filter(year == 2021)


key_cols <- colnames(df)
key_cols <- key_cols[!key_cols %in% c("scenario", "value")]

scenario_2015 <- setdiff(unique(df$scenario), 'Reference')

# separar escenarios
df_ref <- df %>%
  filter(scenario == "Reference") %>%
  select(-scenario) %>%
  rename(value_ref = value)

df_chY <- df %>%
  filter(scenario == scenario_2015) %>%
  select(-scenario) %>%
  rename(value_chY = value)

df_comp <- df_ref %>%
  inner_join(df_chY, by = key_cols)

# error base
df_comp <- df_comp %>%
  mutate(
    error = value_ref - value_chY,
    abs_error = abs(error),
    sq_error = error^2,
    bias_ratio =  ifelse(
      value_chY == 0 & value_ref == 0,
      0,
      value_chY / value_ref
    ),
    rel_error = ifelse(error == 0 & value_ref == 0, 0,error / value_ref),
    query = query
  )

all_errors_output_by_tech <- df_comp %>%
  select(
    region,
    sector,
    subsector,
    technology,
    output,
    value_ref,
    value_chY,
    error,
    abs_error,
    rel_error
  )



if (!dir.exists("Data")) {
  dir.create("Data")
}
write.csv(all_errors_output_by_tech, 'Data/all_errors_output_by_tech.csv', row.names = FALSE)
