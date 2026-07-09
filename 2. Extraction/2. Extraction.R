library(rgcam)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(patchwork)



######## Extracción ########
prj1 <- loadProject(proj = "BaseYear2015.dat")
queries <- listQueries(prj1)
regions_of_interest <- europe_regions <- c(
  "Albania",
  "Austria",
  "Belarus",
  "Belgium",
  "Bosnia and Herzegovina",
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
  "Iceland",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Macedonia",
  "Malta",
  "Moldova",
  "Netherlands",
  "Norway",
  "Poland",
  "Portugal",
  "Romania",
  "Serbia and Montenegro",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Switzerland",
  "Turkey",
  "UK",
  "Ukraine"
)
######## Construcción de errores ########
variables <- c()

for (query in queries) {
  df <- getQuery(prj1, query)

  cols <- colnames(df)
  cols <- cols[!cols %in% c("scenario", "value", "Units", "year")]

  variables <- c(variables, cols)
}

variables <- unique(variables)


all_results = list()

for (query in queries) {

  df <- getQuery(prj1, query)
  if ('region' %in% colnames(df)){
    df <- df %>% filter(region %in% regions_of_interest)
  }

  df <- df %>% filter(year == 2021)

  if (all(c("BaseYear2015", "Reference") %in% unique(df$scenario))) {

    key_cols <- colnames(df)
    key_cols <- key_cols[!key_cols %in% c("scenario", "value")]
    # separar escenarios
    df_ref <- df %>%
      filter(scenario == "Reference") %>%
      select(-scenario) %>%
      rename(value_ref = value)

    df_chY <- df %>%
      filter(scenario == "BaseYear2015") %>%
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

    df_errors <- df_comp %>%
      select(
        query,
        any_of("region"),
        year,
        value_ref,
        value_chY,
        error,
        abs_error,
        rel_error,
        any_of(variables)
      )

    all_results[[query]] <- list(
      errors = df_errors
    )


  }
}


######## Unificación ########
all_errors <- bind_rows(lapply(all_results, `[[`, "errors"))

if (!dir.exists("Data")) {
  dir.create("Data")
}
write.csv(all_errors,"Data/all_errors.csv", row.names = FALSE)
