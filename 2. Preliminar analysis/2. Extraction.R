library(rgcam)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(patchwork)



######## Extracción ########
prj1 <- loadProject(proj = "BaseYear2015.dat")
queries <- listQueries(prj1)

######## Construcción de errores ########
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

variables <- c()

for (query in queries) {
  df <- getQuery(prj1, query)

  cols <- colnames(df)
  cols <- cols[!cols %in% c("scenario", "value", "Units", "year")]

  variables <- c(variables, cols)
}

variables <- unique(variables)

cols_final <- c(variables, "MAE", "RMSE", "bias_dir", "query", "n")

df_final <- as.data.frame(matrix(ncol = length(cols_final), nrow = 0))
colnames(df_final) <- cols_final



result_list <- list()
i <- 1


for (query in queries) {

  df <- getQuery(prj1, query)
  if ('region' %in% colnames(df)){
    df <- df %>% filter(region %in% regions_of_interest)
  }


  if (all(c("BaseYear2015", "Reference") %in% unique(df$scenario))){

    key_cols <- colnames(df)
    key_cols <- key_cols[!key_cols %in% c("scenario", "value")]

    if ("technology" %in% key_cols) {

      if (all(grepl("=", df$technology, fixed = TRUE), na.rm = TRUE)) {
        key_cols <- setdiff(key_cols, "technology")
        df <- df %>% select (-technology)
      }
    }

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
      inner_join(df_chY, by = key_cols) %>%
      filter(is.finite(value_ref), is.finite(value_chY)) %>%
      group_by(across(all_of(setdiff(key_cols, "year")))) %>%
      summarise(

        n = n(),

        MAE = if (n > 0) {
          mean(abs(value_ref - value_chY))
        } else {
          NA_real_
        },

        RMSE = if (n > 0) {
          sqrt(mean((value_ref - value_chY)^2))
        } else {
          NA_real_
        },

        bias_dir = if (n > 0) {
          mean(sign(value_chY - value_ref))
        } else {
          NA_real_
        },

        query = query,
        .groups = "drop"
      ) %>%
      select(-Units)

    missing_cols <- setdiff(cols_final, colnames(df_comp))

    df_comp[missing_cols] <- NA

    result_list[[i]] <- df_comp
    i <- i + 1

  }

}

df_final <- dplyr::bind_rows(result_list)

write.csv(df_final,"Data/errors_indicators.csv", row.names = FALSE)

