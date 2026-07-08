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

cols_final <- c(variables, "MAE", "RMSE", "bias_dir", "query", "n")

df_final <- as.data.frame(matrix(ncol = length(cols_final), nrow = 0))
colnames(df_final) <- cols_final



result_list <- list()
# i <- 1
#
#
# for (query in queries) {
#
#
#   df <- getQuery(prj1, query)
#
#   if (all(c("BaseYear2015", "Reference") %in% unique(df$scenario))){
#
#     key_cols <- colnames(df)
#     key_cols <- key_cols[!key_cols %in% c("scenario", "value")]
#
#     if ("technology" %in% key_cols) {
#
#       if (all(grepl("=", df$technology, fixed = TRUE), na.rm = TRUE)) {
#         key_cols <- setdiff(key_cols, "technology")
#         df <- df %>% select (-technology)
#       }
#     }
#
#     # separar escenarios
#     df_ref <- df %>%
#       filter(scenario == "Reference") %>%
#       select(-scenario) %>%
#       rename(value_ref = value)
#
#     df_chY <- df %>%
#       filter(scenario == "BaseYear2015") %>%
#       select(-scenario) %>%
#       rename(value_chY = value)
#
#     df_comp <- df_ref %>%
#       inner_join(df_chY, by = key_cols) %>%
#       filter(is.finite(value_ref), is.finite(value_chY)) %>%
#       group_by(across(all_of(setdiff(key_cols, "year")))) %>%
#       summarise(
#
#         n = n(),
#
#         MAE = if (n > 0) {
#           mean(abs(value_ref - value_chY))
#         } else {
#           NA_real_
#         },
#
#         RMSE = if (n > 0) {
#           sqrt(mean((value_ref - value_chY)^2))
#         } else {
#           NA_real_
#         },
#
#         bias_dir = if (n > 0) {
#           mean(sign(value_chY - value_ref))
#         } else {
#           NA_real_
#         },
#
#         spearman_corr = if (n > 1) {
#
#           sd_ref <- sd(value_ref)
#           sd_chY <- sd(value_chY)
#
#           if (sd_ref == 0 && sd_chY == 0) {
#             1 ######### ESTA CASUÍSTICA HAY QUE INDICARLA
#           } else if (sd_ref == 0 || sd_chY == 0) {
#             0   ######### ESTA CASUÍSTICA HAY QUE INDICARLA
#           } else {
#             cor(value_ref, value_chY,
#                 method = "spearman",
#                 use = "complete.obs")
#           }
#
#         } else {
#           NA_real_
#         },
#
#         query = query,
#         .groups = "drop"
#       ) %>%
#       select(-Units)
#
#     missing_cols <- setdiff(cols_final, colnames(df_comp))
#
#     df_comp[missing_cols] <- NA
#
#     result_list[[i]] <- df_comp
#     i <- i + 1
#
#   }
#
# }
#
# df_final <- dplyr::bind_rows(result_list)
#
# write.csv(query_errors_all,"Data/errors_all.csv", row.names = FALSE)
#
#
#
#
#
#
#
#
#
#



all_results = list()

for (query in queries) {

  df <- getQuery(prj1, query)
  if ('region' %in% colnames(df)){
    df <- df %>% filter(region %in% regions_of_interest)
  }

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
      inner_join(df_chY, by = key_cols) %>%
      filter(is.finite(value_ref), is.finite(value_chY)) ###Aqui elimino Infs pero hay que estudiar qué pasa aquí


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


  # -----------------------------
  # 1. ERROR POR AÑO
  # -----------------------------
  if ('year' %in% names(df_comp)){
  year_metrics <- df_comp %>%
    group_by(query, year) %>%
    summarise(
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
      rel_MAE_RMSE = ifelse(MAE == 0 & RMSE == 0, 1, RMSE / MAE),
      bias_ratio = mean(bias_ratio[is.finite(bias_ratio)], na.rm = TRUE),
      rel_error = mean(rel_error[is.finite(rel_error)], na.rm = TRUE),
      .groups = "drop"
    ) %>% filter(year %in% c(2015, 2021))
  }else{
    year_metrics <- tibble(
      query = character(),
      year = numeric(),
      MAE = numeric(),
      RMSE = numeric(),
      bias_ratio = numeric(),
      rel_error = numeric()
    )
  }
  # -----------------------------
  # 2. ERROR POR REGIÓN
  # -----------------------------
  if ('region' %in% names(df_comp)){
  region_metrics <- df_comp %>%
    group_by(query, region) %>%
    summarise(
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
      rel_MAE_RMSE = ifelse(MAE == 0 & RMSE == 0, 1, RMSE / MAE),
      bias_ratio = mean(bias_ratio[is.finite(bias_ratio)], na.rm = TRUE),
      rel_error = mean(rel_error[is.finite(rel_error)], na.rm = TRUE),
      .groups = "drop"

    )
  df_errors <- df_comp %>%
    select(query, region, year, value_ref, value_chY, error, abs_error, rel_error, any_of(variables))

  query_errors <- df_comp %>%
    group_by(query,region, year) %>%
    summarise(
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
      rel_MAE_RMSE = ifelse(MAE == 0 & RMSE == 0, 1, RMSE / MAE),
      bias_ratio = mean(bias_ratio[is.finite(bias_ratio)], na.rm = TRUE),
      rel_error = mean(rel_error[is.finite(rel_error)], na.rm = TRUE),
      .groups = "drop"
    ) %>% select(query,MAE , RMSE, rel_error, region, year)

  }else{
    region_metrics <- tibble(
      query = character(),
      region = character(),
      MAE = numeric(),
      RMSE = numeric(),
      rel_MAE_RMSE = numeric(),
      bias_ratio = numeric(),
      rel_error = numeric()
    )
      query_errors <- tibble(
        query = character(),
        region = character(),
        year = numeric(),
        MAE = numeric(),
        RMSE = numeric(),
        bias_ratio = numeric(),
        rel_error = numeric()
    )
  }
  # -----------------------------
  # 3. ERROR POR QUERY
  # -----------------------------

  query_metrics <- df_comp %>%
    group_by(query) %>%
    summarise(
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
      rel_MAE_RMSE = ifelse(MAE == 0 & RMSE == 0, 1, RMSE / MAE),
      bias_ratio = mean(bias_ratio[is.finite(bias_ratio)], na.rm = TRUE),
      rel_error = mean(rel_error[is.finite(rel_error)], na.rm = TRUE),,
      .groups = "drop"
    )



  all_results[[query]] <- list(
    year = year_metrics,
    region = region_metrics,
    query = query_metrics,
    query_errors = query_errors,
    errors = df_errors
  )

}
}

######## Unificación ########
# query_errors_all <- bind_rows(lapply(all_results, `[[`, "query_errors"))
year_metrics_all <- bind_rows(lapply(all_results, `[[`, "year"))
region_metrics_all <- bind_rows(lapply(all_results, `[[`, "region"))
query_metrics_all <- bind_rows(lapply(all_results, `[[`, "query"))
# all_errors <- bind_rows(lapply(all_results, `[[`, "errors"))

# write.csv(query_errors_all,"Data/query_errors_all.csv", row.names = FALSE)
write.csv(year_metrics_all,"Data/year_metrics_all.csv", row.names = FALSE)
write.csv(region_metrics_all,"Data/region_metrics_all.csv", row.names = FALSE)
write.csv(query_metrics_all,"Data/query_metrics_all.csv", row.names = FALSE)
#write.csv(all_errors,"Data/all_errors.csv", row.names = FALSE)

# saveRDS(all_errors, "Data/all_errors.rds")

