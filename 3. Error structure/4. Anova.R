library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ranger)


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


all_errors <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv')  %>%
  select(where(~ !all(is.na(.)))) %>% filter (region %in% regions_eur) %>%
  select ("region","sector","subsector",  "technology", "output",  "abs_error")

all_errors_non_zero <- all_errors %>% filter(abs_error > 0.001)



#----- ANOVA individual por factor -----

factores <- c(
  "region",
  "sector",
  "subsector",
  "technology",
  "output"
)

resultados_anova <- lapply(factores, function(variable) {

  modelo <- aov(
    reformulate(variable, response = "log_abs_error"),
    data = df_anova
  )

  tabla <- summary(modelo)[[1]]

  ss_factor <- tabla[1, "Sum Sq"]
  ss_residual <- tabla["Residuals", "Sum Sq"]

  data.frame(
    factor = variable,
    n_categorias = nlevels(df_anova[[variable]]),
    n = nrow(df_anova),
    sum_sq = ss_factor,
    F = tabla[1, "F value"],
    p_value = tabla[1, "Pr(>F)"],
    eta2 = ss_factor / (ss_factor + ss_residual)
  )
})

resultados_anova <- do.call(rbind, resultados_anova)

resultados_anova <- resultados_anova[
  order(-resultados_anova$eta2),
]

print(resultados_anova, row.names = FALSE)

############
#----- modelo factorial aditivo -----

modelo_factorial <- aov(
  log_abs_error ~ region + sector + subsector +
    technology + output,
  data = df_anova
)

summary(modelo_factorial)










