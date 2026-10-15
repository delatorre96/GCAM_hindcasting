library(dplyr)
library(tidyr)


inputsPerParam <- readRDS("gradient_boosting_models.rds")

df_allParams <- read.csv('create_dfParams/df_allParams.csv')

logit_param <- inputsPerParam$logit$drivers #%>% select (xml_file, xpath)
sat_param <- inputsPerParam$satiation_level$drivers# %>% select (xml_file, xpath)
price_param <- inputsPerParam$price_elasticity$drivers #%>% select (xml_file, xpath)

params_mostSens <- bind_rows(logit_param,
                             sat_param,
                             price_param) %>%
  mutate (xpath_xfile = paste(xml_file, xpath, sep = "_"))

df_mostSensParams <- df_allParams  %>%
  mutate (xpath_xfile = paste(xml_file, xpath, sep = "_")) %>% filter(xpath_xfile %in% unique(params_mostSens$xpath_xfile))

write.csv(df_mostSensParams, 'create_dfParams/df_mostSensParams.csv')


