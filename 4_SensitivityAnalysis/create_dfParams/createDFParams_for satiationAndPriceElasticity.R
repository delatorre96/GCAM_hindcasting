source('C:/GCAM/Nacho/GCAM_sensitivity_analysis/R/Experiment/0_mainFunctions.R')
source('C:/GCAM/Nacho/GCAM_sensitivity_analysis/R/Experiment/3_createDF_paramsXML.R')
check_packages()
set_gcam_paths('C:/GCAM/Nacho/gcam_europe')

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
  "Ukraine"
)

xml_files <- st1_get_xml_files(config_file = config_file)
xml_files_EUR <- xml_files[grepl("EUR", xml_files)]

interested_terms <- c('logit-exponent', 'price-elasticity', 'satiation-level')


xmls_by_value_EUR <- st1_get_xmls_with_value(xml_files = xml_files_EUR, 
                                         dir_xml = dir_xml, 
                                         value = interested_terms)

#For price-elasticity: "transportation_UCD_CORE_EUR.xml"
#For satiation-level: "building_det_EUR.xml"

xml_file_path <- file.path(dir_xml, "transportation_UCD_CORE_EUR.xml")
df_price_elas <- st2_extract_price_elasticity_anyXML(xml_file_path) %>% 
  filter(year == 2021) %>%
  mutate(destination_file = sub("\\.xml$", "_cal.xml", xml_file))  %>% 
  rename('param' = 'price_elasticity') %>% mutate (type_of_param = 'price_elasticity')
# write.csv(df_price_elas, 'df_params_price_elasticity.csv', row.names = FALSE)

xml_file_path <- file.path(dir_xml, "building_det_EUR.xml")
df_satiationlevel <- st2_extract_satiation_level(xml_file_path) %>%
  mutate(destination_file = sub("\\.xml$", "_cal.xml", xml_file))   %>% 
  rename('param' = 'satiation_level') %>% mutate (type_of_param = 'satiation_level')
# write.csv(df_satiationlevel, 'df_params_satiation_level.csv', row.names = FALSE)

df_logits <- createDF_params(xmls_by_value_EUR$`logit-exponent`, regions_eur) %>% 
  rename('param' = 'logit')  %>% mutate (type_of_param = 'logit')

df_allParams <- bind_rows (df_logits,
           df_price_elas,
           df_satiationlevel)

write.csv(df_allParams, 'df_allParams.csv', row.names = FALSE)





