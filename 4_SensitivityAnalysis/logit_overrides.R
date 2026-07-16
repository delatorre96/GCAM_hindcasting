source('1_2_Functions.R')
thisScriptPath <- getwd()
set_gcam_paths('C:/GCAM/Nacho/gcam_europe')

xml_files <- get_xml_files(config_path = config_file)

xml_files_withLogit <- get_xmls_with_logit(xml_files, dir_xml) 

xmls_with_logit_EUR <- xml_files_withLogit[grepl("EUR", xml_files_withLogit)]

xml_file <- paste0(dir_xml,"/en_supply_EUR.xml")
xml_file_cal <- paste0(dir_xml,"/en_supply_EUR_cal.xml")

tabla_logits <- extraer_logits(xml_file)
tabla_logits$year <- 2021
tabla_logits$logit <- tabla_logits$logit + rnorm(nrow(tabla_logits),0,0.2)

insertar_logits(xml_file,tabla_logits, xml_file_cal)













