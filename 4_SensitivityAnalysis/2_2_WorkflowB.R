library(future.apply)
library(dplyr)


source('1_2_Functions.R')
thisScript_path <- getwd()
set_gcam_paths('C:/GCAM/Nacho/gcam_europe')

if (!file.exists('df_params.csv')) {
  message('No logits table. Extracting them...')
  all_errors_output_by_tech <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv') 
  xml_files <- get_xml_files(config_path = config_file)
  xml_files_EUR <-  xml_files[grepl("EUR", xml_files)]
  xmls_with_logit_EUR <- get_xmls_with_logit(xml_files_EUR, dir_xml) 
  xml_not_to_include <- c(
    "water_td_EUR.xml",
    "EFW_irrigation_EUR.xml",
    "EFW_manufacturing_EUR.xml",
    "EFW_municipal_EUR.xml",
    "ind_urb_processing_sectors_EUR.xml"
  ) 
  xmls_with_logit_EUR <- xmls_with_logit_EUR[!xmls_with_logit_EUR %in% xml_not_to_include]
  xml_to_include <- c('ag_an_demand_input.xml')
  xmls_with_logit_EUR <- c(xmls_with_logit_EUR, xml_to_include)
  
  
  interested_subsectors <- all_errors_output_by_tech$subsector
  interested_sectors <- all_errors_output_by_tech$output
  df_params <- createDF_params(xml_files = xmls_with_logit_EUR,
                               interested_subsectors = interested_subsectors,
                               interested_sectors = interested_sectors)
}else{
  message('df_params already created. Loading logits....')
  df_params <- read.csv('df_params.csv') %>% select(-id) #%>% filter(xml_file != 'building_det_EUR.xml')
}

n_iterations <- 200
#relative_uncertainty = 0.3
plan(multisession, workers = 16)

most_important_xml <- c( "en_supply_EUR.xml","en_transformation_EUR.xml", "elec_segments_water_EUR.xml",
                       'ag_an_demand_input.xml')
less_important_xml <- setdiff( unique(df_params$xml_file), most_important_xml)


simulation_log <- data.frame(
  iteration = integer(),
  factor = numeric(),
  time_changing_xml = numeric(),
  time_gcam = numeric(),
  total_time = numeric(),
  relative_uncertainty = numeric()
)

for (i in 1:n_iterations){
  relative_uncertainty <- round(runif(1,0.6,5),1)
    message(paste0('#################################### ITERATION ', i,' ####################################'))
    message('Inducing uncertainty in the parameters...')
    t1 <- Sys.time()
    factor <- runif(
      1,
      1 - relative_uncertainty,
      1 + relative_uncertainty
    )
    while (factor < 0) {
      factor <- runif(
        1,
        1 - relative_uncertainty,
        1 + relative_uncertainty
      )
    }
    
    df_params_copy <- df_params
    df_params_copy$year  <- 2021
    df_params_copy$logit <- round(df_params_copy$logit * factor, 2)
    
    xml_files_set_all <- unique(df_params_copy$xml_file)
    xml_files_set <- sample(xml_files_set_all, size = 5, replace = FALSE)
    df_params_copy <- df_params_copy[df_params_copy$xml_file %in% xml_files_set, ]
    
    change_config(df_params_copy, exe_dir, config_file)
    for (xml_i in xml_files_set){
      message(paste0('Processing ',xml_i,'...'))
      df_logit_i <- df_params_copy[df_params_copy$xml_file == xml_i, ]
      xml_file_path <- paste0(dir_xml, '/', unique(df_logit_i$xml_file))
      xml_file_cal  <- paste0(dir_xml,'/',unique(df_logit_i$destination_file))
      
      df_logit_i$xml_file <- NULL
      df_logit_i$destination_file <- NULL
      
      insertar_logits(
        xml_file_path,
        df_logit_i,
        xml_file_cal
      ) 
      
    }
    
    t2 <- Sys.time()
    
    run_gcam(run_gcam_file_cal)
    
    message('Saving results....')
  
    df_params_copy <- df_params_copy %>%
      select(region, supplysector, subsector, logit)
    
    ok <- tryCatch(
      {
        append_iteration_results()
        TRUE
      },
      error = function(e) {
        message(e$message)
        FALSE
      }
    )
    
    if (ok) {
      append_input(df_params_copy, file.path("Data", "inputs",'df_params.csv'))
    }
    
    delete_iteration_csvs()
    t3 <- Sys.time()
    
    simulation_log <-
      data.frame(
        factor = factor,
        relative_uncertainty = relative_uncertainty,
        time_changing_xml = as.numeric(t2 - t1, units = "mins"),
        time_gcam = as.numeric(t3 - t2, units = "mins"),
        total_time = as.numeric(t3 - t1, units = "mins"),
        xml_files = paste(xml_files_set, collapse = ";"),
        stringsAsFactors = FALSE
      )
    
    
    append_log(df = simulation_log, output_file = "simulation_log.csv") 
    
  }
















