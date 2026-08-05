library(future.apply)
library(dplyr)


source('1_2_Functions.R')
thisScript_path <- getwd()
set_gcam_paths('C:/GCAM/Nacho/gcam_europe')

if (!file.exists('df_params.csv')) {
  regions <- c(
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
  message('No logits table. Extracting them...')
  all_errors_output_by_tech <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv') 
  xml_files <- get_xml_files(config_path = config_file)
  
  xml_files_EUR <-  xml_files[grepl("EUR", xml_files)]
  xml_to_include <- c('ag_an_demand_input.xml')
  xml_files_EUR <- c(xml_files_EUR, xml_to_include)
  
  ## LOGITS ##
  xmls_with_logit_EUR <- get_xmls_with_logit(xml_files_EUR, dir_xml) 
  xml_not_to_include <- c(
    "water_td_EUR.xml",
    "EFW_irrigation_EUR.xml",
    "EFW_manufacturing_EUR.xml",
    "EFW_municipal_EUR.xml",
    "ind_urb_processing_sectors_EUR.xml"
  ) 
  xmls_with_logit_EUR <- xmls_with_logit_EUR[!xmls_with_logit_EUR %in% xml_not_to_include]

  
  ## others ##
  xmls_with_price_elasticity <- get_xmls_with_param(xmls_with_logit_EUR, dir_xml, 'price-elasticity')

  
  # interested_subsectors <- all_errors_output_by_tech$subsector
  # interested_sectors <- all_errors_output_by_tech$output
  df_params <- createDF_params(xml_files = xmls_with_logit_EUR, regions)
  # df_otherParams <- createDF_otherParams(xml_files = xmls_with_price_elasticity, regions = regions, param = 'price-elasticity')
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

df_params <- df_params %>%
  filter(xml_file %in% most_important_xml)

simulation_log <- data.frame(
  iteration = integer(),
  factor = numeric(),
  time_changing_xml = numeric(),
  time_gcam = numeric(),
  total_time = numeric(),
  relative_uncertainty = numeric()
)

for (i in 1:n_iterations){
  t1 <- Sys.time()
  run_id <- paste0('r',format(Sys.time(), "%d%m%Y%H%M%S"))
  
  message('Inducing uncertainty in the parameters...')
  df_params_copy <- df_params
  df_params_copy$year  <- 2021
  
  repeat {
    
    delta <- round(runif(1,-2,2),2)

    new_logit <- df_params$logit * (1 + delta)
    
    if (all(new_logit <= 0)) break
    
  }
  

  df_params_copy$logit <- round(new_logit, 2)
  xml_files_set <- unique(df_params_copy$xml_file)
  
  # xml_files_set_all <- unique(df_params_copy$xml_file)
  # xml_files_set <- sample(xml_files_set_all, size = 5, replace = FALSE)
  # df_params_copy <- df_params_copy[df_params_copy$xml_file %in% xml_files_set, ]
  
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
  
  #t2 <- Sys.time()
  
  message('Running GCAM..')
  
  run_gcam(run_gcam_file_cal)
  
  executionErrors <- any(grepl("error", readLines(log_gcam), ignore.case = TRUE))
  message('Saving results....')

  df_params_copy <- df_params_copy %>%
    select(xml_file,region, supplysector, subsector, nesting_subsector, year, logit)
  
  ok <- tryCatch(
    {
      append_iteration_results(run_id = run_id)
      TRUE
    },
    error = function(e) {
      message(e$message)
      FALSE
    }
  )
  
  if (ok) {
    append_input(df = df_params_copy, output_file = file.path("Data", "inputs",'df_params.csv'), run_id = run_id)
  }
  
  delete_iteration_csvs()
  t3 <- Sys.time()
  
  simulation_log <-
    data.frame(
      run_id = run_id,
      timestamp = Sys.Date(),
      executionTime = as.numeric(t3 - t1, units = "mins"),
      executionErrors = executionErrors,
      delta = delta,
      xml_files = paste(xml_files_set, collapse = ";"),
      stringsAsFactors = FALSE
    )
  
  
  append_log(df = simulation_log, output_file = "simulation_log.csv", run_id = run_id) 
    
  }
















