library(future.apply)
library(dplyr)


source('1_2_Functions.R')
thisScript_path <- getwd()
set_gcam_paths('C:/GCAM/Nacho/gcam_europe')

if (!file.exists('df_logits.csv')) {
  message('No logits table. Extracting them...')
  all_errors_output_by_tech <- read.csv('../2. Extraction/Data/all_errors_output_by_tech.csv') 
  xml_files <- get_xml_files(config_path = config_file)
  xml_files_withLogit <- get_xmls_with_logit(xml_files, dir_xml) 
  xmls_with_logit_EUR <- xml_files_withLogit[grepl("EUR", xml_files_withLogit)]
  
  tablas_logits <- list()
  
  for (xml_file in xmls_with_logit_EUR) {
   message(paste0('extracting logits from ', xml_file))
    xml_file_path <- file.path(dir_xml, xml_file)
    
    tabla_logits <- extraer_logits_anyXML(xml_file_path)
    
    tabla_logits_filter <- tabla_logits %>%
      filter(
        subsector    %in% all_errors_output_by_tech$subsector &
        supplysector %in% all_errors_output_by_tech$output
      )
    
    if (nrow(tabla_logits_filter) > 0) {
      tablas_logits[[xml_file]] <- tabla_logits_filter
    }
  }
  
  df_logits <- bind_rows(tablas_logits) %>%
    mutate(destination_file = sub("\\.xml$", "_cal.xml", xml_file)) %>% filter(xml_file != 'building_det_EUR.xml')
  
  write.csv(df_logits, 'df_logits.csv', row.names = FALSE)
}else{
  message('df_logits already created. Loading logits....')
  df_logits <- read.csv('df_logits.csv') %>% select(-id) %>% filter(xml_file != 'building_det_EUR.xml')
}

change_config(df_logits, exe_dir, config_file)

n_iterations <- 200
#relative_uncertainty = 0.3
plan(multisession, workers = 16)



simulation_log <- data.frame(
  iteration = integer(),
  factor = numeric(),
  time_changing_xml = numeric(),
  time_gcam = numeric(),
  total_time = numeric(),
  relative_uncertainty = numeric()
)

for (i in 1:n_iterations){
  relative_uncertainty <- round(runif(1,0.1,0.6),1)
    message(paste0('#################################### ITERATION ', i,' ####################################'))
    message('Inducing uncertainty in the parameters...')
    t1 <- Sys.time()
    factor <- runif(
      1,
      1 - relative_uncertainty,
      1 + relative_uncertainty
    )
    
    df_logits_copy <- df_logits
    df_logits_copy$year  <- 2021
    df_logits_copy$logit <- round(df_logits_copy$logit * factor, 2)
    
    xml_files_set <- unique(df_logits_copy$xml_file)
    
    for (xml_i in xml_files_set){
      message(paste0('Processing ',xml_i,'...'))
      df_logit_i <- df_logits_copy[df_logits_copy$xml_file == xml_i, ]
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
  
    df_logits_copy <- df_logits_copy %>%
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
      append_input(df_logits_copy, file.path("Data", "inputs",'df_logits.csv'))
    }
    
    delete_iteration_csvs()
    t3 <- Sys.time()
    
    simulation_log <- rbind(
      simulation_log,
      data.frame(
        iteration = i,
        factor = factor,
        relative_uncertainty,
        time_changing_xml = as.numeric(t2 - t1, units = "mins"),
        time_gcam = as.numeric(t3 - t2, units = "mins"),
        total_time = as.numeric(t3 - t1, units = "mins")
      )
    )
    
    write.csv(
      simulation_log,
      "simulation_log.csv",
      row.names = FALSE
    )
    
  }
















