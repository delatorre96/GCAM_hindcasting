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
    mutate(destination_file = sub("\\.xml$", "_cal.xml", xml_file))
  
  write.csv(df_logits, 'df_logits.csv', row.names = FALSE)
}else{
  message('df_logits already created. Loading logits....')
  df_logits <- read.csv('df_logits.csv')
}

change_config(df_logits, exe_dir, config_file)

n_iterations <- 100
relative_uncertainty = 0.3
plan(multisession, workers = 16)


timings <- data.frame(
  iteration = integer(),
  time_changing_xml = numeric(),
  time_gcam = numeric(),
  total_time = numeric()
)

for (i in 1:n_iterations){
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
  
  df_split <- split(df_logits_copy, df_logits_copy$xml_file)
  
  resultado <- future_lapply(names(df_split), function(xml_file){
    
    tabla_logits <- df_split[[xml_file]]
    
    xml_file_path <- file.path(dir_xml, xml_file)
    xml_file_cal  <- file.path(dir_xml, tabla_logits$destination_file[1])
    
    tabla_logits$xml_file <- NULL
    tabla_logits$destination_file <- NULL
    
    insertar_logits(
      xml_file_path,
      tabla_logits,
      xml_file_cal
    )
    
    tabla_logits
  })
  
  df_logits_with_uncer <-
    bind_rows(resultado) |>
    select(region, supplysector, subsector, logit)
  t2 <- Sys.time()
  
  run_gcam(run_gcam_file_cal)
  
  message('Saving results....')

  
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
    append_input(df_logits_with_uncer, file.path("Data", "inputs",'df_logits.csv'))
  }
  
  delete_iteration_csvs()
  t3 <- Sys.time()
  
  timings <- rbind(
    timings,
    data.frame(
      iteration = i,
      time_changing_xml = as.numeric(t2 - t1, units = "mins"),
      time_gcam = as.numeric(t3 - t2, units = "mins"),
      total_time = as.numeric(t3 - t1, units = "mins")
    )
  )
  write.csv(timings, "timings.csv", row.names = FALSE)
}















# 
# ############ Extraer las combinaciones únicas de cada data frame
# combis <- lapply(tablas_logits, \(df) {
#   unique(df[, c("supplysector", "subsector")])
# })
# 
# # Comparar todos los pares
# res <- combn(names(combis), 2, simplify = FALSE, FUN = function(par) {
#   comunes <- merge(
#     combis[[par[1]]],
#     combis[[par[2]]],
#     by = c("supplysector", "subsector")
#   )
#   
#   if (nrow(comunes) > 0) {
#     data.frame(
#       df1 = par[1],
#       df2 = par[2],
#       n_coincidencias = nrow(comunes),
#       coincidencias = I(list(comunes))
#     )
#   }
# })
# 
# # Mantener solo los pares con coincidencias
# res <- do.call(rbind, Filter(Negate(is.null), res))
# 
# res























xml_file_cal <- paste0(dir_xml,"/en_transformation_EUR_cal.xml")

tabla_logits <- extraer_logits_anyXML(xml_file)
tabla_logits$year <- 2021
tabla_logits$logit <- tabla_logits$logit + rnorm(nrow(tabla_logits),0,0.2)
insertar_logits(xml_file,tabla_logits, xml_file_cal)





