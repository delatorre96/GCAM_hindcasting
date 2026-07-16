thisScript_path <- "C:/GCAM/Nacho/Hindcasting/4_SensitivityAnalysis"
source('1_2_Functions.R')
set_gcam_paths('C:/GCAM/Nacho/gcam_europe')
set.seed(1234)

n_iterations <- 100
relative_uncertainty = 0.3
xml_file <- paste0(dir_xml,"/en_supply_EUR.xml")
xml_file_cal <- paste0(dir_xml,"/en_supply_EUR_cal.xml")

for (i in 1:n_iterations){
  
  tabla_logits <- extraer_logits(xml_file)
  tabla_logits$year <- 2021
  
  #tabla_logits$logit <- round(tabla_logits$logit * runif(nrow(tabla_logits),1 - relative_uncertainty,1 + relative_uncertainty),2)
  
  factor <- runif(1,
                  1 - relative_uncertainty,
                  1 + relative_uncertainty)
   
  tabla_logits$logit <-  round(tabla_logits$logit * factor,2)
  
  insertar_logits(xml_file,tabla_logits, xml_file_cal)

  run_gcam(run_gcam_file)

  setwd(thisScript_path)

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
    append_input(tabla_logits, file.path(thisScript_path,"Data", "inputs",'en_supply_EUR.csv'))
  }
  
  delete_iteration_csvs()
 
}
