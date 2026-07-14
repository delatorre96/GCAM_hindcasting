thisScript_path <- "C:/GCAM/Nacho/Hindcasting/4_SensitivityAnalysis"
source('Functions.R')
##get all paths from gcam folder
gcam_path <- "C:/GCAM/Nacho/gcam_europe"
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()
set.seed(1234)

n_iterations <- 100
integer_exponent = TRUE
relative_uncertainty = 0.6

#logit.type <- c('absolute-cost-logit','relative-cost-logit', NA)

logit_EUR_files <- c(#'gcam-europe/A44.subsector_logit_EUR',
                     'gcam-europe/A23.elecS_subsector_logit'
                     #'gcam-europe/A42.subsector_logit_EUR'
                     )

for (i in 1:n_iterations){
  change_csvs(logit_EUR_files,
              relative_uncertainty,
              integer_exponent)
  
  driver_drake()

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
    append_iteration_inputs(logit_EUR_files)
  }
  
  delete_iteration_csvs()
  
  setwd(dir_gcamdata)
}
