thisScript_path <- "C:/GCAM/Nacho/Hindcasting/4_SensitivityAnalysis/Parameters_identification"
source('Functions.R')
##get all paths from gcam folder
gcam_path <- "C:/GCAM/Nacho/gcam_europe"
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()

files <- search_string(content_patterns  = 'logit', filename_patterns = 'zgcameurope')
