
thisLoc <- getwd()

path2save <-"C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis"
gcam_path1 <- 'C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata'
gcam_path2 <- 'C:/Users/ignacio.delatorre/Documents/GCAM/Release/input/gcamdata'

if (thisLoc != path2save){
  thisLoc = path2save
  setwd(thisLoc)
}
source('../all_inputs.R')

setwd(gcam_path1)
setwd('R')

files <- 'zaglu_xml_ag_an_demand_input.R'

names_gcams = c("baseYear2010", "baseYear2021")
gcam_paths = c(gcam_path1,gcam_path2)

comparing_inputs(gcam_paths = gcam_paths, chunks_inputs = files, path2save = path2save, 
                 drake1 = FALSE, drake2 = FALSE, names_gcams = names_gcams)

##### Analyze inputs #####
setwd(thisLoc)
load('inputs.Rdata')

comparison_results <- get_diffs(all_inputs, names_gcams)

comparison_results_false<- comparison_results[comparison_results$identical == FALSE , ] 

row_differences <- compute_row_diffs(all_inputs, names_gcams, comparison_results_false)
row_differences <- row_differences[row_differences$col_name != 'from.year',]

#################3

row_differences <- compute_row_values_all(all_inputs, names_gcams, comparison_results_false)

row_differences <- row_differences[row_differences$value_2010 != row_differences$value_2021, ]

df_2010 <- all_inputs$baseYear2010$zaglu_xml_ag_an_demand_input.R$L203.StubCalorieContent
df_2021 <- all_inputs$baseYear2021$zaglu_xml_ag_an_demand_input.R$L203.StubCalorieContent
