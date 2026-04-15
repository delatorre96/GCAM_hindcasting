#### GetInputs #####


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

files <- list.files()
# files <-  files[grepl('xml', files) &
#                   grepl('zaglu', files) &
#                   (grepl('_ag_', files) | grepl('_an_', files))]

files <- files[
  grepl('xml', files) & 
    grepl(paste(c('zaglu','zenergy','zemissions','zwater'), collapse = "|"), files)
]

names_gcams = c("baseYear2010", "baseYear2021")
gcam_paths = c(gcam_path1,gcam_path2)

comparing_inputs(gcam_paths = gcam_paths, chunks_inputs = files, path2save = path2save, 
                 drake1 = FALSE, drake2 = FALSE, names_gcams = names_gcams)

##### Analyze inputs #####
setwd(thisLoc)
load('inputs.Rdata')

comparison_results <- get_diffs(all_inputs, names_gcams)

comparison_results_false<- comparison_results[comparison_results$identical == FALSE &
  !sapply(comparison_results$diff_columns, function(cols) any(grepl("year", cols, ignore.case = TRUE))),
]
comparison_results_false<- comparison_results[comparison_results$identical == FALSE &
                                                comparison_results$n_diff_columns == 1, ] 

row_differences <- compute_row_diffs(all_inputs, names_gcams, comparison_results_false)
row_differences_zeroIn2010 <- row_differences[row_differences$value_2010 == 0 & row_differences$value_2021 != 0 ,]
row_differences_zeroIn2021 <- row_differences[row_differences$value_2010 != 0 & row_differences$value_2021 == 0 ,]

chunks_df2010 <- distinct( row_differences_zeroIn2010[c("chunk","dataframe",  "col_name")])
chunks_df2021 <- distinct( row_differences_zeroIn2021[c("chunk","dataframe",  "col_name")])
antijoin <- chunks_df2010 %>% anti_join(chunks_df2021, by = c('chunk','dataframe'))

write.csv(chunks_df,paste0(path2save,'/chunk_zero2010_1.csv'), row.names = FALSE)


