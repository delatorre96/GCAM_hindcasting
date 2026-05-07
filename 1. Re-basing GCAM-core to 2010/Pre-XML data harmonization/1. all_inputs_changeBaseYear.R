#### GetInputs #####


thisLoc <- getwd()

path2save <-"C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis"
gcam_path <- 'C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata'

if (thisLoc != path2save){
  thisLoc = path2save
  setwd(thisLoc)
}
source('../all_inputs.R')

setwd(gcam_path)
setwd('R')

files <- list.files()

files_xml <- files[
  grepl('xml', files) & 
    grepl(paste(c('zaglu','zenergy','zemissions','zwater','zsocio','zgcamusa'), collapse = "|"), files) &
    grepl("\\.R$", files)   
]
setwd(thisLoc)
csvs_to_xml <- get_allData(gcam_path = gcam_path, do_driver = TRUE, save_input_data = TRUE, path2save = path2save, files_xml = files_xml,csvs_to_xml_name = 'csvs_to_xml_2010')

##### Analyze inputs #####
setwd(thisLoc)
if (!('csvs_to_xml' %in% ls())){
  load('csvs_to_xml.RData')
}

results_exact_years <- analyze_exact_year_columns(csvs_to_xml)
unique_exact_years <- results_exact_years  %>% select(min_year, max_year, all_years) %>% distinct()

results_contains_years <- analyze_year_columns_contains(csvs_to_xml)
unique_contains_years <- results_contains_years  %>% select(min_year, max_year, all_years) %>% distinct()


write.csv(results, 'final_df_years_2021.csv', row.names = FALSE)
write.csv(unique_col_years, 'unique_col_years_2021.csv', row.names = FALSE)
write.csv(unique_contains_years, 'unique_contains_years_2021.csv', row.names = FALSE)

