library(tidyr)
library(dplyr)

load('C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/1. Re-basing GCAM-core to 2010/Pre-XML data harmonization/csvs_to_xml_2010.RData')
csvs_to_xml_2010 <- csvs_to_xml
load('C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/1. Re-basing GCAM-core to 2010/Pre-XML data harmonization/csvs_to_xml_2021.RData')
csvs_to_xml_2021 <- csvs_to_xml
rm(csvs_to_xml)
load('C:/Users/ignacio.delatorre/Documents/GCAM/Hindcasting/1. Re-basing GCAM-core to 2010/Pre-XML data harmonization/new_chunks_2010.RData')


############## All columns ##############
all_columns <- c()
for (chunk_i in names(csvs_to_xml_2010)){
     chunk <- csvs_to_xml_2010[[chunk_i]]
     for (df_i in names(chunk)){
         df_ <- chunk[[df_i]]
         all_columns <- c(all_columns, names(df_))
         # calibration_cols <- c()
          # for (col in names(df_)){
           #   if 
           # }
         }
   }
all_columns <- unique(all_columns)


############## Identification of calinration cols and modification ##############



##### Option 1 ##### 

# 
# new_chunks_2010 <- list()
# 
# for (chunk_i in names(csvs_to_xml_2010)) {
#   chunk <- csvs_to_xml_2010[[chunk_i]]
#   
#   new_chunks_2010[[chunk_i]] <- list()
#   
#   for (df_i in names(chunk)) {
#     df_2010 <- chunk[[df_i]]
#     cols <- names(df_2010)
#     
#     cols_cal <- cols[grepl("^cal", cols) | grepl("base//.", cols)]
#     
#     df_2021 <- csvs_to_xml_2021[[chunk_i]][[df_i]]
#     
#     if (length(cols_cal) > 0) {
#       
#       key_cols <- setdiff(cols, cols_cal)
#       
#       df_2010_new <- df_2010 %>%
#         select(-all_of(cols_cal)) %>%
#         left_join(df_2021, by = key_cols)
#       
#       new_chunks_2010[[chunk_i]][[df_i]] <- df_2010_new
#       
#     } else {
#       
#       new_chunks_2010[[chunk_i]][[df_i]] <- df_2010
#       
#     }
#   }
# }
# 
# save(new_chunks_2010, file = "new_chunks_2010.RData")

###### Option 2 ###### 

library(purrr)

exclude_cols <- c(
  "region", "sector", "technology", "market.name",
  "scenario", "GCAM_region_ID", "fuel",
  "units", "price.unit", "input.unit", "output.unit"
)
is_calibration_col <- function(col) {
  grepl("^cal", col) |
    grepl("^base\\.", col) |
    grepl("share\\.weight", col) |
    grepl("logit", col) |
    grepl("elasticity", col) |
    grepl("efficiency", col) |
    grepl("productivity", col) |
    grepl("aeei", col) |
    grepl("tech\\.change", col) |
    grepl("cost", col) |
    grepl("price", col) |
    grepl("input\\.cost", col) |
    grepl("capital", col) |
    grepl("depreciation", col) |
    grepl("fixed\\.charge", col) |
    grepl("available", col) |
    grepl("reserve", col) |
    grepl("extractioncost", col) |
    grepl("demand", col) |
    grepl("service", col)
}
select_calibration_cols <- function(cols) {
  cols[is_calibration_col(cols) & !cols %in% exclude_cols]
}
new_chunks_2010_1 <- list()

for (chunk_i in names(csvs_to_xml_2010)) {
  
  chunk_2010 <- csvs_to_xml_2010[[chunk_i]]
  chunk_2021 <- new_chunks_2010[[chunk_i]]
  
  new_chunks_2010_1[[chunk_i]] <- list()
  
  for (df_i in names(chunk_2010)) {
    
    df_2010 <- chunk_2010[[df_i]]
    df_2021 <- new_chunks_2010[[df_i]]
    
    cols <- names(df_2010)
    
    # 1. detectar columnas calibración
    cols_cal <- select_calibration_cols(cols)
    
    # 2. si no hay calibración, copiar directo
    if (length(cols_cal) == 0) {
      new_chunks_2010_1[[chunk_i]][[df_i]] <- df_2010
      next
    }
    
    # 3. definir keys (estructura GCAM)
    key_cols <- setdiff(cols, cols_cal)
    
    # seguridad adicional
    key_cols <- setdiff(key_cols, exclude_cols)
    
    # 4. evitar duplicación por join mal definido
    df_2021_sub <- df_2021 %>%
      select(all_of(key_cols), all_of(cols_cal)) %>%
      distinct()
    
    # 5. transplant calibración 2021 → 2010
    df_2010_new <- df_2010 %>%
      select(-all_of(cols_cal)) %>%
      left_join(df_2021_sub, by = key_cols)
    
    new_chunks_2010_1[[chunk_i]][[df_i]] <- df_2010_new
  }
}
save(new_chunks_2010_1, file = "new_chunks_2010.RData")
