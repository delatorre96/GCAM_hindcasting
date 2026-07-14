library(fs)
library(readr)
library(dplyr)
library(purrr)
source('Functions.R')

thisScript_path <- getwd()

set_gcam_paths("C:/GCAM/Nacho/gcam_europe")
setwd(dir_gcamdata)
devtools::load_all()

files <- dir_ls(
  dir_csvs_iniciales,
  recurse = TRUE,
  regexp = "(?i)logit.*\\.csv$"
)


results <- vector("list", length(files))

density_results <- list()

for (i in seq_along(files)) {
  
  f <- files[i]
  
  message("Procesando: ", f)
  
  f_limpio <- sub(
    "\\.csv$",
    "",
    sub(
      "^C:/GCAM/Nacho/gcam_europe/input/gcamdata/inst/extdata/",
      "",
      f
    )
  )
  
  l <- get_csv_info(f_limpio)
  df <- l$df
  
  has_exp <- "logit.exponent" %in% names(df)
  
  exp_numeric <- if (has_exp) is.numeric(df$logit.exponent) else NA
  
  exp_values <- if (has_exp) {
    vals <- unique(df$logit.exponent)
    vals <- vals[!is.na(vals)]
    paste(sort(vals), collapse = ", ")
  } else {
    NA_character_
  }
  
  has_type <- "logit.type" %in% names(df)
  
  type_values <- if (has_type) {
    vals <- unique(as.character(df$logit.type))
    vals <- vals[!is.na(vals) & vals != ""]
    paste(sort(vals), collapse = ", ")
  } else {
    NA_character_
  }
  
  results[[i]] <- tibble(
    file = sub("\\.csv$", "", l$path),
    n_rows = nrow(df),
    exponent_numeric = exp_numeric,
    exponent_values = exp_values,
    minVal = min(df$logit.exponent, na.rm = TRUE),
    maxVal = max(df$logit.exponent, na.rm = TRUE),
    type_values = type_values
  )
  
  ## Densidad
  if (has_exp && is.numeric(df$logit.exponent)) {
    
    x <- na.omit(df$logit.exponent)
    
    if (length(unique(x)) > 1) {
      
      d <- density(x)
      
      density_results[[length(density_results) + 1]] <-
        tibble(
          file = sub("\\.csv$", "", l$path),
          logit.exponent = d$x,
          density = d$y
        )
    }
  }
}

results <- bind_rows(results)
density_results <- bind_rows(density_results)

write.csv(results, "params_logit.csv", row.names = FALSE)
write.csv(density_results, "logit_density.csv", row.names = FALSE)



setwd(thisScript_path)

      