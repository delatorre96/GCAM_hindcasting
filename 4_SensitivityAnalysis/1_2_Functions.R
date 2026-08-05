library(xml2)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

set_gcam_paths <- function(gcam_path) {
  #Exmple:
  #dir_gcamdata <- "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata"
  dir_gcam <<- gcam_path
  config_file <<-  paste0(gcam_path,'/exe/configuration.xml')
  exe_dir <<- paste0(gcam_path,'/exe')
  run_gcam_file <<- paste0(gcam_path,'/exe/run-gcam.bat')
  run_gcam_file_cal <<- paste0(gcam_path,'/exe/run-gcam_cal.bat')
  dir_gcamdata <<- paste0(gcam_path,'/input/gcamdata')
  dir_xml <<- paste0(gcam_path,'/input/gcamdata/xml')
  log_gcam <<- paste0(gcam_path,'/exe/logs/main_log.txt')
  
  print(dir_gcam)
  print(config_file)
  print(run_gcam_file)
  print(run_gcam_file_cal)
  print(dir_gcamdata)
  print(dir_xml)
  print(log_gcam)
}

get_xml_files <- function(config_path) {
  
  # Leer el XML
  doc <- read_xml(config_path)
  
  # Extraer todos los Value de ScenarioComponents
  paths <- xml_text(xml_find_all(doc, "//ScenarioComponents/Value"))
  
  # Quedarse sólo con los archivos de gcamdata/xml
  paths <- paths[grepl("gcamdata/xml", paths)]
  
  # Extraer únicamente el nombre del archivo
  xml_files <- basename(paths)
  
  return(xml_files)
}

get_xmls_with_logit <- function(xml_files, xml_dir) {
  Filter(function(f) {
    doc <- read_xml(file.path(xml_dir, f))
    length(xml_find_all(doc, ".//logit-exponent")) > 0
  }, xml_files)
}



get_xmls_with_param <- function(xml_files, xml_dir, param) {
  Filter(function(f) {
    doc <- read_xml(file.path(xml_dir, f))
    length(xml_find_all(doc, paste0(".//", param))) > 0
  }, xml_files)
}



extraer_params <- function(xml_file, param){
  
  library(xml2)
  
  doc <- read_xml(xml_file)
  
    logits <- xml_find_all(doc, paste0(".//", param))
    
    salida <- vector("list", length(logits))
    
    for(i in seq_along(logits)){
      
      logit <- logits[[i]]
      
      padres <- xml_parents(logit)
      
      region <- NA
      supplysector <- NA
      energy_final_demand <- NA
      subsector <- NA
      level <- NA
      
      for(p in padres){
        
        etiqueta <- xml_name(p)
        
        if(etiqueta == "region"){
          region <- xml_attr(p, "name")
        }
        
        if (etiqueta == "energy-final-demand"){
          energy_final_demand <- xml_attr(p, "name")
          level <- "energy-final-demand"
        }
        if(etiqueta == "supplysector"){
          supplysector <- xml_attr(p, "name")
          level <- "supplysector"
        }
        
        if(etiqueta == "subsector"){
          subsector <- xml_attr(p, "name")
          level <- "subsector"
        }
        
      }
      
      salida[[i]] <- data.frame(
        
        id = i,
        
        region = region,
        
        energy_final_demand = energy_final_demand,
        
        supplysector = supplysector,
        
        subsector = subsector,
        
        level = level,
        
        fillout = xml_attr(logit,"fillout"),
        
        year = as.numeric(xml_attr(logit,"year")),
        
        logit = as.numeric(xml_text(logit)),
        
        xpath = xml_path(logit),
        
        stringsAsFactors = FALSE
        
      )
      
    }
    
    do.call(rbind, salida)
  
}






extraer_logits_anyXML<- function(xml_file){
  
  library(xml2)
  
  doc <- read_xml(xml_file)
  
  logits <- xml_find_all(doc, ".//logit-exponent")
  
  salida <- vector("list", length(logits))
  
  for(i in seq_along(logits)){
    
    logit <- logits[[i]]
    
    padres <- xml_parents(logit)
    
    region <- NA
    supplysector <- NA
    subsector <- NA
    nesting_subsector  <- NA
    level <- NA
    
    for(p in padres){
      
      etiqueta <- xml_name(p)
      
      if(etiqueta == "region"){
        region <- xml_attr(p,"name")
      }
      
      if(etiqueta == "supplysector"){
        supplysector <- xml_attr(p,"name")
        level <- "supplysector"
      }
      
      if(etiqueta == "subsector"){
        subsector <- xml_attr(p,"name")
        level <- "subsector"
      }
      if(etiqueta == "nesting-subsector"){
        nesting_subsector <- xml_attr(p,"name")
        level <- "nesting-subsector"
      }
      
    }
    
    salida[[i]] <- data.frame(
      
      xml_file = basename(xml_file),
      
      id = i,
      
      region = region,
      
      supplysector = supplysector,
      
      subsector = subsector,
      
      level = level,
      
      nesting_subsector = nesting_subsector,
      
      fillout = xml_attr(logit,"fillout"),
      
      year = as.numeric(xml_attr(logit,"year")),
      
      logit = as.numeric(xml_text(logit)),
      
      xpath = xml_path(logit),
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  do.call(rbind,salida)
  
}



insertar_logits <- function(xml_entrada,
                            tabla_logits,
                            xml_salida){
  
  doc <- read_xml(xml_entrada)
  
  for(i in seq_len(nrow(tabla_logits))){
    
    nodo <- xml_find_first(doc, tabla_logits$xpath[i])
    
    if(!inherits(nodo, "xml_missing")){
      
      # Crear el nuevo nodo
      nuevo <- read_xml(sprintf(
        '<logit-exponent fillout="%s" year="%s">%s</logit-exponent>',
        tabla_logits$fillout[i],
        tabla_logits$year[i],
        tabla_logits$logit[i]
      ))
      
      # Insertarlo después del logit existente
      xml_add_sibling(nodo, nuevo, .where = "after")
    }
  }
  
  write_xml(doc, xml_salida, options = "format")
}



change_config <- function(df_logits, exe_dir, config_file){
  config <- read_xml(config_file)
  
  archivos_modificar <- unique(df_logits$xml_file)
  
  # Todos los nodos <Value> de ScenarioComponents
  nodos <- xml_find_all(config, ".//ScenarioComponents/Value")
  
  for (nodo in nodos) {
    
    ruta <- xml_text(nodo)
    archivo <- basename(ruta)
    
    if (archivo %in% archivos_modificar) {
      
      ruta_nueva <- sub("\\.xml$", "_cal.xml", ruta)
      
      xml_text(nodo) <- ruta_nueva
    }
  }
  
  # Guardar con el nombre que quieras
  write_xml(config, paste0(exe_dir,"/configuration_cal.xml"))
}


createDF_params <- function(xml_files, regions, interested_subsectors = NA, interested_sectors = NA){
  tablas_logits <- list()
  
  for (xml_file in xml_files) {
    message(paste0('extracting logits from ', xml_file))
    xml_file_path <- file.path(dir_xml, xml_file)
    
    tabla_logits <- extraer_logits_anyXML(xml_file_path) %>% 
      filter(region %in% regions)
    
    if (!(length(interested_subsectors) == 1 && is.na(interested_subsectors)) &&
        !(length(interested_sectors) == 1 && is.na(interested_sectors))) {
      tabla_logits <- tabla_logits %>%
        filter(
          subsector    %in% interested_subsectors |
            supplysector %in% interested_sectors
        )
    }
    if (nrow(tabla_logits) > 0) {
      tablas_logits[[xml_file]] <- tabla_logits
    }
  }
  
  df_params <- bind_rows(tablas_logits) %>%
    mutate(destination_file = sub("\\.xml$", "_cal.xml", xml_file)) #%>% filter(xml_file != 'building_det_EUR.xml')
  
  write.csv(df_params, 'df_params.csv', row.names = FALSE)
  return(df_params)
}


createDF_otherParams <- function(xml_files, regions, interested_subsectors = NA, interested_sectors = NA, param){
  tablas_logits <- list()
  
  for (xml_file in xml_files) {
    message(paste0('extracting logits from ', xml_file))
    xml_file_path <- file.path(dir_xml, xml_file)
    
    tabla_logits <- extraer_params(xml_file_path, param) %>% 
      filter(region %in% regions)
    
    if (nrow(tabla_logits) > 0) {
      tablas_logits[[xml_file]] <- tabla_logits
    }
  }
  
  df_params <- bind_rows(tablas_logits) %>%
    mutate(destination_file = sub("\\.xml$", "_cal.xml", xml_file)) #%>% filter(xml_file != 'building_det_EUR.xml')
  
  write.csv(df_params, 'df_otherParams.csv', row.names = FALSE)
  return(df_params)
}



run_gcam <- function(bat_path) {
  bat_dir <- dirname(bat_path)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(bat_dir)
  status <- system2("cmd.exe", args = c("/c", basename(bat_path)), stdout = "", stderr = "")
  cat(sprintf("\nGCAM terminó con código de salida %d\n", status))
  return(status)
}




append_input <- function(df, output_file, run_id) {
  dir.create(
    file.path(thisScript_path, "Data", "inputs"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Añadir columna
  df$run_id <- run_id
  
  # Escribir
  write.table(
    df,
    file = output_file,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(output_file),
    append = file.exists(output_file),
    quote = FALSE,
    na = ""
  )
}



append_log <- function(df, output_file, run_id) {
  
  df$run_id <- run_id
  
  
  # Escribir
  write.table(
    df,
    file = output_file,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(output_file),
    append = file.exists(output_file),
    quote = FALSE,
    na = ""
  )
}



append_iteration_results <- function(data_dir = file.path(getwd(), "Data"), run_id) {
  
  source_files <- list.files(
    data_dir,
    pattern = "0\\.csv$",
    full.names = TRUE
  )
  
  for (source_file in source_files) {
    
    ## Comprobar que el archivo es válido
    lines <- tryCatch(
      readLines(source_file, warn = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(lines) ||
        length(lines) < 2 ||
        grepl("had error", lines[1], ignore.case = TRUE)) {
      
      warning(
        sprintf(
          "Se omite '%s' porque el resultado es inválido.",
          basename(source_file)
        ),
        call. = FALSE
      )
      
      next
    }
    
    ## Leer el CSV
    df <- tryCatch(
      read.csv(
        source_file,
        check.names = FALSE,
        skip = 1
      ),
      error = function(e) NULL
    )
    
    if (is.null(df)) {
      
      warning(
        sprintf(
          "No se pudo leer '%s'. Se omite.",
          basename(source_file)
        ),
        call. = FALSE
      )
      
      next
    }
    
    ## Eliminar columnas completamente vacías
    df_vacio <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
    
    ## Si no hay datos, pasar al siguiente archivo
    if (nrow(df_vacio) == 0) {
      
      warning(
        sprintf(
          "La consulta '%s' no devolvió filas. Se omite.",
          basename(source_file)
        ),
        call. = FALSE
      )
      
      next
    }
    
    target_file <- sub("0\\.csv$", ".csv", source_file)
    df$run_id <- run_id
    
    df <- df[c('region', 'sector', 'subsector', 'output', 'technology', '2021','run_id')]
    
   
    
    write.table(
      df,
      file = target_file,
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(target_file),
      append = file.exists(target_file),
      quote = TRUE
    )
  }
  
  invisible(TRUE)
}


delete_iteration_csvs <- function(data_dir = file.path(getwd(), "Data")) {
  
  files <- list.files(
    data_dir,
    pattern = "0\\.csv$",
    full.names = TRUE
  )
  
  if (length(files) > 0) {
    file.remove(files)
  }
  
  invisible(NULL)
}




