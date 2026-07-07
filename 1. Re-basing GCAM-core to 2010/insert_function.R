insert_function <- function(){
  thisLoc <- getwd()
  ##Get all functions from dir: C:\Users\ignacio.delatorre\Documents\GCAM\gcam-core\input\gcamdata\R
  dir = "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata/R"
  setwd(dir)
  files <- list.files()
  files <- files[
      grepl(paste(c('zaglu','zenergy','zemissions','zwater','zsocio','zgcamusa'), collapse = "|"), files) &
      grepl("\\.R$", files) & grepl("L2",files)
  ]

  for (file in files) {
    lines <- readLines(file)

    pos_get <- grep("\\bcuttingYears\\b", lines)

    if (length(pos_get) == 0) {
      warning("No se encontró cuttingYears en ", file)
      next
    }

    last_get <- max(pos_get)

    # Insertar hindcasting() después de esa línea
    lines <- append(lines, "hindcasting()", after = last_get)

    # Sobrescribir el archivo
    writeLines(lines, file)

  }
  setwd(thisLoc)
}

insert_function_in_xml <- function(){
  thisLoc <- getwd()
  ##Get all functions from dir: C:\Users\ignacio.delatorre\Documents\GCAM\gcam-core\input\gcamdata\R
  dir = "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata/R"
  setwd(dir)
  files <- list.files()
  files <- files[
    grepl('xml', files) &
      grepl("\\.R$", files)
  ]
  files <- files[files != "xml.R"]

  for (file in files) {
    lines <- readLines(file)

    pos_get <- grep(
      "all_data <- list(...)[[1]]",
      lines,
      fixed = TRUE
    )

    if (length(pos_get) == 0) {
      warning("No se encontró all_data en ", file)
      next
    }

    last_get <- max(pos_get)

    # Insertar hindcasting() después de esa línea
    lines <- append(lines, "hindcasting()", after = last_get)

    # Sobrescribir el archivo
    writeLines(lines, file)

  }
  setwd(thisLoc)
}

