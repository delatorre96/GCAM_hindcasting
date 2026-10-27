
#----- Función de comparación de distribuciones por dimensión -----

analizar_distribuciones_ks <- function(
    data,
    dimension,
    variable_error = "error",
    alpha = 0.01,
    metodo_ajuste = "BH"
) {

  #----- Validaciones -----

  if (!dimension %in% names(data)) {
    stop("La dimensión no existe en el dataframe: ", dimension)
  }

  if (!variable_error %in% names(data)) {
    stop("La variable de error no existe en el dataframe: ", variable_error)
  }

  if (!is.numeric(data[[variable_error]])) {
    stop("La variable de error debe ser numérica.")
  }

  #----- Preparar datos -----

  datos <- data %>%
    filter(
      !is.na(.data[[dimension]]),
      is.finite(.data[[variable_error]])
    ) %>%
    transmute(
      categoria = as.character(.data[[dimension]]),
      error = .data[[variable_error]]
    )

  #----- Separar errores por categoría -----

  errores <- split(datos$error, datos$categoria)
  errores <- errores[lengths(errores) > 0]

  categorias <- names(errores)

  if (length(categorias) < 2) {
    stop("Se necesitan al menos dos categorías con datos válidos.")
  }

  #----- Generar combinaciones únicas -----

  combinaciones <- combn(categorias, 2, simplify = FALSE)

  #----- Test Kolmogorov-Smirnov por pareja -----

  resultados <- lapply(combinaciones, function(par) {

    i <- par[1]
    j <- par[2]

    test <- tryCatch(
      suppressWarnings(
        ks.test(errores[[i]], errores[[j]])
      ),
      error = function(e) NULL
    )

    if (is.null(test)) {
      return(data.frame(
        categoria_i = i,
        categoria_j = j,
        n_i = length(errores[[i]]),
        n_j = length(errores[[j]]),
        D = NA_real_,
        p_value = NA_real_
      ))
    }

    data.frame(
      categoria_i = i,
      categoria_j = j,
      n_i = length(errores[[i]]),
      n_j = length(errores[[j]]),
      D = unname(test$statistic),
      p_value = test$p.value
    )
  })

  #----- Unir resultados y ajustar p-values -----

  comparaciones <- bind_rows(resultados) %>%
    mutate(
      p_value_ajustado = p.adjust(
        p_value,
        method = metodo_ajuste
      ),
      distribuciones_difieren = if_else(
        !is.na(p_value_ajustado) &
          p_value_ajustado < alpha,
        1L, 0L
      )
    )

  #----- Resumir resultados por categoría -----

  resumen <- bind_rows(
    comparaciones %>%
      transmute(
        categoria = categoria_i,
        distribuciones_difieren
      ),
    comparaciones %>%
      transmute(
        categoria = categoria_j,
        distribuciones_difieren
      )
  ) %>%
    group_by(categoria) %>%
    summarise(
      n_comparaciones = n(),
      n_distribuciones_diferentes = sum(distribuciones_difieren),
      prct_distr_diff = mean(distribuciones_difieren) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(prct_distr_diff))

  #----- Devolver ambos data frames -----

  list(
    comparaciones = comparaciones,
    resumen = resumen
  )
}


#----- Análisis por región -----

ks_region <- analizar_distribuciones_ks(
  all_errors,
  dimension = "region"
)

df_ks_regions <- ks_region$comparaciones
sum_regions <- ks_region$resumen


#----- Análisis por sector -----

ks_sector <- analizar_distribuciones_ks(
  all_errors,
  dimension = "sector"
)

df_ks_sector <- ks_sector$comparaciones
sum_sector <- ks_sector$resumen


#----- Análisis por subsector -----

ks_subsector <- analizar_distribuciones_ks(
  all_errors,
  dimension = "subsector"
)

df_ks_subsector <- ks_subsector$comparaciones
sum_subsector <- ks_subsector$resumen


#----- Análisis por tecnología -----

ks_technology <- analizar_distribuciones_ks(
  all_errors,
  dimension = "technology"
)

df_ks_technology <- ks_technology$comparaciones
sum_technology <- ks_technology$resumen


#----- Análisis por output -----

ks_output <- analizar_distribuciones_ks(
  all_errors,
  dimension = "output"
)

df_ks_output <- ks_output$comparaciones
sum_output <- ks_output$resumen
