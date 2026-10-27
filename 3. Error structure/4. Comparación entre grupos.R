
analizar_distribuciones <- function(
    data,
    dimension,
    variable_error = "error",
    alpha = 0.01,
    metodo_ajuste = "BH"
) {

  #----- Validaciones -----

  if (!dimension %in% names(data)) {
    stop("La dimensión no existe: ", dimension)
  }

  if (!variable_error %in% names(data)) {
    stop("La variable de error no existe: ", variable_error)
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
    ) %>%
    droplevels()

  errores <- split(datos$error, datos$categoria)
  errores <- errores[lengths(errores) > 0]
  categorias <- names(errores)

  if (length(categorias) < 2) {
    stop("Se necesitan al menos dos categorías.")
  }

  #----- ANOVA global -----

  modelo_aov <- aov(error ~ categoria, data = datos)
  tabla_aov <- summary(modelo_aov)[[1]]

  anova_global <- data.frame(
    dimension = dimension,
    variable_error = variable_error,
    n = nrow(datos),
    n_categorias = length(categorias),
    F = tabla_aov[1, "F value"],
    p_value = tabla_aov[1, "Pr(>F)"]
  )

  #----- ANOVA de Welch global -----

  test_welch <- tryCatch(
    oneway.test(error ~ categoria, data = datos,
                var.equal = FALSE),
    error = function(e) NULL
  )

  welch_global <- data.frame(
    dimension = dimension,
    F = if (is.null(test_welch)) NA_real_ else
      unname(test_welch$statistic),
    p_value = if (is.null(test_welch)) NA_real_ else
      test_welch$p.value
  )

  #----- Comparaciones por parejas -----

  combinaciones <- combn(categorias, 2, simplify = FALSE)

  resultados <- lapply(combinaciones, function(par) {

    i <- par[1]
    j <- par[2]

    error_i <- errores[[i]]
    error_j <- errores[[j]]

    test_ks <- tryCatch(
      suppressWarnings(ks.test(error_i, error_j)),
      error = function(e) NULL
    )

    test_mw <- tryCatch(
      suppressWarnings(
        wilcox.test(error_i, error_j, exact = FALSE)
      ),
      error = function(e) NULL
    )

    data.frame(
      categoria_i = i,
      categoria_j = j,
      n_i = length(error_i),
      n_j = length(error_j),
      D_ks = if (is.null(test_ks)) NA_real_ else
        unname(test_ks$statistic),
      p_value_ks = if (is.null(test_ks)) NA_real_ else
        test_ks$p.value,
      W_mw = if (is.null(test_mw)) NA_real_ else
        unname(test_mw$statistic),
      p_value_mw = if (is.null(test_mw)) NA_real_ else
        test_mw$p.value
    )
  })

  comparaciones <- bind_rows(resultados) %>%
    mutate(
      p_value_ks_ajustado =
        p.adjust(p_value_ks, method = metodo_ajuste),
      p_value_mw_ajustado =
        p.adjust(p_value_mw, method = metodo_ajuste),
      ks_difieren = as.integer(
        !is.na(p_value_ks_ajustado) &
          p_value_ks_ajustado < alpha
      ),
      mw_difieren = as.integer(
        !is.na(p_value_mw_ajustado) &
          p_value_mw_ajustado < alpha
      )
    )

  #----- Resumen por categoría -----

  resumen <- bind_rows(
    comparaciones %>%
      transmute(categoria = categoria_i,
                ks_difieren, mw_difieren),
    comparaciones %>%
      transmute(categoria = categoria_j,
                ks_difieren, mw_difieren)
  ) %>%
    group_by(categoria) %>%
    summarise(
      n_comparaciones = n(),
      prct_ks_diff = mean(ks_difieren) * 100,
      prct_mw_diff = mean(mw_difieren) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(prct_ks_diff))

  #----- Devolver resultados -----

  list(
    anova = anova_global,
    welch = welch_global,
    comparaciones = comparaciones,
    resumen = resumen
  )
}


#----- Análisis por región -----

resultados_region <- analizar_distribuciones(
  all_errors,
  dimension = "region",
  variable_error = "error"
)

resultados_region$anova
resultados_region$welch
resultados_region$comparaciones
resultados_region$resumen


#----- Análisis por tecnología -----
resultados_technology <- analizar_distribuciones(
  all_errors,
  dimension = "technology",
  variable_error = "error"
)

resultados_technology$anova
resultados_technology$welch
resultados_technology$comparaciones
resultados_technology$resumen
