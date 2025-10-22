
suppressPackageStartupMessages({
  library(gt); library(dplyr); library(scales); library(glue); library(tibble)
})

if (!exists("filter_program_scope")) {
  filter_program_scope <- function(df, cohorte, facultad = NULL, programa = NULL) {
    out <- df %>% filter(cohorte == !!cohorte)

    if (!is.null(programa) && !is.na(programa) && nzchar(as.character(programa))) {
      out <- out %>% filter(programa == !!programa)
    }

    if (!is.null(facultad) && !is.na(facultad) && nzchar(as.character(facultad))) {
      scoped <- out %>% filter(facultad == !!facultad)
      if (nrow(scoped) > 0) {
        out <- scoped
      }
    }

    out
  }
}

gt_summary_table <- function(df, title = NULL, subtitle = NULL,
                             percent_cols = NULL, number_cols = NULL,
                             integer_cols = NULL, missing_text = "—") {
  tab <- gt(df) |>
    tab_header(title = title, subtitle = subtitle) |>
    tab_options(table.font.size = px(12)) |>
    fmt_missing(columns = everything(), missing_text = missing_text)

  if (!is.null(integer_cols)) {
    cols <- intersect(integer_cols, names(df))
    if (length(cols)) {
      tab <- tab |>
        fmt_number(columns = all_of(cols), decimals = 0, use_seps = TRUE)
    }
  }

  if (!is.null(number_cols)) {
    cols <- intersect(number_cols, names(df))
    if (length(cols)) {
      tab <- tab |>
        fmt_number(columns = all_of(cols), decimals = 1, use_seps = TRUE)
    }
  }

  if (!is.null(percent_cols)) {
    cols <- intersect(percent_cols, names(df))
    if (length(cols)) {
      tab <- tab |>
        fmt_percent(columns = all_of(cols), decimals = 1)
    }
  }

  tab
}

first_or_default <- function(x, default) {
  val <- dplyr::first(x)
  if (is.null(val) || is.na(val)) return(default)
  val_chr <- as.character(val)
  if (!nzchar(val_chr)) default else val_chr
}

pull_or_na <- function(df, col) {
  if (is.null(col) || is.na(col) || !col %in% names(df)) return(NA_real_)
  dplyr::first(df[[col]])
}

table_programa_cobertura <- function(programas_df, cohorte, facultad = NULL, programa = NULL) {
  if (!exists("filter_program_scope")) {
    stop("filter_program_scope() debe estar disponible (ver R/02_derive_indicators.R)")
  }

  row <- filter_program_scope(programas_df, cohorte, facultad, programa) %>%
    slice_head(n = 1)

  if (nrow(row) == 0) {
    return(NULL)
  }

  fac_label <- dplyr::coalesce(dplyr::first(row$facultad), facultad)
  prog_label <- dplyr::coalesce(dplyr::first(row$programa), programa)
  coh <- dplyr::first(row$cohorte)
  n_total <- dplyr::first(row$n_total)

  pruebas <- c("Lectura", "Escritura", "Matemática", "Inglés")
  rindio_cols <- c("n_rindio_lectura", "n_rindio_escritura", "n_rindio_matematica", "n_rindio_ingles")
  cobertura_cols <- c("cobertura_lectura", "cobertura_escritura", "cobertura_matematica", "cobertura_ingles")
  promedio_cols <- c("prom_lectura", "prom_escritura", "prom_matematica", NA_character_)

  tabla <- tibble(
    prueba = factor(pruebas, levels = pruebas),
    n_rindio = vapply(rindio_cols, function(col) suppressWarnings(as.integer(pull_or_na(row, col))), integer(1)),
    cobertura = vapply(cobertura_cols, function(col) suppressWarnings(as.numeric(pull_or_na(row, col))), numeric(1)),
    promedio = vapply(promedio_cols, function(col) {
      if (is.na(col)) return(NA_real_)
      suppressWarnings(as.numeric(pull_or_na(row, col)))
    }, numeric(1))
  ) %>%
    mutate(across(where(is.double), ~ ifelse(is.nan(.x), NA_real_, .x))) %>%
    filter(!(is.na(n_rindio) & is.na(cobertura) & is.na(promedio)))

  if (nrow(tabla) == 0) {
    return(NULL)
  }

  fac_txt <- first_or_default(fac_label, "Facultad no especificada")
  prog_txt <- first_or_default(prog_label, "Programa no especificado")
  subtitle <- glue("{fac_txt} · {prog_txt} — Cohorte {coh}")
  if (!is.na(n_total)) {
    subtitle <- glue("{subtitle} | Total inscritos: {comma(n_total, accuracy = 1)}")
  }

  out <- gt_summary_table(
    tabla,
    title = "Cobertura por prueba",
    subtitle = subtitle,
    percent_cols = "cobertura",
    number_cols = "promedio",
    integer_cols = "n_rindio"
  ) |>
    cols_label(
      prueba = "Prueba",
      n_rindio = "n rindió",
      cobertura = "Cobertura",
      promedio = "Puntaje promedio"
    )

  if (all(is.na(tabla$promedio))) {
    out <- out |> cols_hide("promedio")
  }

  out
}

table_programa_desglose <- function(desglose_df, cohorte, facultad = NULL, programa = NULL) {
  if (!exists("filter_program_scope")) {
    stop("filter_program_scope() debe estar disponible (ver R/02_derive_indicators.R)")
  }

  tabla <- filter_program_scope(desglose_df, cohorte, facultad, programa) %>%
    arrange(prueba, tipo, etiqueta)

  if (nrow(tabla) == 0) {
    return(NULL)
  }

  fac_label <- dplyr::coalesce(dplyr::first(tabla$facultad), facultad)
  prog_label <- dplyr::coalesce(dplyr::first(tabla$programa), programa)
  coh <- dplyr::first(tabla$cohorte)

  fac_txt <- first_or_default(fac_label, "Facultad no especificada")
  prog_txt <- first_or_default(prog_label, "Programa no especificado")

  out <- gt_summary_table(
    tabla %>% select(prueba, tipo, etiqueta, n, pct, media),
    title = "Detalle por indicador",
    subtitle = glue("{fac_txt} · {prog_txt} — Cohorte {coh}"),
    percent_cols = "pct",
    number_cols = "media",
    integer_cols = "n"
  ) |>
    cols_label(
      prueba = "Prueba",
      tipo = "Tipo",
      etiqueta = "Indicador",
      n = "n",
      pct = "% estudiantes",
      media = "Puntaje promedio"
    )

  if (all(is.na(tabla$pct))) {
    out <- out |> cols_hide("pct")
  }

  out
}
