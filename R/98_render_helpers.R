suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
  library(purrr)
  library(stringr)
})

catalogo_programas <- function(data = NULL) {
  if (is.null(data)) {
    if (!exists("PROGRAMAS")) {
      stop("Cargar primero R/01_load_data.R", call. = FALSE)
    }
    data <- PROGRAMAS
  }

  data %>% distinct(cohorte, facultad, programa)
}

safe_value <- function(x, fallback = "sin_dato") {
  if (is.null(x) || length(x) == 0) {
    return(fallback)
  }

  val <- x[[1]]
  if (is.na(val) || (is.character(val) && !nzchar(val))) {
    return(fallback)
  }

  val
}

slugify <- function(x, fallback = "SIN_DATO") {
  val <- safe_value(x, fallback)

  if (is.numeric(val)) {
    val <- as.character(val)
  }

  val <- iconv(as.character(val), from = "UTF-8", to = "ASCII//TRANSLIT")

  if (is.na(val)) {
    val <- safe_value(x, fallback)
  }

  val <- stringr::str_replace_all(val, "[^A-Za-z0-9]+", "_")
  val <- stringr::str_replace_all(val, "_+", "_")
  val <- stringr::str_replace_all(val, "^_+|_+$", "")

  if (!nzchar(val)) fallback else val
}

ensure_output_dirs <- function(...) {
  dirs <- unlist(list(...), use.names = FALSE)
  dirs <- dirs[!is.na(dirs)]
  walk(dirs, ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE, showWarnings = FALSE))
}

format_catalog_preview <- function(tbl) {
  if (nrow(tbl) == 0) {
    return(character())
  }

  tbl %>%
    mutate(
      resumen = glue(
        "cohorte={coalesce(as.character(cohorte), 'NA')}, facultad={coalesce(as.character(facultad), 'NA')}, programa={coalesce(as.character(programa), 'NA')}"
      )
    ) %>%
    pull(resumen)
}

resolve_program_row <- function(cohorte = NULL,
                                 facultad = NULL,
                                 programa = NULL,
                                 data = NULL) {
  catalogo <- catalogo_programas(data)

  candidatos <- catalogo

  if (!is.null(cohorte)) {
    candidatos <- candidatos %>% filter(.data$cohorte == cohorte)
  }

  if (!is.null(facultad)) {
    candidatos <- candidatos %>% filter(.data$facultad == facultad)
  }

  if (!is.null(programa)) {
    exactos <- candidatos %>% filter(.data$programa == programa)

    if (nrow(exactos) > 0) {
      candidatos <- exactos
    } else {
      candidatos <- candidatos %>%
        filter(stringr::str_detect(.data$programa, stringr::regex(programa, ignore_case = TRUE)))
    }
  }

  if (nrow(candidatos) == 0) {
    stop(
      glue("No se encontró una coincidencia con cohorte={coalesce(as.character(cohorte), 'NA')}, facultad={coalesce(as.character(facultad), 'NA')}, programa={coalesce(as.character(programa), 'NA')}. Usa catalogo_programas() para revisar opciones."),
      call. = FALSE
    )
  }

  if (nrow(candidatos) > 1) {
    preview <- format_catalog_preview(candidatos)
    stop(
      glue(
        "Se encontraron {nrow(candidatos)} coincidencias. Especifica más filtros.\n{paste0('- ', preview, collapse = '\n')}"
      ),
      call. = FALSE
    )
  }

  candidatos[1, ]
}

program_params <- function(cohorte = NULL,
                           facultad = NULL,
                           programa = NULL,
                           data = NULL) {
  fila <- resolve_program_row(
    cohorte = cohorte,
    facultad = facultad,
    programa = programa,
    data = data
  )

  list(
    cohorte = safe_value(fila$cohorte, fallback = NA),
    facultad = safe_value(fila$facultad, fallback = NA_character_),
    programa = safe_value(fila$programa, fallback = NA_character_)
  )
}

use_program_params <- function(cohorte = NULL,
                                facultad = NULL,
                                programa = NULL,
                                data = NULL,
                                .env = parent.frame()) {
  params <- program_params(
    cohorte = cohorte,
    facultad = facultad,
    programa = programa,
    data = data
  )

  list2env(params, envir = .env)
  invisible(params)
}
