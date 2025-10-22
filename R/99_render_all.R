suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(glue)
  library(quarto)
  library(stringr)
  library(dplyr); library(purrr); library(glue); library(quarto); library(stringr)
})

source("R/98_render_helpers.R")

if (!exists("PROGRAMAS")) {
  stop("Cargar primero R/01_load_data.R")
}

catalogo <- catalogo_programas(PROGRAMAS)

if (nrow(catalogo) == 0) {
  message(">> Catálogo vacío. Verifica que los Excel hayan sido parseados en 01_load_data.R")
}

render_catalog_entry <- function(row,
                                 fecha_corte = Sys.Date(),
                                 output_dir = "output") {
  cohorte_slug <- safe_value(row$cohorte, fallback = "cohorte")
  facultad_slug <- slugify(row$facultad)
  programa_slug <- slugify(row$programa)

  base_dir <- file.path(output_dir, as.character(cohorte_slug))
  ensure_output_dirs(base_dir, file.path(base_dir, "pdf"), file.path(base_dir, "html"))

  formatos <- c("pdf", "html")

  for (fmt in formatos) {
    out_file <- file.path(
      base_dir,
      fmt,
      glue("FACULTAD={facultad_slug}_PROGR={programa_slug}_CCHH_{cohorte_slug}.{fmt}")
    )

    quarto_render(
      input = "templates/report_template.qmd",
      execute_params = list(
        cohorte = row$cohorte,
        facultad = row$facultad,
        programa = row$programa,
        fecha_corte = fecha_corte
      ),
      output_file = out_file
    )
  }
}

walk(seq_len(nrow(catalogo)), function(i) {
  render_catalog_entry(catalogo[i, , drop = FALSE])
# Helpers --------------------------------------------------------------------

safe_value <- function(x, fallback = "sin_dato") {
  if (is.null(x) || length(x) == 0) return(fallback)
  val <- x[[1]]
  if (is.na(val) || (is.character(val) && !nzchar(val))) return(fallback)
  val
}

slugify <- function(x, fallback = "SIN_DATO") {
  val <- safe_value(x, fallback)
  if (is.numeric(val)) val <- as.character(val)
  val <- iconv(as.character(val), from = "UTF-8", to = "ASCII//TRANSLIT")
  if (is.na(val)) val <- safe_value(x, fallback)
  val <- stringr::str_replace_all(val, "[^A-Za-z0-9]+", "_")
  val <- stringr::str_replace_all(val, "_+", "_")
  val <- stringr::str_replace_all(val, "^_+|_+$", "")
  if (!nzchar(val)) fallback else val
}

ensure_output_dirs <- function(root_dir, ...) {
  dirs <- file.path(root_dir, ...)
  purrr::walk(dirs, ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE, showWarnings = FALSE))
}

# Render por cada combinación
walk(seq_len(nrow(catalogo)), function(i) {
  row <- catalogo[i,]
  cohorte <- safe_value(row$cohorte, fallback = "cohorte")
  facultad_slug <- slugify(row$facultad)
  programa_slug <- slugify(row$programa)

  output_root <- file.path("output", as.character(cohorte))
  ensure_output_dirs(output_root, "pdf", "html")

  out_file_pdf  <- file.path(output_root, "pdf", glue("FACULTAD={facultad_slug}_PROGR={programa_slug}_CCHH_{cohorte}.pdf"))
  out_file_html <- file.path(output_root, "html", glue("FACULTAD={facultad_slug}_PROGR={programa_slug}_CCHH_{cohorte}.html"))

  quarto_render(
    input  = "templates/report_template.qmd",
    execute_params = list(
      cohorte     = row$cohorte,
      facultad    = row$facultad,
      programa    = row$programa,
      fecha_corte = Sys.Date()
    ),
    output_file = out_file_pdf
  )

  quarto_render(
    input  = "templates/report_template.qmd",
    execute_params = list(
      cohorte     = row$cohorte,
      facultad    = row$facultad,
      programa    = row$programa,
      fecha_corte = Sys.Date()
    ),
    output_file = out_file_html
  )
})
