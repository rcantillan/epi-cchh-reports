suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(glue)
  library(quarto)
  library(stringr)
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
})
