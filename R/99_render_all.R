
suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(glue); library(quarto)
})

# Requiere que existan los objetos PROGRAMAS (y/o tablas auxiliares)
if (!exists("PROGRAMAS")) stop("Cargar primero R/01_load_data.R")

# Catálogo único de combinaciones
catalogo <- PROGRAMAS %>% distinct(cohorte, facultad, programa)

if (nrow(catalogo) == 0) {
  message(">> Catálogo vacío. Verifica que los Excel hayan sido parseados en 01_load_data.R")
}

# Render por cada combinación
walk(seq_len(nrow(catalogo)), function(i) {
  row <- catalogo[i,]
  out_file_pdf  <- glue("output/2025/pdf/FACULTAD={row$facultad}_PROGR={row$programa}_CCHH_{row$cohorte}.pdf")
  out_file_html <- glue("output/2025/html/FACULTAD={row$facultad}_PROGR={row$programa}_CCHH_{row$cohorte}.html")

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
