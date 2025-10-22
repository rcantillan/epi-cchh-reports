
# Derivaciones y helpers para construir insumos de figuras/tablas
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(glue); library(forcats)
})

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

is_truthy_string <- function(x) {
  !is.null(x) && !is.na(x) && nzchar(x)
}

filter_program_scope <- function(df, cohorte, facultad = NULL, programa = NULL) {
  out <- df %>% filter(cohorte == !!cohorte)

  if (is_truthy_string(programa)) {
    out <- out %>% filter(programa == !!programa)
  }

  if (is_truthy_string(facultad)) {
    scoped <- out %>% filter(facultad == !!facultad)
    if (nrow(scoped) > 0) {
      out <- scoped
    }
  }

  out
}

program_row <- function(programas_df, cohorte, facultad = NULL, programa = NULL) {
  filter_program_scope(programas_df, cohorte, facultad, programa) %>%
    slice_head(n = 1)
}

# Ejemplo: niveles apilados por prueba
levels_stacked <- function(niveles_df, cohorte, facultad, programa) {
  filter_program_scope(niveles_df, cohorte, facultad, programa) %>%
    mutate(nivel = fct_relevel(nivel, "Bajo","Medio","Adecuado")) %>%
    arrange(prueba, nivel)
}

# Ejemplo: CEFR apilado
cefr_stacked <- function(cefr_df, cohorte, facultad, programa) {
  filter_program_scope(cefr_df, cohorte, facultad, programa) %>%
    mutate(nivel_cefr = fct_relevel(nivel_cefr, "A1","A2","A2+","B1")) %>%
    arrange(nivel_cefr)
}

# Notas de cautela por n pequeño
build_caution_note <- function(n) {
  if (!is.na(n) && n < 10) {
    return("Nota: distribución ilustrativa con n<10; interpretar con cautela.")
  } else {
    return(NULL)
  }
}
