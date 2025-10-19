
# Derivaciones y helpers para construir insumos de figuras/tablas
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(glue); library(forcats)
})

# Ejemplo: niveles apilados por prueba
levels_stacked <- function(niveles_df, cohorte, facultad, programa) {
  niveles_df %>%
    filter(cohorte == !!cohorte, facultad == !!facultad, programa == !!programa) %>%
    mutate(nivel = fct_relevel(nivel, "Bajo","Medio","Adecuado")) %>%
    arrange(prueba, nivel)
}

# Ejemplo: CEFR apilado
cefr_stacked <- function(cefr_df, cohorte, facultad, programa) {
  cefr_df %>%
    filter(cohorte == !!cohorte, facultad == !!facultad, programa == !!programa) %>%
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
