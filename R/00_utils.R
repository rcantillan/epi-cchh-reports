
# Utils: etiquetas, formatos, paletas, reglas
library(glue)
library(scales)

fmt_pct <- function(x, digits = 1) {
  percent(x, accuracy = 10^-digits, big.mark = ".", decimal.mark = ",")
}

fmt_num <- function(x, digits = 1) {
  number(x, accuracy = 10^-digits, big.mark = ".", decimal.mark = ",")
}

mask_small <- function(n, thresh = 5) {
  ifelse(!is.na(n) & n < thresh, "<5", as.character(n))
}

palette_levels <- c("Bajo"="#c7e9b4","Medio"="#7fcdbb","Adecuado"="#1d91c0")
palette_cefr   <- c("A1"="#f0f9e8","A2"="#bae4bc","A2+"="#7bccc4","B1"="#2b8cbe")
