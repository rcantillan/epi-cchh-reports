
# Lectura y estandarización de Excel a un esquema común
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(purrr); library(forcats); library(glue)
})

raw_dir <- "data/raw"

# Helper para lectura segura de hojas
read_any_sheet <- function(path) {
  suppressWarnings(readxl::read_excel(path))
}

# 1) Listado de archivos
xlsx_files <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)

if (length(xlsx_files) == 0) {
  message(">> No se encontraron archivos .xlsx en data/raw. Coloca los Excel y vuelve a ejecutar.")
}

# 2) Estructuras objetivo (tibbles vacíos como guía)
programas_df <- tibble(
  cohorte = integer(), facultad = character(), programa = character(),
  n_total = integer(),
  n_rindio_lectura = integer(), n_rindio_escritura = integer(),
  n_rindio_matematica = integer(), n_rindio_ingles = integer(),
  cobertura_lectura = double(), cobertura_escritura = double(),
  cobertura_matematica = double(), cobertura_ingles = double(),
  prom_lectura = double(), prom_escritura = double(), prom_matematica = double()
)

niveles_df <- tibble(
  cohorte = integer(), facultad = character(), programa = character(),
  prueba = factor(levels = c("Lectura","Escritura","Matemática")),
  nivel = factor(levels = c("Bajo","Medio","Adecuado")),
  n = integer(), pct = double()
)

# CEFR Inglés
cefr_df <- tibble(
  cohorte = integer(), facultad = character(), programa = character(),
  nivel_cefr = factor(levels = c("A1","A2","A2+","B1")),
  n = integer(), pct = double()
)

# Desglose por Dimensión/Eje/Indicador
desglose_df <- tibble(
  cohorte = integer(), facultad = character(), programa = character(),
  prueba = factor(levels = c("Lectura","Escritura","Matemática")),
  tipo = factor(levels = c("Dimensión","Eje","Indicador")),
  etiqueta = character(),
  n = integer(), pct = double(), media = double()
)

# 3) Pipeline (adaptar al layout concreto de tus archivos)
#    Aquí dejamos un ejemplo de cómo mapear si las hojas vienen con nombres similares.
parse_one_file <- function(path) {
  # TODO: adaptar a cada layout real; este esqueleto asume hojas por prueba
  # y tablas con columnas estandarizables mediante clean_names().
  # Se sugiere crear funciones lectoras por facultad si hay variaciones.
  fname <- basename(path)
  # heurística simple para programa/facultad desde nombre de archivo:
  program_guess  <- fname %>% str_remove("\\.xlsx$") %>% str_replace("_", " ")

  # Lectura genérica
  # (en práctica: usar read_excel(path, sheet="Lectura"), etc.)
  # Aquí simulamos data frames vacíos para ilustrar la estructura final.
  list(
    programas = programas_df[0,],
    niveles   = niveles_df[0,],
    cefr      = cefr_df[0,],
    desglose  = desglose_df[0,]
  )
}

parsed <- map(xlsx_files, parse_one_file)

# 4) Bind rows a objetos globales
PROGRAMAS <- bind_rows(map(parsed, "programas"))
NIVELES   <- bind_rows(map(parsed, "niveles"))
CEFR      <- bind_rows(map(parsed, "cefr"))
DESGLOSE  <- bind_rows(map(parsed, "desglose"))

# 5) Validaciones básicas
validate_percentages <- function(df, group_cols, level_col = NULL) {
  if (nrow(df) == 0) return(invisible(NULL))
  tmp <- df %>% group_by(across(all_of(group_cols)))
  if (!is.null(level_col)) tmp <- tmp %>% group_by(across(all_of(group_cols)), .add = TRUE)
  chk <- tmp %>% summarise(sum_pct = sum(pct, na.rm = TRUE), .groups = "drop")
  bad <- chk %>% filter(abs(sum_pct - 1) > 0.02)
  if (nrow(bad) > 0) {
    warning("Grupos con porcentajes que no suman ~1:\n", paste(capture.output(print(bad)), collapse="\n"))
  }
}

validate_percentages(NIVELES, c("cohorte","facultad","programa","prueba"))
validate_percentages(CEFR,    c("cohorte","facultad","programa"))
# DESGLOSE puede sumar por tipo

# 6) Guardar intermedios si se desea
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
# readr::write_csv(PROGRAMAS, "data/processed/programas.csv")
# readr::write_csv(NIVELES,   "data/processed/niveles.csv")
# readr::write_csv(CEFR,      "data/processed/cefr.csv")
# readr::write_csv(DESGLOSE,  "data/processed/desglose.csv")

message(">> Objetos disponibles: PROGRAMAS, NIVELES, CEFR, DESGLOSE")
