
# Lectura y estandarización de Excel a un esquema común
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(purrr); library(forcats); library(glue)
  library(readr); library(tibble)
})

raw_dir <- "data/raw"

# -----------------------------------------------------------------------------
# Helpers para detectar cabeceras y estandarizar archivos "Resultados Individuales"
# -----------------------------------------------------------------------------

guess_data_start <- function(df0) {
  v <- suppressWarnings(readr::parse_double(df0[[1]]))
  idx <- which(!is.na(v) & v > 1e6)
  if (length(idx)) min(idx) else NA_integer_
}

guess_header_row <- function(df0, data_start) {
  if (is.na(data_start) || data_start <= 1) return(NA_integer_)
  prev_df <- as.data.frame(df0[seq_len(data_start - 1), , drop = FALSE], stringsAsFactors = FALSE)
  nonempty <- rowSums(!is.na(prev_df) & prev_df != "")
  idx <- which(nonempty >= 2)
  if (length(idx)) max(idx) else NA_integer_
}

make_names_from_block <- function(df0, header_row, data_start) {
  if (is.na(header_row)) {
    raw_names <- names(df0)
  } else {
    block <- as.data.frame(df0[seq(from = header_row, to = data_start - 1), , drop = FALSE],
                           stringsAsFactors = FALSE)
    collapse_col <- function(x) {
      x <- x[!is.na(x) & x != ""]
      if (!length(x)) NA_character_ else paste(x, collapse = " | ")
    }
    raw_names <- vapply(block, collapse_col, character(1))
    empties <- is.na(raw_names) | raw_names == ""
    if (any(empties)) {
      idx <- seq_along(raw_names)
      raw_names[empties] <- paste0("col_", idx[empties])
    }
  }
  janitor::make_clean_names(raw_names)
}

first_existing_col <- function(df, candidates) {
  keep <- candidates[candidates %in% names(df)]
  if (length(keep)) keep[1] else NA_character_
}

standardize_core_names <- function(dat) {
  nms <- names(dat)
  pats <- list(
    rut        = "(^rut(\\s|_|$)|^rut_estudiante|^id.*estudiante)",
    dv         = "^(dv|digito|dígito)",
    primer     = "^primer.*apellido|apellid.*paterno",
    segundo    = "^segundo.*apellido|apellid.*materno",
    nombres    = "^nombres?$",
    estado_hab = "^estado.*habilitaci[oó]n",
    estatus    = "^estatus.*rendici[oó]n|^estado.*rendici[oó]n",
    fecha      = "^fecha.*rendici[oó]n",
    justif     = "^justificaci[oó]n.*no.*rend"
  )

  for (k in names(pats)) {
    hit <- stringr::str_which(nms, stringr::regex(pats[[k]], ignore_case = TRUE))
    if (length(hit)) {
      old <- nms[hit[1]]
      new <- switch(k,
        rut = "rut",
        dv = "dv",
        primer = "primer_apellido",
        segundo = "segundo_apellido",
        nombres = "nombres",
        estado_hab = "estado_habilitacion",
        estatus = "estatus_rendicion",
        fecha = "fecha_rendicion",
        justif = "justificacion_no_rendicion",
        old
      )
      names(dat)[names(dat) == old] <- new
    }
  }
  dat
}

fallback_positionals <- function(dat) {
  pos_map <- c(
    "rut", "dv", "primer_apellido", "segundo_apellido", "nombres",
    "estado_habilitacion", "estatus_rendicion", "fecha_rendicion", "justificacion_no_rendicion"
  )
  for (i in seq_along(pos_map)) {
    tgt <- pos_map[i]
    if (!tgt %in% names(dat) && i <= ncol(dat)) {
      names(dat)[i] <- tgt
    }
  }
  dat
}

looks_like_comp_score <- function(x) {
  x2 <- if (is.factor(x)) as.character(x) else x
  x2 <- x2[!is.na(x2) & x2 != ""]
  if (!length(x2)) return(FALSE)
  ok <- stringr::str_detect(x2, stringr::regex("^(nr|n\\s*r|[0-3])$", ignore_case = TRUE)) |
    stringr::str_detect(x2, stringr::regex("^\\d{1,2}$", ignore_case = TRUE))
  mean(ok) >= 0.7
}

standardize_competencias <- function(dat) {
  nms <- names(dat)
  ren <- list(
    comp_11          = "^(x\\s*)?1[\\._\\- ]?1\\b|distingue\\s+las\\s+ideas\\s+centrales",
    comp_12          = "^(x\\s*)?1[\\._\\- ]?2\\b|explica\\s+conceptos\\s+te[oó]ricos?\\s*vinculados?",
    comp_32a         = "^(x\\s*)?3[\\._\\- ]?2[\\._\\- ]?a\\b",
    comp_32b         = "^(x\\s*)?3[\\._\\- ]?2[\\._\\- ]?b\\b",
    comp_32c         = "^(x\\s*)?3[\\._\\- ]?2[\\._\\- ]?c\\b",
    comp_41          = "^(x\\s*)?4[\\._\\- ]?1\\b|reconoce\\s+los\\s+componentes\\s+de\\s+una\\s+investigaci[oó]n",
    comp_42          = "^(x\\s*)?4[\\._\\- ]?2\\b|eval[íi]a\\s+la\\s+coherencia\\s+entre\\s+los\\s+distintos",
    puntaje_total    = "puntaje.*total.*estudiante",
    porcentaje_logro = "porcentaje.*logro|%.*logro",
    categoria_global = "categor[ií]a.*resultado.*global.*estudiante",
    incluido_listado = "incluido.*listado"
  )

  for (k in names(ren)) {
    hit <- stringr::str_which(nms, stringr::regex(ren[[k]], ignore_case = TRUE))
    if (length(hit)) {
      old <- nms[hit[1]]
      names(dat)[names(dat) == old] <- k
    }
  }

  nm <- names(dat)
  if (!"comp_32a" %in% nm) {
    hit <- which(stringr::str_detect(nm, stringr::regex("^x3_2_a", ignore_case = TRUE)))
    if (length(hit)) nm[hit[1]] <- "comp_32a"
  }
  if (!"comp_32b" %in% nm) {
    hit <- which(stringr::str_detect(nm, stringr::regex("^x3_2_b", ignore_case = TRUE)))
    if (length(hit)) nm[hit[1]] <- "comp_32b"
  }
  if (!"comp_32c" %in% nm) {
    hit <- which(stringr::str_detect(nm, stringr::regex("^x3_2_c", ignore_case = TRUE)))
    if (length(hit)) nm[hit[1]] <- "comp_32c"
  }
  names(dat) <- nm
  nm <- names(dat)

  idx_just <- which(nm == "justificacion_no_rendicion")
  idx_32a  <- which(nm == "comp_32a")
  idx_32c  <- which(nm == "comp_32c")

  if (length(idx_just) && length(idx_32a)) {
    rng <- seq(from = idx_just + 1, to = idx_32a - 1)
    rng <- rng[rng >= 1 & rng <= ncol(dat)]
    if (length(rng)) {
      cand <- nm[rng]
      mask <- vapply(dat[cand], looks_like_comp_score, logical(1))
      cand2 <- cand[mask]
      if (length(cand2) >= 2) {
        nm[match(tail(cand2, 2), nm)] <- c("comp_11", "comp_12")
      } else if (length(cand) >= 2) {
        nm[match(tail(cand, 2), nm)] <- c("comp_11", "comp_12")
      }
      names(dat) <- nm
      nm <- names(dat)
    }
  }

  if (length(idx_32c)) {
    pos <- idx_32c + 1
    if (!"comp_41" %in% nm && pos <= ncol(dat)) { nm[pos] <- "comp_41"; pos <- pos + 1 }
    if (!"comp_42" %in% nm && pos <= ncol(dat)) { nm[pos] <- "comp_42"; pos <- pos + 1 }
    if (!"puntaje_total" %in% nm && pos <= ncol(dat)) { nm[pos] <- "puntaje_total"; pos <- pos + 1 }
    if (!"porcentaje_logro" %in% nm && pos <= ncol(dat)) { nm[pos] <- "porcentaje_logro"; pos <- pos + 1 }
    if (!"categoria_global" %in% nm && pos <= ncol(dat)) { nm[pos] <- "categoria_global"; pos <- pos + 1 }
    names(dat) <- nm
  }

  dat
}

repair_person_names <- function(df) {
  required <- c("primer_apellido", "segundo_apellido", "nombres")
  if (!all(required %in% names(df))) return(df)

  split_full <- function(txt) {
    if (is.na(txt) || txt == "") return(list(nombres = NA_character_, ap1 = NA_character_, ap2 = NA_character_))
    toks <- stringr::str_squish(txt)
    toks <- stringr::str_split(toks, "\\s+")[[1]]
    toks <- toks[toks != "" & !is.na(toks)]
    if (length(toks) < 3) return(list(nombres = NA_character_, ap1 = NA_character_, ap2 = NA_character_))
    ap2 <- toks[length(toks)]
    ap1 <- toks[length(toks) - 1]
    nom <- if (length(toks) > 2) paste(toks[seq_len(length(toks) - 2)], collapse = " ") else NA_character_
    list(nombres = nom, ap1 = ap1, ap2 = ap2)
  }

  need_fix <- vapply(df$primer_apellido, function(x) {
    if (is.na(x) || x == "") {
      FALSE
    } else {
      stringr::str_count(x, "\\S+") >= 3
    }
  }, logical(1))

  if (any(need_fix)) {
    fixed <- lapply(df$primer_apellido[need_fix], split_full)
    df$nombres[need_fix]          <- vapply(fixed, `[[`, character(1), "nombres")
    df$primer_apellido[need_fix]  <- vapply(fixed, `[[`, character(1), "ap1")
    df$segundo_apellido[need_fix] <- vapply(fixed, `[[`, character(1), "ap2")
  }
  df
}

parse_resultados_individuales_v2 <- function(path, sheet = NULL, compute_total = TRUE) {
  stopifnot(file.exists(path))

  if (is.null(sheet)) {
    shs <- readxl::excel_sheets(path)
    s_low <- stringr::str_to_lower(shs)
    cand <- shs[stringr::str_detect(s_low, "resultado") & stringr::str_detect(s_low, "individual")]
    if (!length(cand)) cand <- shs[stringr::str_detect(s_low, "individ")]
    if (!length(cand)) cand <- shs
    sheet <- cand[1]
    message("Usando hoja: ", sheet)
  }

  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")
  raw <- janitor::remove_empty(raw, which = c("rows", "cols"))

  data_start <- guess_data_start(raw)
  if (is.na(data_start)) stop("No pude detectar el inicio de datos (ID numérico) en la 1ª columna.")
  hdr_row <- guess_header_row(raw, data_start)

  col_nms <- make_names_from_block(raw, hdr_row, data_start)

  dat <- tibble::as_tibble(raw[seq(from = data_start, to = nrow(raw)), , drop = FALSE])
  names(dat) <- col_nms

  dat <- dat %>% mutate(across(everything(), ~ na_if(.x, "")))

  dat <- dat %>% standardize_core_names() %>% fallback_positionals()

  id_candidates <- c("datos_de_estudiantes_de_la_cohorte", "rut", "id_estudiante", "id", names(dat)[1])
  id_col <- first_existing_col(dat, id_candidates)
  if (!is.na(id_col)) {
    id_vals <- as.character(dat[[id_col]])
    dat <- dat %>% mutate(id_estudiante = id_vals)
  } else {
    dat <- dat %>% mutate(id_estudiante = NA_character_)
  }

  dat <- repair_person_names(dat)

  has_3 <- all(c("nombres", "primer_apellido", "segundo_apellido") %in% names(dat))
  dat <- dat %>% mutate(
    nombre_completo = if (has_3) stringr::str_squish(paste(nombres, primer_apellido, segundo_apellido)) else NA_character_
  )

  n <- nrow(dat)
  rindio_from_fecha <- if ("fecha_rendicion" %in% names(dat)) {
    tmp <- tolower(ifelse(is.na(dat$fecha_rendicion), "", dat$fecha_rendicion))
    ifelse(stringr::str_detect(tmp, stringr::regex("no\\s*rendida", ignore_case = TRUE)), 0L,
           ifelse(stringr::str_detect(tmp, stringr::regex("rendida", ignore_case = TRUE)), 1L, NA_integer_))
  } else rep(NA_integer_, n)

  rindio_from_estat <- if ("estatus_rendicion" %in% names(dat)) {
    tmp <- tolower(ifelse(is.na(dat$estatus_rendicion), "", dat$estatus_rendicion))
    ifelse(stringr::str_detect(tmp, stringr::regex("no\\s*rend", ignore_case = TRUE)), 0L,
           ifelse(stringr::str_detect(tmp, stringr::regex("rend", ignore_case = TRUE)), 1L, NA_integer_))
  } else rep(NA_integer_, n)

  dat$rindio <- dplyr::coalesce(rindio_from_fecha, rindio_from_estat)

  dat <- standardize_competencias(dat)

  comp_guess <- c("comp_11", "comp_12", "comp_32a", "comp_32b", "comp_32c", "comp_41", "comp_42")
  comp_cols <- intersect(names(dat), comp_guess)
  if (length(comp_cols)) {
    dat <- dat %>% mutate(across(all_of(comp_cols), ~ suppressWarnings(readr::parse_number(as.character(.x)))))
  }

  if ("puntaje_total" %in% names(dat)) {
    dat <- dat %>% mutate(puntaje_total = suppressWarnings(readr::parse_number(as.character(puntaje_total))))
  } else if (compute_total && length(comp_cols)) {
    dat <- dat %>% mutate(puntaje_total = rowSums(across(all_of(comp_cols)), na.rm = TRUE))
  }

  if ("porcentaje_logro" %in% names(dat)) {
    dat <- dat %>% mutate(porcentaje_logro = readr::parse_number(as.character(porcentaje_logro)))
  }

  dat
}

first_non_na <- function(x) {
  out <- x[!is.na(x)]
  if (length(out)) out[1] else NA
}

guess_metadata_from_filename <- function(path) {
  fname <- basename(path)
  stem <- stringr::str_remove(fname, "\\.xlsx$")
  stem <- stringr::str_replace_all(stem, "_", " ")
  stem <- stringr::str_replace_all(stem, "\\s+", " ")
  stem <- stringr::str_squish(stem)

  year <- stringr::str_extract(stem, "(20\\d{2})")
  cohorte <- if (!is.na(year)) as.integer(year) else NA_integer_

  program <- stringr::str_remove(stem, "(?i)^bd\\s*reporte\\s*epi\\s*")
  program <- stringr::str_remove(program, "(?i)^br\\s*reporte\\s*")
  program <- stringr::str_remove(program, "(?i)^reporte\\s*epi\\s*")
  program <- stringr::str_remove(program, "(?i)^bd\\s*reporte\\s*")
  program <- stringr::str_replace(program, "(?i)_rev$", "")
  program <- stringr::str_replace_all(program, "\\(.*\\)$", "")
  program <- stringr::str_squish(program)

  list(
    cohorte = cohorte,
    facultad = NA_character_,
    programa = if (nzchar(program)) stringr::str_to_title(program) else program,
    source_file = fname
  )
}

map_categoria_to_nivel <- function(x) {
  if (is.null(x)) return(rep(NA_character_, length(x)))
  purrr::map_chr(x, function(val) {
    if (is.na(val) || val == "") return(NA_character_)
    low <- stringr::str_to_lower(val)
    dplyr::case_when(
      stringr::str_detect(low, "adecuad") ~ "Adecuado",
      stringr::str_detect(low, "alto") ~ "Adecuado",
      stringr::str_detect(low, "medio") ~ "Medio",
      stringr::str_detect(low, "bajo") ~ "Bajo",
      stringr::str_detect(low, "inicio|emergente|insuficiente|en proceso") ~ "Bajo",
      TRUE ~ NA_character_
    )
  })
}

summarise_programas <- function(dat) {
  if (nrow(dat) == 0) return(programas_df[0, ])
  n_total <- nrow(dat)
  rindio_total <- sum(dat$rindio == 1, na.rm = TRUE)
  cobertura_lectura <- if (n_total > 0) rindio_total / n_total else NA_real_
  prom_lectura <- if ("puntaje_total" %in% names(dat) && any(!is.na(dat$puntaje_total))) {
    mean(dat$puntaje_total, na.rm = TRUE)
  } else {
    NA_real_
  }

  tibble(
    cohorte = first_non_na(dat$cohorte),
    facultad = first_non_na(dat$facultad),
    programa = first_non_na(dat$programa),
    n_total = n_total,
    n_rindio_lectura = rindio_total,
    n_rindio_escritura = NA_integer_,
    n_rindio_matematica = NA_integer_,
    n_rindio_ingles = NA_integer_,
    cobertura_lectura = cobertura_lectura,
    cobertura_escritura = NA_real_,
    cobertura_matematica = NA_real_,
    cobertura_ingles = NA_real_,
    prom_lectura = prom_lectura,
    prom_escritura = NA_real_,
    prom_matematica = NA_real_
  )
}

summarise_niveles <- function(dat) {
  if (!"categoria_global" %in% names(dat)) return(niveles_df[0, ])
  niveles <- map_categoria_to_nivel(dat$categoria_global)
  tmp <- dat %>% mutate(nivel = niveles) %>% filter(!is.na(nivel))
  if (nrow(tmp) == 0) return(niveles_df[0, ])

  tmp %>%
    mutate(prueba = factor("Lectura", levels = levels(niveles_df$prueba)),
           nivel = factor(nivel, levels = levels(niveles_df$nivel))) %>%
    filter(!is.na(prueba), !is.na(nivel)) %>%
    group_by(cohorte, facultad, programa, prueba, nivel) %>%
    summarise(n = dplyr::n(), .groups = "drop_last") %>%
    group_by(cohorte, facultad, programa, prueba) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
}

summarise_cefr <- function(dat) {
  if (!"categoria_global" %in% names(dat)) return(cefr_df[0, ])
  vals <- dat$categoria_global
  matches <- stringr::str_detect(vals, stringr::regex("^(a1|a2|a2\+|b1)$", ignore_case = TRUE))
  if (!any(matches, na.rm = TRUE)) return(cefr_df[0, ])
  tmp <- dat[matches, , drop = FALSE]
  if (nrow(tmp) == 0) return(cefr_df[0, ])

  tmp %>%
    mutate(nivel_cefr = stringr::str_to_upper(categoria_global),
           nivel_cefr = factor(nivel_cefr, levels = levels(cefr_df$nivel_cefr))) %>%
    filter(!is.na(nivel_cefr)) %>%
    group_by(cohorte, facultad, programa, nivel_cefr) %>%
    summarise(n = dplyr::n(), .groups = "drop_last") %>%
    group_by(cohorte, facultad, programa) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
}

summarise_desglose <- function(dat) {
  comp_cols <- grep("^comp_", names(dat), value = TRUE)
  if (!length(comp_cols)) return(desglose_df[0, ])

  long_df <- dat %>%
    select(cohorte, facultad, programa, all_of(comp_cols)) %>%
    tidyr::pivot_longer(cols = all_of(comp_cols), names_to = "etiqueta", values_to = "valor", values_drop_na = TRUE)

  if (nrow(long_df) == 0) return(desglose_df[0, ])

  long_df %>%
    group_by(cohorte, facultad, programa, etiqueta) %>%
    summarise(
      n = dplyr::n(),
      media = mean(as.numeric(valor), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prueba = factor("Lectura", levels = levels(desglose_df$prueba)),
      tipo = factor("Indicador", levels = levels(desglose_df$tipo)),
      pct = NA_real_
    ) %>%
    select(cohorte, facultad, programa, prueba, tipo, etiqueta, n, pct, media)
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

# 3) Pipeline de lectura utilizando el parser robusto
parse_one_file <- function(path) {
  meta <- guess_metadata_from_filename(path)
  message(glue::glue(">> Procesando {meta$source_file}"))

  dat <- parse_resultados_individuales_v2(path)
  if (nrow(dat) == 0) {
    warning(glue::glue("Archivo {meta$source_file} no produjo filas tras el parseo."))
  }

  dat <- dat %>%
    mutate(
      cohorte = meta$cohorte,
      facultad = meta$facultad,
      programa = meta$programa,
      source_file = meta$source_file
    ) %>%
    relocate(source_file, cohorte, facultad, programa, .before = 1)

  list(
    programas = summarise_programas(dat),
    niveles   = summarise_niveles(dat),
    cefr      = summarise_cefr(dat),
    desglose  = summarise_desglose(dat),
    individuales = dat
  )
}

parsed <- map(xlsx_files, parse_one_file)

# 4) Bind rows a objetos globales
PROGRAMAS <- if (length(parsed)) bind_rows(map(parsed, "programas")) else programas_df[0, ]
NIVELES   <- if (length(parsed)) bind_rows(map(parsed, "niveles")) else niveles_df[0, ]
CEFR      <- if (length(parsed)) bind_rows(map(parsed, "cefr")) else cefr_df[0, ]
DESGLOSE  <- if (length(parsed)) bind_rows(map(parsed, "desglose")) else desglose_df[0, ]
INDIVIDUALES <- if (length(parsed)) bind_rows(map(parsed, "individuales")) else tibble()

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
# readr::write_csv(INDIVIDUALES, "data/processed/individuales.csv")

message(">> Objetos disponibles: PROGRAMAS, NIVELES, CEFR, DESGLOSE, INDIVIDUALES")
