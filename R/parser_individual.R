# =============================================================================
# PARSER "Resultados Individuales" (nivel alumno) — v4.1 robusto
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(readxl, stringr, janitor, readr, tibble, dplyr, purrr)

# --------------------------- Utilidades base R ---------------------------

# Detecta la primera fila de datos asumiendo IDs numéricos (RUT) en 1a columna
guess_data_start_base <- function(df0){
  v <- suppressWarnings(readr::parse_double(df0[[1]]))
  idx <- which(!is.na(v) & v > 1e6)  # ajusta/quita >1e6 si no son RUT numéricos
  if (length(idx)) min(idx) else NA_integer_
}

# Última fila de “encabezado” previa al inicio de datos (≥2 celdas no vacías)
guess_header_row_base <- function(df0, data_start){
  if (is.na(data_start) || data_start <= 1) return(NA_integer_)
  prev_df <- as.data.frame(df0[seq_len(data_start - 1), , drop = FALSE], stringsAsFactors = FALSE)
  nonempty <- rowSums(!is.na(prev_df) & prev_df != "")
  idx <- which(nonempty >= 2)
  if (length(idx)) max(idx) else NA_integer_
}

# Construye nombres colapsando TODAS las filas del bloque de cabeceras
make_names_from_block_base <- function(df0, header_row, data_start){
  if (is.na(header_row)) {
    raw_names <- names(df0)
  } else {
    block <- as.data.frame(df0[seq(from = header_row, to = data_start - 1), , drop = FALSE],
                           stringsAsFactors = FALSE)
    collapse_col <- function(x){
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

# Devuelve el primer candidato de columna que exista
first_existing_col_base <- function(df, candidates){
  keep <- candidates[candidates %in% names(df)]
  if (length(keep)) keep[1] else NA_character_
}

# --------------------------- Estandarizaciones ---------------------------

# Renombra campos “core” por patrones (rut, dv, apellidos, nombres, etc.)
standardize_core_names <- function(dat){
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
  
  rename_map <- list()
  for (k in names(pats)){
    hit <- stringr::str_which(nms, stringr::regex(pats[[k]], ignore_case = TRUE))
    if (length(hit)) {
      old <- nms[hit[1]]
      new <- switch(k,
                    rut="rut", dv="dv", primer="primer_apellido", segundo="segundo_apellido",
                    nombres="nombres", estado_hab="estado_habilitacion", estatus="estatus_rendicion",
                    fecha="fecha_rendicion", justif="justificacion_no_rendicion"
      )
      rename_map[[new]] <- old
    }
  }
  if (length(rename_map)) dat <- dplyr::rename(dat, !!!rename_map)
  dat
}

# Fallback posicional (por si los patrones no aparecen)
fallback_positionals <- function(dat){
  pos_map <- c(
    "rut","dv","primer_apellido","segundo_apellido","nombres",
    "estado_habilitacion","estatus_rendicion","fecha_rendicion","justificacion_no_rendicion"
  )
  for (i in seq_along(pos_map)){
    tgt <- pos_map[i]
    if (!tgt %in% names(dat) && i <= ncol(dat)) {
      names(dat)[i] <- tgt
    }
  }
  dat
}

# Detecta si una columna “parece nota de competencia” (0–3 o “NR”)
looks_like_comp_score <- function(x){
  x2 <- if (is.factor(x)) as.character(x) else x
  x2 <- x2[!is.na(x2) & x2 != ""]
  if (!length(x2)) return(FALSE)
  ok <- stringr::str_detect(x2, stringr::regex("^(nr|n\\s*r|[0-3])$", ignore_case = TRUE)) |
    stringr::str_detect(x2, stringr::regex("^\\d{1,2}$", ignore_case = TRUE))
  mean(ok) >= 0.7
}

# Renombra competencias y totales (acepta prefijo 'x' y variantes) + inferencia posicional
standardize_competencias <- function(dat){
  nms <- names(dat)
  
  # 1) Mapeo por patrones en encabezados
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
  
  rename_map <- list()
  for (k in names(ren)){
    hit <- stringr::str_which(nms, stringr::regex(ren[[k]], ignore_case = TRUE))
    if (length(hit)) {
      old <- nms[hit[1]]
      rename_map[[k]] <- old      # new = old
    }
  }
  if (length(rename_map)) dat <- dplyr::rename(dat, !!!rename_map)
  
  # 2) Si aún faltan, usar anclas e inferencia por posición/contenido
  nm <- names(dat)
  
  # (a) Mapear explícitamente si existen columnas que empiecen con x3_2_a/b/c
  if (!"comp_32a" %in% nm){
    hit <- which(stringr::str_detect(nm, stringr::regex("^x3_2_a", ignore_case = TRUE)))
    if (length(hit)) nm[hit[1]] <- "comp_32a"
  }
  if (!"comp_32b" %in% nm){
    hit <- which(stringr::str_detect(nm, stringr::regex("^x3_2_b", ignore_case = TRUE)))
    if (length(hit)) nm[hit[1]] <- "comp_32b"
  }
  if (!"comp_32c" %in% nm){
    hit <- which(stringr::str_detect(nm, stringr::regex("^x3_2_c", ignore_case = TRUE)))
    if (length(hit)) nm[hit[1]] <- "comp_32c"
  }
  names(dat) <- nm
  nm <- names(dat)  # refresh
  
  # Índices de ancla
  idx_just <- which(nm == "justificacion_no_rendicion")
  idx_32a  <- which(nm == "comp_32a")
  idx_32c  <- which(nm == "comp_32c")
  
  # (b) Inferir 1.1 y 1.2: buscar entre justificación y 3.2a
  if (length(idx_just) && length(idx_32a)){
    rng <- seq(from = idx_just + 1, to = idx_32a - 1)
    rng <- rng[rng >= 1 & rng <= ncol(dat)]
    if (length(rng)){
      cand <- nm[rng]
      mask <- vapply(dat[cand], looks_like_comp_score, logical(1))
      cand2 <- cand[mask]
      if (length(cand2) >= 2){
        nm[match(tail(cand2, 2), nm)] <- c("comp_11", "comp_12")
      } else if (length(cand) >= 2){
        nm[match(tail(cand, 2), nm)] <- c("comp_11", "comp_12")
      }
      names(dat) <- nm; nm <- names(dat)
    }
  }
  
  # (c) Inferir 4.1, 4.2 y totales: buscar a partir de 3.2c
  if (length(idx_32c)){
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

# --------------------------- Fix de nombres por fila ---------------------------

# Si 'primer_apellido' contiene "NOMBRES + APELLIDO1 + APELLIDO2", lo repara fila a fila
repair_person_names <- function(df){
  required <- c("primer_apellido","segundo_apellido","nombres")
  if (!all(required %in% names(df))) return(df)
  
  split_full <- function(txt){
    if (is.na(txt) || txt == "") return(list(nombres=NA_character_, ap1=NA_character_, ap2=NA_character_))
    toks <- str_squish(txt) |> str_split("\\s+") |> unlist()
    toks <- toks[toks != "" & !is.na(toks)]
    if (length(toks) < 3) return(list(nombres=NA_character_, ap1=NA_character_, ap2=NA_character_))
    ap2 <- toks[length(toks)]
    ap1 <- toks[length(toks)-1]
    nom <- if (length(toks) > 2) paste(toks[1:(length(toks)-2)], collapse = " ") else NA_character_
    list(nombres=nom, ap1=ap1, ap2=ap2)
  }
  
  need_fix <- vapply(df$primer_apellido, function(x){
    if (is.na(x) || x == "") FALSE else str_count(x, "\\S+") >= 3
  }, logical(1))
  
  if (any(need_fix)){
    fixed <- lapply(df$primer_apellido[need_fix], split_full)
    df$nombres[need_fix]          <- vapply(fixed, `[[`, character(1), "nombres")
    df$primer_apellido[need_fix]  <- vapply(fixed, `[[`, character(1), "ap1")
    df$segundo_apellido[need_fix] <- vapply(fixed, `[[`, character(1), "ap2")
  }
  df
}

# --------------------------- Parser principal v4.1 ---------------------------

#' @title Parse de Resultados Individuales (EPI)
#' @param path Ruta al archivo .xlsx
#' @param sheet Hoja a leer (auto-detección por defecto)
#' @param compute_total Si TRUE, calcula puntaje_total si no viene
#' @return tibble con columnas estandarizadas
parse_resultados_individuales_v2 <- function(path, sheet = NULL, compute_total = TRUE){
  stopifnot(file.exists(path))
  
  # 1) Selección de hoja
  if (is.null(sheet)){
    shs <- readxl::excel_sheets(path)
    s_low <- stringr::str_to_lower(shs)
    cand <- shs[stringr::str_detect(s_low, "resultado") & stringr::str_detect(s_low, "individual")]
    if (!length(cand)) cand <- shs[stringr::str_detect(s_low, "individ")]
    if (!length(cand)) cand <- shs
    sheet <- cand[1]
    message("Usando hoja: ", sheet)
  }
  
  # 2) Leer crudo sin nombres y todo como texto
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")
  raw <- janitor::remove_empty(raw, which = c("rows","cols"))
  
  # 3) Detectores base-R
  data_start <- guess_data_start_base(raw)
  if (is.na(data_start)) stop("No pude detectar el inicio de datos (ID numérico) en la 1ª columna.")
  hdr_row <- guess_header_row_base(raw, data_start)
  
  # 4) Reconstruir nombres con bloque de cabeceras
  col_nms <- make_names_from_block_base(raw, hdr_row, data_start)
  
  # 5) Cortar datos y aplicar nombres
  dat <- tibble::as_tibble(raw[seq(from = data_start, to = nrow(raw)), , drop = FALSE])
  names(dat) <- col_nms
  
  # 6) Limpieza básica
  dat <- dat %>% mutate(across(everything(), ~ na_if(.x, "")))
  
  # 7) Estándar de campos núcleo + fallback posicional
  dat <- standardize_core_names(dat) %>% fallback_positionals()
  
  # 8) id_estudiante (mejor candidato; si no, 1ª col)
  id_candidates <- c("datos_de_estudiantes_de_la_cohorte", "rut", "id_estudiante", "id", names(dat)[1])
  id_col <- first_existing_col_base(dat, id_candidates)
  dat <- dat %>% mutate(id_estudiante = as.character(.data[[id_col]]))
  
  # 9) Reparar nombres/apellidos cuando vengan concatenados en 'primer_apellido'
  dat <- repair_person_names(dat)
  
  # 10) nombre_completo
  has_3 <- all(c("nombres","primer_apellido","segundo_apellido") %in% names(dat))
  dat <- dat %>% mutate(
    nombre_completo = if (has_3) stringr::str_squish(paste(nombres, primer_apellido, segundo_apellido))
    else NA_character_
  )
  
  # 11) rindio seguro (usa vectores NA del tamaño de n si faltan columnas)
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
  
  # 12) Competencias y totales (con inferencia)
  dat <- standardize_competencias(dat)
  
  comp_guess <- c("comp_11","comp_12","comp_32a","comp_32b","comp_32c","comp_41","comp_42")
  comp_cols <- intersect(names(dat), comp_guess)
  if (length(comp_cols)){
    dat <- dat %>% mutate(across(all_of(comp_cols),
                                 ~ suppressWarnings(readr::parse_number(as.character(.x)))))
  }
  
  # 13) puntaje_total numérico (si existe) o calcularlo
  if ("puntaje_total" %in% names(dat)){
    dat <- dat %>% mutate(puntaje_total = suppressWarnings(readr::parse_number(as.character(puntaje_total))))
  } else if (compute_total && length(comp_cols)){
    dat <- dat %>% mutate(puntaje_total = rowSums(across(all_of(comp_cols)), na.rm = TRUE))
  }
  
  # 14) porcentaje_logro a numérico si existe
  if ("porcentaje_logro" %in% names(dat)){
    dat <- dat %>% mutate(porcentaje_logro = readr::parse_number(as.character(porcentaje_logro)))
  }
  
  dat
}

# --------------------------- Helper opcional ---------------------------

# Envoltura mínima: lee y retorna tibble ya estandarizado
read_individual_results <- function(path, sheet = NULL, compute_total = TRUE){
  parse_resultados_individuales_v2(path = path, sheet = sheet, compute_total = compute_total)
}

# --------------------------- Ejemplo (comentar en Quarto) ---------------------------
# psico <- read_individual_results("/home/rober/Documentos/epi-cchh-reports/data/raw/BD REPORTE EPI PSICOLOGÍA.xlsx")
# glimpse(psico)
# readr::write_csv(psico, "psicologia_individual_limpio.csv")
