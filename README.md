
# Reportes CCHH — Cohorte 2025 (Quarto)

Este repositorio genera reportes por Programa/Facultad siguiendo la estructura de los informes de referencia.
Requiere **R (≥ 4.2)**, **Quarto (≥ 1.4)** y los paquetes listados abajo.

## Estructura
```
epi-cchh-reports/
├── _quarto.yml
├── README.md
├── .gitignore
├── data/
│   ├── raw/                 # Excel originales (solo lectura)
│   └── processed/           # CSV/Parquet limpios (generados)
├── R/
│   ├── 00_utils.R
│   ├── 01_load_data.R
│   ├── 02_derive_indicators.R
│   ├── 03_figures.R
│   ├── 04_tables.R
│   └── 99_render_all.R
├── styles/
│   └── report.scss
├── templates/
│   └── report_template.qmd
└── output/2025/{pdf,html}/   # resultados
```

## Paquetes R
- tidyverse, readxl, janitor, glue, here, scales, forcats
- gt, flextable (opcional)
- patchwork (opcional)
- arrow (opcional, si usa Parquet)
- quarto

> Sugerido: administrar versiones con `{renv}`.

## Uso rápido
1. Copia los Excel a `data/raw/`.
2. Abre R en la carpeta del proyecto:
   ```r
   source("R/01_load_data.R")         # genera objetos estandarizados
   source("R/02_derive_indicators.R") # deriva tablas/figuras
   source("R/99_render_all.R")        # renderiza todos los programas
   ```
3. Los PDFs/HTML quedan en `output/2025/`.

## Parametrización
Cada reporte utiliza `templates/report_template.qmd` con `params:` (cohorte, facultad, programa, fecha_corte).
El script `R/99_render_all.R` itera por todas las combinaciones detectadas en `data/processed/`.

## Notas
- Se enmascaran conteos muy pequeños (e.g., `< 5`). 
- Cuando n<10, se inserta una nota de cautela para las distribuciones.
