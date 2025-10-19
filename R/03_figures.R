
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(scales)
})

# Paletas (desde 00_utils.R si se cargó previamente)
if (!exists("palette_levels")) palette_levels <- c("Bajo"="#c7e9b4","Medio"="#7fcdbb","Adecuado"="#1d91c0")
if (!exists("palette_cefr"))   palette_cefr   <- c("A1"="#f0f9e8","A2"="#bae4bc","A2+"="#7bccc4","B1"="#2b8cbe")

plot_niveles <- function(df_levels) {
  ggplot(df_levels, aes(x = prueba, y = pct, fill = nivel)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = palette_levels) +
    labs(x = NULL, y = "% de estudiantes", fill = "Nivel") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

plot_cefr <- function(df_cefr) {
  ggplot(df_cefr, aes(x = nivel_cefr, y = pct, fill = nivel_cefr)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = palette_cefr, guide = "none") +
    labs(x = NULL, y = "% de estudiantes") +
    theme_minimal(base_size = 11)
}

# Placeholder para promedio + distribución (según disponibilidad)
plot_promedio_placeholder <- function(promedio, titulo = "Promedio (placeholder)") {
  df <- tibble(x = 1, y = promedio)
  ggplot(df, aes(x, y)) +
    geom_point(size = 3) +
    scale_y_continuous(limits = c(0,100)) +
    labs(x = NULL, y = "Puntaje", title = titulo) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}
