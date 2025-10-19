
suppressPackageStartupMessages({
  library(gt); library(dplyr); library(scales)
})

gt_summary_table <- function(df, title = NULL, subtitle = NULL) {
  gt(df) |>
    fmt_percent(columns = where(is.numeric), rows = everything(),
                decimals = 1, pattern = "{x}") |>
    tab_header(title = title, subtitle = subtitle) |>
    tab_options(table.font.size = px(12))
}
