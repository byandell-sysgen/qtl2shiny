#' QTL2 Shiny UI Theme
#'
#' @param font_size base font size in rem (default "0.85rem")
#' @return a \code{bslib::bs_theme} object
#' @export
qtl2shiny_theme <- function(font_size = "0.85rem") {
  bslib::bs_theme(
    version = 5,
    "font-size-base" = font_size
  )
}

#' QTL2 Shiny Plot Theme
#'
#' @param base_size base font size (default 9)
#' @return a ggplot2 theme
#' @export
qtl2shiny_plot_theme <- function(base_size = 9) {
  ggplot2::theme_minimal(base_size = base_size)
}
