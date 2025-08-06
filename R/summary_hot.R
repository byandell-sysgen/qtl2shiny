#' @importFrom dplyr rename select
#' @importFrom rlang .data
#' @importFrom qtl2ggplot summary_scan1
#' 
summary_hot <- function(hotspot_obj, class = 1) {
  # Used chosen datasets, or all if not chosen.

  map <- hotspot_obj$map
  scan <- hotspot_obj$scan
  
  # Match lod columns to those present.
  lodcol <- colnames(scan)[class]

  if(length(lodcol) & (nrow(scan) == length(unlist(map)))) {
    chr_id <- names(map)
    dplyr::select(
      dplyr::rename(
        qtl2ggplot::summary_scan1(
          subset(scan, lodcolumn = lodcol),
          map, chr = chr_id),
        count = .data$lod),
      -.data$marker)
  } else {
    NULL
  }
}