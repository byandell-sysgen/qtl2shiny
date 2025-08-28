decode_hotspot <- function(hotspot) {
  # See summary.hotspot in hotspot.R for construction.
  # paste0(.data$chr, ":", .data$pos, " (", .data$count, ")"))
  data.frame(
    chr = stringr::str_remove(hotspot, ":.*$"),
    pos = as.numeric(
      stringr::str_remove(
        stringr::str_remove(hotspot, "^.*:"),
        "\\(.*$")),
    count = as.numeric(
      stringr::str_remove(
        stringr::str_remove(hotspot, "^.*\\("),
        "\\)$"))
    )
}