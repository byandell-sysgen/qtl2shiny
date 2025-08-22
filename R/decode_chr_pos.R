decode_chr_pos <- function(chr_pos) {
  # See summary.hotspot in hotspot.R for construction.
  # paste0(.data$chr, ":", .data$pos, " (", .data$count, ")"))
  data.frame(
    chr = stringr::str_remove(chr_pos, ":.*$"),
    pos = as.numeric(
      stringr::str_remove(
        stringr::str_remove(chr_pos, "^.*:"),
        "\\(.*$")))
}