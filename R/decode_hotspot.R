decode_hotspot <- function(hotspot_code, hotspot_df) {
  list(
    chr_id = stringr::str_remove(hotspot_code, ":.*$"),
    peak_Mbp = as.numeric(
      stringr::str_remove(
        stringr::str_remove(hotspot_code, "^.*:"),
        "\\(.*$")),
    count_hotspot = as.numeric(
      stringr::str_remove(
        stringr::str_remove(hotspot_code, "^.*\\("),
        "\\)$")),
    window_Mbp = attr(hotspot_df, "window")
    )
}
encode_hotspot <- function(chr, pos, count) {
  paste0(chr, ":", pos, " (", count, ")")
}