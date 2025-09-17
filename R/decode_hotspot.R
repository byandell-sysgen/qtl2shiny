decode_hotspot <- function(hotspot_code, hotspot_df, collapse = TRUE) {
  out <- list(
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
  if(collapse & length(hotspot_code) > 1) {
    # Collapse to first `chr_id`
    chr_id <- out$chr_id[1]
    uchr <- out$chr_id == chr_id
    # Expand window to accommodate multiple peaks.
    out$window_Mbp <- (diff(range(out$peak_Mbp[uchr])) / 2) + out$window_Mbp
    out$peak_Mbp <- mean(out$peak_Mbp[uchr])
    out$count_hotspot <- sum(out$count_hotspot[uchr])
    out$chr_id <- chr_id
  }
  out
}
encode_hotspot <- function(chr, pos, count) {
  paste0(chr, ":", pos, " (", count, ")")
}