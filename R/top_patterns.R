top_patterns <- function(top_snps_tbl, snp_action = "basic") {
  dplyr::arrange(
    dplyr::mutate(
      dplyr::filter(
        summary(top_snps_tbl), 
        .data$max_lod >= 3), 
      contrast = snp_action), 
    dplyr::desc(.data$max_lod))
}