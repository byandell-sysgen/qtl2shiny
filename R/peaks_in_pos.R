#' @importFrom dplyr arrange desc distinct filter mutate select
#' @importFrom rlang .data
#' 
peaks_in_pos <- function(peaks_df, use_pos = TRUE,
                         chr_id=NULL, pos_Mbp=NULL, win=NULL) {
  if(use_pos) {
    if(!(is.null(chr_id) | is.null(pos_Mbp) | is.null(win))) {
      if(win > 0) {
        ## Filter peaks_df
        peaks_df <- dplyr::filter(peaks_df, 
                                  .data$qtl_chr == chr_id,
                                  .data$qtl_pos >= pos_Mbp - win,
                                  .data$qtl_pos <= pos_Mbp + win)
      }
    }
  }
  dplyr::mutate(
    dplyr::select(
      dplyr::distinct(
        dplyr::arrange(
          peaks_df,
          dplyr::desc(.data$qtl_lod)),
        .data$phenotype, .data$phenotype_class,
        .data$qtl_lod, .data$qtl_chr, .data$qtl_pos,
        .data$addcovar, .data$intcovar),
      phenotype, phenotype_class, qtl_lod, qtl_chr, qtl_pos,
      .data$addcovar, .data$intcovar),
    qtl_lod = round(.data$qtl_lod, 1),
    qtl_pos = round(.data$qtl_pos, 2))
}
