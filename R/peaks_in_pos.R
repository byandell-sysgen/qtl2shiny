#' @importFrom dplyr arrange desc distinct filter mutate select
#' @importFrom rlang .data
#' 
peaks_in_pos <- function(peak_df, use_pos = TRUE,
                         chr_id=NULL, pos_Mbp=NULL, win=NULL) {
  if(use_pos) {
    if(!(is.null(chr_id) | is.null(pos_Mbp) | is.null(win))) {
      if(win > 0) {
        ## Filter peak_df. Allow for more than one hotspot.
        out <- list()
        for(i in seq_along(chr_id)) {
          out[[i]] <- dplyr::filter(peak_df, 
                                    .data$qtl_chr == chr_id[i],
                                    .data$qtl_pos >= pos_Mbp[i] - win,
                                    .data$qtl_pos <= pos_Mbp[i] + win)
        }
        peak_df <- dplyr::bind_rows(out)
      }
    }
  }
  if(!nrow(peak_df)) return(NULL)
  
  # Arrange by descending `qtl_lod`
  dplyr::mutate(
    dplyr::distinct(
      dplyr::arrange(
        peak_df,
        dplyr::desc(.data$qtl_lod)),
      .data$phenotype, .data$phenotype_original, .data$phenotype_class,
      .data$qtl_lod, .data$qtl_chr, .data$qtl_pos,
      .data$addcovar, .data$intcovar, .data$subject, .data$model),
    qtl_lod = round(.data$qtl_lod, 1),
    qtl_pos = round(.data$qtl_pos, 2))
}
