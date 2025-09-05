peak_probs_filter <- function(pheno_name, peak_df, pairprobs_obj) {
  chr_id <- names(pairprobs_obj$map)[1]
  scan_window <- range(pairprobs_obj$map[[1]])

  # Make sure `peak_df` covers `map` scan window.
  peak_df <- dplyr::filter(
    peak_df,
    .data$phenotype %in% pheno_name,
    .data$qtl_chr %in% chr_id,
    .data$qtl_pos >= scan_window[1],
    .data$qtl_pos <= scan_window[2])
  if(!nrow(peak_df)) return(NULL)
  peak_df
}