mediate_signif <- function(mediate_obj, signif = 0.1) {
  if(is.null(mediate_obj)) return(NULL)
  
  out <- mediate_obj
  out$best <- 
    # Rename `qtl_pos` as `pos` for `intermediate::ggplot_mediation_test`.
    dplyr::rename(
      # Filter out n.s. entries.
      dplyr::filter(out$best, .data$pvalue <= signif),
      pos = "qtl_pos")
  class(out) <- class(mediate_obj)
  out
}
