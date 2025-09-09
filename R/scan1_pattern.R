#' @importFrom rlang .data
#' @importFrom dplyr filter mutate
#' 
pull_patterns <- function(patterns, pheno_names) {
  # if(all(pheno_names %in% patterns$pheno)) {
    dplyr::filter(patterns, .data$pheno %in% pheno_names)
  # }
  # else {
  #   out <- dplyr::filter(patterns, .data$pheno == "AddSex")
  #   if(nrow(out))
  #     out <- dplyr::mutate(out, pheno = pheno_names[1])
  #   out
  # }
}
scan1_pattern <- function(pheno_name, pheno_mx, covar_df, pairprobs_obj, K_chr,
                          peak_df, pats, blups) {
  peak_df <- peak_probs_filter(pheno_name, peak_df, pairprobs_obj)
  if(is.null(peak_df)) return(NULL)
  
  addcovar <- covar_model_matrix(peak_df$addcovar, covar_df)
  #** Need to add intcovar to qtl2pattern::scan1pattern.
  intcovar <- covar_model_matrix(peak_df$intcovar, covar_df)
  
  qtl2pattern::scan1pattern(pairprobs_obj$probs,
                            pheno_mx[, pheno_name, drop=FALSE],
                            K_chr, addcovar,
                            pairprobs_obj$map,
                            pats,
                            blups = blups)
}