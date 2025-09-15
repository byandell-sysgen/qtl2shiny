allele_scan <- function(pheno_mx, covar_df, pairprobs_obj, K_chr,
                    peak_df, patterns, scan_pat, blups) {
  peak_df <- peak_probs_filter(colnames(pheno_mx), peak_df, pairprobs_obj)
  if(is.null(peak_df)) return(NULL)
  
  addcovar <- covar_model_matrix(peak_df$addcovar, covar_df)
  #** Need to add intcovar to qtl2pattern::allele1
  intcovar <- covar_model_matrix(peak_df$intcovar, covar_df)

  qtl2pattern::allele1(pairprobs_obj$probs, pheno_mx, addcovar, 
                       pairprobs_obj$map, K_chr,
                       patterns = patterns,
                       scan_pat = scan_pat,
                       blups = blups)
}
allele_summary <- function(object, pos_Mbp) {
  positions <- unique(object$pos)
  pos_cen <- positions[which.min(abs(positions - pos_Mbp))]
  levels(object$source)[1:2] <- c("allele", "allele_pair")
  dplyr::arrange(
    dplyr::mutate(
      dplyr::rename(
        dplyr::select(
          dplyr::filter(object, pos == pos_cen),
          source, allele, effect),
        geno = "allele"),
      effect = signif(.data$effect, 3),
      source = factor(source,
        levels = c(levels(.data$source)[-2], levels(.data$source)[2]))),
    .data$source)
}
