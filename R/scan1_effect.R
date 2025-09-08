scan1_effect <- function(genoprobs, pheno_mx, kinship_list, covar_df,
                   peaks_df, blups) {
  covars <- covar_matrix_phenos(pheno_mx, covar_df, peaks_df)
  qtl2ggplot::listof_scan1coef(genoprobs, pheno_mx, kinship_list,
                               covars$addcovar, blups)
}