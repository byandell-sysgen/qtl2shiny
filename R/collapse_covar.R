# Collapse covariate names in analyses table
#
# Collapse covariates used to comma-separated list and return table
#' @importFrom rlang .data
#' 
collapse_covar <- function(analyses_tbl) {
  if(is.null(analyses_tbl))
    return(NULL)
  
  ## Collapse covariates (past winsorize column).
  covar_names <- names(analyses_tbl)[-seq_len(match("winsorize",
                                                    names(analyses_tbl)))]
  dplyr::select(
    dplyr::mutate(
      tidyr::unite(analyses_tbl, "covar", 
                   dplyr::one_of(covar_names)), 
      covar = sapply(strsplit(.data$covar,"_"),
                     function(x) paste(covar_names[as.logical(x)],
                                       collapse=","))),
    .data$pheno, .data$covar, .data$transf, .data$offset, .data$winsorize)
}
