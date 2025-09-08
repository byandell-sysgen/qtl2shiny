#' Scan1 for traits with same covariates.
#' 
#' Modified from qtl2mediate version for newer data organization.
#' Could build out a companion routine splitting on covariates.
#' 
#' @param pheno_mx matrix of phenotypes
#' @param covar_df data frame of covariates
#' @param genoprobs object with genotype probabilities
#' @param kinship kinship matrix or list of kinship matrices
#' @param peaks_df data frame of peaks information
#' @param model model argument for \code{\link[qtl2]{scan1}}
#' @param ... additional arguments passed on
#' @param force force pheno names to be in peaks
#' 
#' @return object of class \code{\link[qtl2]{scan1}}.
#' 
#' @importFrom qtl2 get_common_ids scan1
#' @export
scan1covar <- function(pheno_mx, covar_df, genoprobs, kinship, peaks_df, 
                       model = "normal", ..., force = FALSE) {

  covars <- covar_matrix_phenos(pheno_mx, covar_df, peaks_df)
  if(force)
    pheno_mx <- pheno_mx[, covars$phenos, drop = FALSE]
  
  kinship <- if(model == "binary") NULL else kinship
  scans <- qtl2::scan1(genoprobs, pheno_mx, kinship,
    covars$addcovar, intcovar = covars$intcovar, model = model, ...)
  attr(scans, "hsq") <- NULL
  # reorder by decreasing max lod
  modify_object(scans, scans[,order(-apply(scans,2,max)), drop=FALSE])
}
covar_matrix_phenos <- function(pheno_mx, covar_df, peaks_df) {
  # Match phenotypes between data and peaks.
  phenos <- match(colnames(pheno_mx), peaks_df$phenotype, nomatch = 0)
  if(any(phenos == 0)) {
    message("phenotypes not in peaks: ",
            paste(colnames(pheno_mx)[phenos == 0], collapse = ", "))
  }
  if(all(phenos == 0)) return(NULL)
  
  # Check for multiple addcovars or intcovars.
  peaks_df <- peaks_df[phenos,]
  list(
    phenos = phenos,
    addcovar = covar_model_matrix(peaks_df$addcovar, covar_df),
    intcovar = covar_model_matrix(peaks_df$intcovar, covar_df))
}
covar_model_matrix <- function(covform, covar_df) {
  uform <- unique(covform)
  uform <- uform[uform != "none"]
  if(!length(uform)) return(NULL)
  
  # Combine all formula into one
  uform <- paste0("~",
    paste(stringr::str_remove(uform, "^~"),
          collapse = "+"))
  stats::model.matrix(formula(uform), covar_df)[,-1, drop = FALSE]
}
covar_model_matrix_save <- function(covform, covar_df) {
  uform <- unique(covform)
  out <- list()
  for(i in uform) {
    out[[i]] <- if(i == "none") {
      NULL
    } else {
      stats::model.matrix(formula(i), covar_df)[,-1, drop = FALSE]
    }
  }
  out
}
