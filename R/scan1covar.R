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
#' 
#' @return object of class \code{\link[qtl2]{scan1}}.
#' 
#' @importFrom qtl2 scan1
#' @export
scan1covar <- function(pheno_mx, covar_df, genoprobs, kinship, peaks_df, 
                       model = "normal", ...) {
  # Match phenotypes between data and peaks.
  phenos <- match(colnames(pheno_mx), peaks_df$phenotype)
  if(any(is.na(phenos))) {
    message("phenotypes not in peaks: ",
            paste(colnames(pheno_mx)[is.na(phenos)], collapse = ", "))
  }
  if(all(is.na(phenos))) return(NULL)
  pheno_mx <- pheno_mx[, !is.na(phenos), drop = FALSE]
  
  # Check for multiple addcovars or intcovars.
  addcovar <- covar_model_matrix(peaks_df$addcovar, covar_df)
  intcovar <- covar_model_matrix(peaks_df$intcovar, covar_df)
  if(length(addcovar) > 1 || length(intcovar) > 1) {
    message("not all covariates the same")
    return(NULL)
  }
  addcovar <- if(length(addcovar)) addcovar[[1]] else NULL
  intcovar <- if(length(intcovar)) intcovar[[1]] else NULL
  
  kinship <- if(model == "binary") NULL else kinship
  scans <- qtl2::scan1(genoprobs, pheno_mx, kinship,
    addcovar, intcovar = intcovar, model = model, ...)
  attr(scans, "hsq") <- NULL
  # reorder by decreasing max lod
  modify_object(scans, scans[,order(-apply(scans,2,max)), drop=FALSE])
}
covar_model_matrix <- function(covform, covar_df) {
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
