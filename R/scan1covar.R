#' Scan1 for multiple traits with possibly different covariates, and some helper routines.
#' 
#' Modified from qtl2mediate version for newer data organization.
#' 
#' @param phe_mx matrix of phenotypes
#' @param cov_df data frame of covariates
#' @param probs_obj object with genotype probabilities
#' @param kinship kinship matrix or list of kinship matrices
#' @param analyses_df data frame of analyses information
#' @param ... additional arguments passed on
#' 
#' @return object of class \code{\link[qtl2]{scan1}}.
#' 
#' @importFrom qtl2 scan1
#' @importFrom dplyr select
#' @importFrom stats as.formula formula model.matrix
#' @importFrom rlang .data
#' 
#' @export
#'
scan1covar <- function(phe_mx, cov_df, probs_obj, kinship, analyses_df, ...) {
  # This is set up for different types of models (e.g. "binary"),
  # but qtl2::scan1 ignores "binary" this right now if kinship is provided.
  # So below, force kinship to NULL if any "binary", 
  # and force attr "hsq" to null as well.
  models <- analyses_df$model
  
  # ** Covariate changing to using covar and the addcovar and intcovar
  # ** columns from peaks (or provided herein).
  # ** model.matrix(formula(addcovar), covar) should work.
  
  analyses_df <- which_covar(analyses_df)
  ## Collapse to unique identifier for each row = each phenotype.
  covarset <- apply(analyses_df, 1, function(x) paste(1 * x, collapse = ""))
  ucov <- unique(covarset)
  
  wh <- which(covarset == ucov[1])
  scans <- scanfn(probs_obj, phe_mx, kinship, cov_df, analyses_df, wh, models, ...)
  attr(scans, "hsq") <- NULL
  if(length(ucov) > 1) for(i in ucov[-1]) {
    wh <- which(covarset == i)
    tmp <- scanfn(probs_obj, phe_mx, kinship, cov_df, analyses_df, wh, models, ...)
    attr(tmp, "hsq") <- NULL
    scans <- cbind(scans, tmp)
  }
  # reorder by decreasing max lod
  modify_object(scans, scans[,order(-apply(scans,2,max)), drop=FALSE])
}
#' @param addcovar Data frame of additive covariate.
#' @rdname scan1covar
#' @export
covar_matrix <- function(addcovar) {
  if(is.null(addcovar))
    return(NULL)
  if(is.data.frame(addcovar)) {
    f <- stats::formula(paste("~", paste(names(addcovar), collapse = "+")))
    addcovar <- stats::model.matrix(f, addcovar)[,-1, drop = FALSE]
  }
  wh_sex(addcovar)
}

#' @param wh Index for which row of \code{analyses_df} to use.
#' @importFrom stats formula model.matrix
#' @rdname scan1covar
#' @export
which_covar <- function(analyses_df, wh, cov_df) {
  if(missing(wh)) {
    ## Covariate indicators follow winsorize column.
    wh <- which("winsorize" == names(analyses_df))
    is_covar <- apply(analyses_df[, -(seq_len(wh)), drop=FALSE], 2, any)
    ## Keep only covariate indicators with at least one TRUE value.
    analyses_df[, names(is_covar)[is_covar], drop=FALSE]
  } else {
    # Get which covariates from condensed analyses table.
    # The analyses table as T/F with names; capture names that have TRUE.
    covars <- unlist(analyses_df[wh[1],, drop = FALSE])
    covars <- names(covars)[covars]
    data.frame(cov_df[, covars, drop=FALSE],
               stringsAsFactors = FALSE)
  }
}

#' @param sex_type subset by sex if \code{"F"} or \code{"M"}.
#' @rdname scan1covar
#' @export
sexcovar <- function(addcovar, sex_type = "all") {
  sexChar <- toupper(stringr::str_sub(addcovar$sex, 1, 1))
  if(!all(sort(unique(sexChar)) %in% c("M","F")) & sex_type %in% c("M","F")) {
    stop("cannot handle levels of sex not M and F")
  }
  switch(sex_type,
         "F" = addcovar <- addcovar[sexChar == "F",, drop = FALSE],
         "M" = addcovar <- addcovar[sexChar == "M",, drop = FALSE])
  if(sex_type %in% c("F","M")) {
    addcovar <- dplyr::select(addcovar, -.data$sex)
    if(ncol(addcovar) == 0)
      addcovar <- NULL
  }
  addcovar
}

scanfn <- function(probs_obj, phe_mx, kinship, cov_df, analyses_df, wh, models,
                   sex_type = c("A","I","F","M","all"), ...) {
  
  sex_type <- match.arg(sex_type)
  if(sex_type == "all" & ncol(phe_mx) > 1)
    sex_type <- "A"
  
  # scan1 for wh phenotypes using their covariates.
  phe_mx <- phe_mx[, wh, drop=FALSE]
  cov_df <- which_covar(analyses_df, wh, cov_df)
  
  models <- models[wh]
  if(all(models == models[1])) {
    kinship <- if(models[1] == "binary") NULL else kinship
    if(ncol(cov_df)) {
      scansex(probs_obj, phe_mx, kinship, cov_df,
              models[1], sex_type)
    } else { # no covariates (unlikely)
      qtl2::scan1(probs_obj, phe_mx, kinship,
                  model = models[1])
    }
  } else { # multiple models
    umod <- unique(models)
    whm <- which(models == umod[1])
    kinship <- if(umod[1] == "binary") NULL else kinship
    if(ncol(cov_df)) {
      out <- scansex(probs_obj, phe_mx[, whm, drop=FALSE], 
                     kinship, cov_df,
                     umod[1], sex_type)
      attr(out, "hsq") <- NULL
      for(mod in umod[-1]) {
        whm <- which(models == mod)
        kinship <- if(mod == "binary") NULL else kinship
        tmp <- scansex(probs_obj, phe_mx[, whm, drop=FALSE], 
                       kinship, cov_df, mod, sex_type)
        attr(tmp, "hsq") <- NULL
        out <- cbind(out, tmp)
      }
    } else { # no covariates (unlikely)
      kinship <- if(umod[1] == "binary") NULL else kinship
      out <- qtl2::scan1(probs_obj, 
                         phe_mx[, whm, drop=FALSE], 
                         kinship, 
                         model = umod[1])
      attr(out, "hsq") <- NULL
      for(mod in umod[-1]) {
        whm <- which(models == mod)
        kinship <- if(mod == "binary") NULL else kinship
        tmp <- qtl2::scan1(probs_obj, 
                           phe_mx[, whm, drop=FALSE], 
                           kinship, 
                           model = mod)
        attr(tmp, "hsq") <- NULL
        out <- cbind(out, tmp)
      }
    }
    out
  }
}

scansex <- function(genoprobs, pheno, kinship, addcovar = NULL,
                    model, sex_type) {
  intcovar <- NULL
  if(!is.null(addcovar)) {
    if(sex_type == "all") {
      out <- scansex(genoprobs, pheno, kinship, addcovar, model, "A")
      out <- cbind(out,
                   scansex(genoprobs, pheno, kinship, addcovar, model, "I"))
      out <- cbind(out,
                   scansex(genoprobs, pheno, kinship, addcovar, model, "F"))
      out <- cbind(out,
                   scansex(genoprobs, pheno, kinship, addcovar, model, "M"))
      colnames(out) <- c("AddSex","IntSex","Female","Male")
      return(out)
    } else {
      intcovar <- NULL
      addcovar <- wh_sex(addcovar)
      if("sex" %in% colnames(addcovar)) {
        switch(sex_type,
               "I" = {
                 intcovar <- stats::model.matrix(~ sex, addcovar)[, -1, drop = FALSE]
               },
               "F","M" = {
                 addcovar <- sexcovar(addcovar, sex_type)
               })
      }
    }
    addcovar <- covar_matrix(addcovar)
  }
  qtl2::scan1(genoprobs, pheno, kinship, addcovar, 
              intcovar = intcovar,
              model = model)
}

wh_sex <- function(addcovar) {
  # Figure out which column is sex and make sure its name is "sex" 
  m <- match("sexm", tolower(colnames(addcovar)))
  if(is.na(m))
    m <- match("sex", tolower(colnames(addcovar)))
  if(!is.na(m))
    colnames(addcovar)[m] <- "sex"
  
  addcovar
}