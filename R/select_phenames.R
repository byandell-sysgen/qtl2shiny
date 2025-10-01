#' @importFrom dplyr arrange desc distinct filter
#' @importFrom rlang .data
#' 
select_phenames <- function(peak_df, primary = NULL, pheno_mx,
                            cor_covar = FALSE, covar_df = NULL) {
  if(shiny::isTruthy(peak_df) && nrow(peak_df) ) {
    peak_df <- dplyr::distinct(
      dplyr::arrange(
        peak_df,
        dplyr::desc(.data$qtl_lod)),
      .data$phenotype, .data$addcovar)
  } else {
    return(NULL)
  }
  if(is.null(primary)) {
    if(shiny::isTruthy(peak_df) && nrow(peak_df) ) {
      phenames <- peak_df$phenotype
    } else {
      return(NULL)
    }
  } else {
    # Residuals after covariates if `cor_covar` is `TRUE`.
    if(cor_covar) pheno_mx <- covar_resid(pheno_mx, peak_df, covar_df)
    phenames <- cor_phenames(pheno_mx, peak_df, primary)
    
    if(is.null(phenames)) return(NULL)
  }

  selected <- NULL
  choices <- phenames
  
  ## Update phenames to include selected (but not "")
  if(is.null(primary)) { # Primary name.
    label <- "Primary phenotype"
  } else { # Additional names.
    phenames <- phenames[phenames != primary]
    label <- "More phenotypes"
  }
  
  list(label = label, choices = choices, selected = selected)
}
cor_phenames <- function(pheno_mx, peak_df, primary, min_cor = 0.1) {
  if(ncol(pheno_mx) == 1) return(NULL)

  wh <- match(primary, colnames(pheno_mx))
  if(is.na(wh)) return(NULL)
  # Order the ranks of absolute correlation.
  cx <- cor(pheno_mx[,wh], pheno_mx[,-wh, drop = FALSE], use = "pair",
            method = "spearman")
  cx <- cx[, order(-abs(cx))]
  # reduce to larger cor, but ensure at least one phenotype.
  min_cor <- min(min_cor, max(abs(cx)))
  cx <- cx[abs(cx) >= min_cor]
  # turn correlation into `+/-n`, with `n = 10 * abs(cx)`.
  nx <- names(cx)
  sx <- c("-","+")[(sign(cx) + 3) / 2]
  cx <- round(10*cx)
  paste0(sx, pmin(abs(cx), 9), " ", nx)
}
cor_remove <- function(phenames) {
  if(is.null(phenames)) return(NULL)
  stringr::str_remove(phenames, "^[+-][1-9] ")
}
covar_resid <- function(pheno_mx, peak_df, covar_df) {
  if(is.null(covar_df)) return(pheno_mx)
  # Get residuals for each column of `pheno_mx` using `addcovar` in `peak_df`.
  phenames <- colnames(pheno_mx)
  out <- pheno_mx
  for(i in seq_along(phenames)) {
    y = rankZ(pheno_mx[,i])
    dat <- dplyr::bind_cols(covar_df, y = y)
    form <- formula(paste0("y",
      dplyr::filter(peak_df, .data$phenotype == phenames[i])$addcovar))
    out[!is.na(y),i] <- c(resid(lm(form, dat)))
  }
  out
}