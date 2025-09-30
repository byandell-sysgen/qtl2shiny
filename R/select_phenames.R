#' @importFrom dplyr arrange desc distinct filter
#' @importFrom rlang .data
#' 
select_phenames <- function(peak_df, primary = NULL, pheno_mx,
                            cor_covar = FALSE, covar_df = NULL) {
  if(is.null(primary)) {
    if(shiny::isTruthy(peak_df) && nrow(peak_df) ) {
      # Use `gene_short` instead of `phenotype` if available.
      peak_df <- replace_peak_gene_short(peak_df)
      
      phenames <- dplyr::distinct(
        dplyr::arrange(
          peak_df,
          dplyr::desc(.data$qtl_lod)),
        .data$phenotype)$phenotype
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
  
  # Modify `colnames` of `pheno_mx` if using `gene_short`.
  # Assuming here that all `colnames` are in `peak_df$phenotype`.
  colnames(pheno_mx) <- pheno_name_gene_short(peak_df, colnames(pheno_mx))
  
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
  out <- matrix(NA, nrow(pheno_mx), ncol = ncol(pheno_mx),
                dimnames = dimnames(pheno_mx))
  for(i in colnames(pheno_mx)) {
    y = rankZ(pheno_mx[,i])
    dat <- dplyr::bind_cols(covar_df, y = y)
    form <- formula(paste0("y",
      dplyr::filter(peak_df, .data$phenotype == i)$addcovar))
    out[!is.na(y),i] <- resid(lm(form, dat))
  }
  out
}