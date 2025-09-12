#' @importFrom dplyr arrange desc distinct filter
#' @importFrom rlang .data
#' 
select_phenames <- function(peak_df, primary = NULL, pheno_mx) {
  if(is.null(primary)) {
    if(shiny::isTruthy(peak_df) && nrow(peak_df) ) {
      phenames <- dplyr::distinct(
        dplyr::arrange(
          peak_df,
          dplyr::desc(.data$qtl_lod)),
        .data$phenotype)$phenotype
    } else {
      return(NULL)
    }
  } else {
    if(ncol(pheno_mx) == 1) return(NULL)
    wh <- match(primary, colnames(pheno_mx))
    if(is.na(wh)) return(NULL)
    # Order the ranks of absolute correlation.
    cx <- cor(pheno_mx[,wh], pheno_mx[,-wh], use = "pair",
              method = "spearman")
    cx <- cx[, order(rank(-abs(cx), ties.method = "random"))]
    phenames <- names(cx)
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