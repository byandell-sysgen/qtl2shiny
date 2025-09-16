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
    phenames <- cor_phenames(pheno_mx, primary)
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
cor_phenames <- function(pheno_mx, primary) {
  if(ncol(pheno_mx) == 1) return(NULL)
  wh <- match(primary, colnames(pheno_mx))
  if(is.na(wh)) return(NULL)
  # Order the ranks of absolute correlation.
  cx <- cor(pheno_mx[,wh], pheno_mx[,-wh], use = "pair",
            method = "spearman")
  cx <- cx[, order(rank(-abs(cx), ties.method = "random"))]
  # turn correlation into +/-n, with n = 10 * abs(cx)
  cx <- round(10*cx)
  cx <- cx[abs(cx) > 0]
  sx <- c("-","+")[(sign(cx) + 3) / 2]
  nx <- names(cx)
  paste0(sx, abs(cx), " ", nx)
}
cor_remove <- function(phenames) {
  if(is.null(phenames)) return(NULL)
  stringr::str_remove(phenames, "^[+-][1-9] ")
}