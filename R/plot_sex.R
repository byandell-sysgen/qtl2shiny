# Plot across traits
#
# Plot of density and pairwise scatterplots.
plot_sex <- function(phe, cov) {
  if(is.null(phe) | is.null(cov)) return(plot_null())
  phename <- colnames(phe)
  if(!length(phename)) return(plot_null())
  if(!length(qtl2::get_common_ids(phe, cov))) return(plot_null("phe-cov mismatch"))
  if(length(phename) > 10) {
    cat(file=stderr(), "\nOnly first 10 phenotypes used\n")
    phename <- phename[seq_len(10)]
    phe <- phe[,phename]
  }
  if(m <- match("sex", tolower(dimnames(cov)[[2]]), nomatch = 0)) {
    ## Need sex in covar. Ignore actual covariates for analyses.
    insex <- data.frame(phe, sex = cov[,m])
    
    if(length(phename) == 1) {
      ggplot2::ggplot(insex, 
                      ggplot2::aes_string(phename, col="sex")) +
        ggplot2::geom_density(na.rm = TRUE) + 
        ggplot2::geom_rug()
    } else {
      any.na <- apply(insex, 1, function(x) any(is.na(x)))
      GGally::ggscatmat(insex[!any.na,], seq_len(ncol(phe)), color="sex")
    }
  } else {
    if(length(phename) == 1) {
      ggplot2::ggplot(phe, 
                      ggplot2::aes_string(phename)) +
        ggplot2::geom_density(na.rm = TRUE) + 
        ggplot2::geom_rug()
    } else {
      any.na <- apply(phe, 1, function(x) any(is.na(x)))
      GGally::ggscatmat(phe[!any.na,], seq_len(ncol(phe)))
    }
  }
}
