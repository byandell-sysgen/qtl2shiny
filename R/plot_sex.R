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
  if(m <- match("sex", tolower(colnames(cov)), nomatch = 0)) {
    sexname <- "sex"
    ## Need sex in covar. Ignore actual covariates for analyses.
    if(md <- match("diet", tolower(colnames(cov)), nomatch = 0)) {
      # If there is a `diet` column unite it with sex
      cov[,m] <- paste(cov[,m], cov[,md], sep = "_")
      sexname <- "sex_diet"
      colnames(cov)[m] <- sexname
      insex <- data.frame(phe, sex_diet = cov[,m])
    } else {
      insex <- data.frame(phe, sex = cov[,m])
    }
    
    if(length(phename) == 1) {
      ggplot2::ggplot(insex, 
                      ggplot2::aes_string(phename, col = sexname)) +
        ggplot2::geom_density(na.rm = TRUE) + 
        ggplot2::geom_rug()
    } else {
      any.na <- apply(insex, 1, function(x) any(is.na(x)))
      GGally::ggscatmat(insex[!any.na,], seq_len(ncol(phe)), alpha = 0.25, color = sexname)
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
