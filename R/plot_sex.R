# Plot across traits
#
# Plot of density and pairwise scatterplots.
plot_sex <- function(phe, cov) {
  dat <- data_sex(phe, cov)
  if(is.null(dat)) return(plot_null())
  
  phename <- attr(dat, "pheno")
  sexname <- attr(dat, "sex")
  
  if(!is.null(sexname)) {
    if(length(phename) == 1) {
      ggplot2::ggplot(dplyr::rename(dat,
                                    phe_name = phename,
                                    sex_name = sexname), 
                      ggplot2::aes(phe_name, col = sex_name)) +
        ggplot2::geom_density(na.rm = TRUE) + 
        ggplot2::geom_rug() +
        ggplot2::xlab(phename)
    } else {
      any.na <- apply(dat, 1, function(x) any(is.na(x)))
      GGally::ggscatmat(dat[!any.na,], seq_len(ncol(phe)), alpha = 0.25,
                        color = sexname)
    }
  } else {
    if(length(phename) == 1) {
      ggplot2::ggplot(dplyr::rename(phe, phe_name = phename), 
                      ggplot2::aes(phe_name)) +
        ggplot2::geom_density(na.rm = TRUE) + 
        ggplot2::geom_rug() +
        ggplot2::xlab(phename)
    } else {
      any.na <- apply(phe, 1, function(x) any(is.na(x)))
      GGally::ggscatmat(phe[!any.na,], seq_len(ncol(phe)))
    }
  }
}
data_sex <- function(phe, cov) {
  if(is.null(phe) | is.null(cov)) return(NULL)
  # No columns?
  phename <- colnames(phe)
  if(!length(phename)) return(NULL)
  # Column mismatch?
  if(!length(qtl2::get_common_ids(phe, cov))) return(NULL)
  if(m <- match("sex", tolower(colnames(cov)), nomatch = 0)) {
    sexname <- "sex"
    ## Need sex in covar. Ignore actual covariates for analyses.
    if(md <- match("diet", tolower(colnames(cov)), nomatch = 0)) {
      # If there is a `diet` column unite it with sex
      cov[,m] <- paste(cov[,m], cov[,md], sep = "_")
      sexname <- "sex_diet"
      colnames(cov)[m] <- sexname
      out <- data.frame(phe, sex_diet = cov[,m], check.names = FALSE)
    } else {
      out <- data.frame(phe, sex = cov[,m], check.names = FALSE)
    }
    attr(out, "sex") <- sexname
  } else {
    out <- phe
  }
  attr(out, "pheno") <- phename
  out
}
