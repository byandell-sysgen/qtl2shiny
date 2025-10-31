# Summary across traits
summary_na <- function(phe, cov) {
  dat <- data_sex(phe, cov)
  if(is.null(dat)) return(NULL)
  
  sexname <- attr(dat, "sex")
  phename <- attr(dat, "pheno")
  
  dat <- tidyr::pivot_longer(dat, phename,
                             names_to = "phenotype", values_to = "value")
  dplyr::ungroup(
    dplyr::summarize(
      dplyr::group_by(dat, .data[["phenotype"]], .data[[sexname]]),
      mean = signif(mean(.data$value, na.rm = TRUE), 4),
      sd = signif(sd(.data$value, na.rm = TRUE), 4),
      na = sum(is.na(.data$value))))
}
