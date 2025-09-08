#' Create list with phenotypes in region
#' 
#' @param chr_id,scan_window chromosome and start and end value
#' @param covar covariate data frame
#' @param map list or vector of map positions 
#' @param peaks_df table of peaks
#' @param pheno_data matrix of phenotype data
#' @param drivers number of drivers (1 or 2; default is 2)
#' 
#' @return list containing \code{pheno}, \code{annot} and \code{covar}.
#'
#' @examples
#' dirpath <- "https://raw.githubusercontent.com/rqtl/qtl2data/master/DOex"
#' 
#' # Read DOex example cross from 'qtl2data'
#' DOex <- qtl2::read_cross2(file.path(dirpath, "DOex.zip"))
#' DOex <- subset(DOex, chr = "2")
#' 
#' # Calculate genotype and allele probabilities
#' pr <- qtl2::calc_genoprob(DOex, error_prob=0.002)
#' 
#' # Summary of coefficients at scan peak
#' scan_pr <- qtl2::scan1(pr, DOex$pheno)
#' peaks_df <- summary(scan_pr, DOex$pmap)
#' 
#' # Select Sex and Cohort columns of covariates
#' analyses_tbl <- data.frame(pheno = "OF_immobile_pct", Sex = TRUE, Cohort = TRUE)
#' 
#' # Get phenos in region.
#' out <- pheno_region("2", c(90, 100), DOex$covar, DOex$pmap, peaks_df, analyses_tbl, DOex$pheno)
#' str(out)
#' 
#' @importFrom dplyr filter group_by inner_join left_join n rename summarize ungroup
#' @importFrom qtl2 find_marker
#' @importFrom stringr str_split
#' @importFrom rlang .data
#' 
#' @export
#' 
pheno_region <- function(chr_id, scan_window, covar, map, 
                         peaks_df, pheno_data,
                         drivers = 2) {
  
  start_val <- scan_window[1]
  end_val <- scan_window[2]
  # Replace NA covariate calls by FALSE.
  covars <- colnames(covar)

  ## Annotation
  annot <- 
    dplyr::rename(
      dplyr::inner_join(
        peaks_df, 
        dplyr::ungroup(
          dplyr::summarize(
            dplyr::group_by(
              peaks_df, 
              .data$phenotype),
            qtl_ct = dplyr::n(),
            info = paste0(.data$qtl_chr, "@",
                          round(.data$qtl_pos), ":",
                          round(.data$qtl_lod), collapse = ","))),
        by = "phenotype"),
      id = .data$phenotype)

  # Used in 'qtl2shiny'.
  if("phenotype_class" %in% names(annot)) {
    annot <- dplyr::rename(
      annot,
      biotype = .data$phenotype_class)
  }
  
  # Reduce to phenotypes with peaks in region.
  annot <- dplyr::filter(
    annot, 
    .data$qtl_chr == chr_id,
    .data$qtl_pos >= start_val,
    .data$qtl_pos <= end_val)
  
  # This limits to traits that reside locally. Only make sense for expression data.
  annot$local <- FALSE
  
  # Identify markers for drivers of mediators.
  if(drivers == 2)
    annot$driver <- qtl2::find_marker(map, chr_id, annot$pos)
  
  # Make sure some pheno_data have peaks.
  m <- match(annot$id, colnames(pheno_data))
  if(any(is.na(m)))
    return(NULL)

  list(pheno = pheno_data,
       annot = annot, 
       covar = covar)
}

#' Create list with expression phenotypes in region
#' 
#' @param project_dir project directory with mRNA data in subdirector \code{RNAseq}
#' @param query_mrna query routine for mRNA data (see \code{\link{create_mrna_query_func}})
#' 
#' @seealso \code{\link{create_mrna_query_func}}
#' @rdname pheno_region
#' @export
expr_region <- function(chr_id, scan_window, covar, map, 
                        project_dir, drivers = 2,
                        query_mrna = create_mrna_query_func(project_dir)) {
  
  start_val <- scan_window[1]
  end_val <- scan_window[2]
  # Get expression mRMNA measurements.
  # This creates a list with elements expr, annot, peaks.
  out <- query_mrna(chr_id, start_val, end_val, qtl = TRUE)
  if(is.null(out))
    return(NULL)
  
  # Identify markers for drivers of expression mediators.
  if(drivers == 2)
    out$annot$driver <- qtl2::find_marker(map, chr_id, out$annot$qtl_pos)
  
  # Identify covariates
  expr_covars <- unique(out$annot$covar)
  if(length(expr_covars) > 1)
    warning("only using first type of covariate for expression")
  expr_covars <- stringr::str_split(expr_covars[1], ",")[[1]]
  m <- match(tolower(expr_covars), tolower(colnames(covar)), nomatch = 0)
  if(any(m == 0))
    warning(paste(paste(expr_covars, collapse = ","), "not found in data"))
  # Get covariates for expression mediators
  out$covar <- covar[, m]
  
  out
}
