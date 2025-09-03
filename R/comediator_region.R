#' Create list with mediators in region
#' 
#' @param pheno_name phenotype name
#' @param chr_id,scan_window chromosome and start and end value
#' @param covar covariate data frame
#' @param analyses_tbl table of analyses
#' @param peaks table of peaks
#' @param qtls number of drivers (1 or 2; default is 2)
#' @param pmap physical map
#' @param pheno_data phenotype data
#' 
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @export
comediator_region <- function(pheno_name, chr_id, scan_window, 
                              covar, peaks_df, 
                              qtls = 2, pmap, pheno_data) {
  peaks_df <- dplyr::filter(peaks_df,
                         .data$phenotype != pheno_name)
  # Filter peaks_df and analyses to region and drop pheno_name
  peaks_local <- dplyr::filter(peaks_df,
                               .data$chr == chr_id,
                               .data$pos >= scan_window[1],
                               .data$pos <= scan_window[2])

  # Read the phenos we need.
  phenos <- unique(peaks_df$phenotype)
  pheno_data <- pheno_data[, phenos, drop = FALSE]
  
  # Create comediator object.
  out <- pheno_region(chr_id, scan_window, covar, pmap,
                      peaks_df, pheno_data, drivers = qtls)
  
  out
}
#' Get mediators of same type as pheno_name
#' 
#' @param comed object from `comediator_region`
#' @param doThis do this if `TRUE`
#' @export
comediator_type <- function(comed, peaks, pheno_name, doThis) {
  if(doThis & !is.null(comed)) {
    phe_type <- (dplyr::filter(peaks, .data$phenotype == pheno_name))$phenotype_class[1]
    not_type <- comed$annot$id[comed$annot$biotype != phe_type]
    comed$comediators <- comed$comediators[, not_type]
  }
  
  comed
}