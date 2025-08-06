#' Hotspots for phenotypes
#'
#' Count hotspots by pheno_group and pheno_type.
#'
#' @param map list of genetic maps
#' @param peaks_df data frame of peak information
#' @param peak_window half-width of peak window in Mbp
#' @param minLOD minimum LOD to include in count
#'
#' @return object of class hotspot as list of \code{\link[qtl2]{scan1}} and \code{map} objects.
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
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
#' hotspot(DOex$pmap, peaks_df)
#' 
#' # Select Sex and Cohort columns of covariates
#' analyses_tbl <- data.frame(pheno = "OF_immobile_pct", Sex = TRUE, Cohort = TRUE)
#' 
#' # Get hotspot (only one phenotype here).
#' out <- hotspot(DOex$pmap, peaks_df)
#' summary(out)
#'
#' @export
#'
#' @importFrom purrr map transpose
#' @importFrom dplyr bind_cols bind_rows distinct everything filter one_of select
#' @importFrom rlang .data
#'
hotspot <- function(map, peaks_df, peak_window = 1, minLOD = 5.5) {
  peaks_df <- dplyr::filter(peaks_df, .data$qtl_lod >= minLOD)
  if(!nrow(peaks_df))
    return(NULL)
  
  # Set up list by chr of positions and peaks.
  
  # Rounded off sequence of map positions by chr.
  round_pos <- function(x) {
    rng <- round(range(x))
    out <- seq(rng[1],rng[2])
    names(out) <- out
    out
  }
  chr_pos <- purrr::map(map, round_pos)
  for(chr in names(chr_pos)) {
    names(chr_pos[[chr]]) <- paste(chr, names(chr_pos[[chr]]), sep = ":")
  }

  out_chr <- purrr::transpose(list(pos = chr_pos,
                                   peaks = split(peaks_df, peaks_df$qtl_chr)))

  peaks_class <- function(posi, peaks_df, peak_window=1) {
    if(is.null(peaks_df))
      return(NULL)
    # count peaks at position by class
    peaks_by_class <- split(peaks_df, peaks_df$phenotype_class)
    out <- data.frame(purrr::map(peaks_by_class,
                                 outer_window,
                                 posi, peak_window),
                      check.names = FALSE)
    if(!nrow(out))
      return(NULL)
    out$all <- apply(out, 1, sum)
    # This is adding extra column sometimes. Fix.
    if(max(out) == 0)
      return(NULL)
    rownames(out) <- posi
    dplyr::select(out, all, dplyr::everything())
  }
  outer_window <- function(peaksi, posi, peak_window = 1) {
    peaksi <- dplyr::filter(peaksi)$qtl_pos
    apply(outer(peaksi, posi,
                function(x,y,z) abs(x-y) <= z,
                peak_window),
          2, sum)
  }

  # Want to identify what purrr::map are NULL and adjust map
  out_peaks <- purrr::map(out_chr,
                          function(x, peak_window) peaks_class(x$pos, x$peaks, peak_window),
                          peak_window)
  chr_pos <- chr_pos[!sapply(out_peaks, is.null)]
  if(!length(chr_pos))
    return(NULL)
  out_peaks <- dplyr::bind_rows(out_peaks)
  if(!nrow(out_peaks))
    return(NULL)

  out_peaks <- as.matrix(out_peaks)
  out_peaks[is.na(out_peaks)] <- 0
  # Breaks here when threshold is too large.
  rownames(out_peaks) <- unlist(sapply(chr_pos, names))
  class(out_peaks) <- c("scan1", "matrix")

  out <- list(scan = out_peaks, map = chr_pos)
  class(out) <- c("hotspot", "list")
  out
}
#' @export
#' @method summary hotspot
summary.hotspot <- function(object, ...) {
  summary(object$scan, object$map, ...)
}
#' @export
#' @method subset hotspot
subset.hotspot <- function(x, chr = NULL, nonzero = NULL, ...) {
  if(!is.null(chr)) {
    x <- list(scan = subset(x$scan, x$map, chr),
                map = x$map[chr])
    class(x) <- c("hotspot", "list")
  }
  # drop chr with all zeroes
  if(!is.null(nonzero)) {
    # Ignore if `nonzero` does not match a column of `x$scan`.
    if(!(nonzero %in% names(x$scan))) return(x)
    cts <- apply(x$scan[, nonzero, drop = FALSE], 1, sum)
    chrs <- unlist(tapply(cts,
                          ordered(rep(names(x$map), sapply(x$map, length)),
                                  names(x$map)),
            function(x) !all(x == 0)))
    if(!all(chrs))
      x <- subset(x, chr = names(chrs)[chrs])
  }
  x
}
