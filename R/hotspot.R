#' Hotspots for phenotypes
#'
#' Count hotspots by pheno_group and pheno_type.
#'
#' @param map list of genetic maps
#' @param peaks data frame of peak information
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
#' peaks <- summary(scan_pr, DOex$pmap)
#' 
#' hotspot(DOex$pmap, peaks)
#' 
#' # Select Sex and Cohort columns of covariates
#' analyses_tbl <- data.frame(pheno = "OF_immobile_pct", Sex = TRUE, Cohort = TRUE)
#' 
#' # Get hotspot (only one phenotype here).
#' out <- hotspot(DOex$pmap, peaks)
#' summary(out)
#'
#' @export
#'
#' @importFrom purrr map transpose
#' @importFrom dplyr bind_cols bind_rows distinct everything filter one_of select
#' @importFrom rlang .data
#'
hotspot <- function(map, peaks, peak_window = 1, minLOD = 5.5) {
  # Set up list by chr of postions and peaks.
  round_pos <- function(x) {
    rng <- round(range(x))
    out <- seq(rng[1],rng[2])
    names(out) <- out
    out
  }
  chr_pos <- purrr::map(map, round_pos)
  # Kludge
  for(chr in names(chr_pos)) {
    names(chr_pos[[chr]]) <- paste(chr, names(chr_pos[[chr]]), sep = ":")
  }

  peaks <- dplyr::filter(peaks,
                         .data$lod >= minLOD)
  if(!nrow(peaks))
    return(NULL)
  out_chr <- purrr::transpose(list(pos = chr_pos,
                                   peaks = split(peaks, peaks$chr)))

  peaks_type <- function(posi, peaks, peak_window=1) {
    if(is.null(peaks))
      return(NULL)
    # count peaks at position by type
    if(!("pheno_type" %in% names(peaks))) {
      peaks$pheno_type <- "type"
    }
    if(!("pheno_group" %in% names(peaks))) {
      peaks$pheno_group <- "group"
    }
    peaks_by_type <- split(peaks, peaks$pheno_type)
    out <- data.frame(purrr::map(peaks_by_type,
                                 outer_window,
                                 posi, peak_window),
                      check.names = FALSE)
    if(!nrow(out))
      return(NULL)
    # count peaks by group. But should check to make sure group has unique name.
    # and if pheno_group = pheno_type, then only need one?
    groups <- dplyr::distinct(peaks, .data$pheno_group, .data$pheno_type)
    groups <- split(groups$pheno_type, groups$pheno_group)
    grps <- data.frame(
      purrr::map(groups,
                 function(x, out) {
                   apply(out[, x, drop = FALSE], 1, sum)
                   },
                 out),
      check.names = FALSE)
    out$all <- apply(out, 1, sum)
    # This is adding extra column sometimes. Fix.
    m <- match(colnames(grps), colnames(out), nomatch = 0)
    if(any(m > 0))
      colnames(grps) <- paste0(colnames(grps), "G")
    out <- dplyr::bind_cols(out, grps)
    if(max(out) == 0)
      return(NULL)
    rownames(out) <- posi
    dplyr::select(out, all,
                  dplyr::one_of(names(groups)),
                  dplyr::everything())
  }
  outer_window <- function(peaksi, posi, peak_window = 1) {
    peaksi <- dplyr::filter(peaksi)$pos
    apply(outer(peaksi, posi,
                function(x,y,z) abs(x-y) <= z,
                peak_window),
          2, sum)
  }

  # Want to identify what purrr::map are NULL and adjust map
  out_peaks <- purrr::map(out_chr,
                          function(x, peak_window) peaks_type(x$pos, x$peaks, peak_window),
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
