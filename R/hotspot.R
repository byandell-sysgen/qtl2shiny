#' Hotspots for phenotypes
#'
#' Count hotspots by pheno_group and pheno_type.
#'
#' @param map list of genetic maps
#' @param peak_df data frame of peak information
#' @param peak_window half-width of peak window in Mbp
#' @param minLOD minimum LOD to include in count
#' @param chrs chromosomes to subset if not `NULL`
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
#' peak_df <- summary(scan_pr, DOex$pmap)
#' 
#' hotspot(DOex$pmap, peak_df)
#' 
#' # Select Sex and Cohort columns of covariates
#' analyses_tbl <- data.frame(pheno = "OF_immobile_pct", Sex = TRUE, Cohort = TRUE)
#' 
#' # Get hotspot (only one phenotype here).
#' out <- hotspot(DOex$pmap, peak_df)
#' summary(out)
#'
#' @export
#'
#' @importFrom purrr map transpose
#' @importFrom dplyr arrange bind_rows bind_cols bind_rows distinct everything
#'             filter group_by mutate one_of select summarize ungroup
#' @importFrom rlang .data
#' @importFrom qtl2ggplot summary_scan1
hotspot <- function(map, peak_df, peak_window = 1, minLOD = 5.5,
                    chrs = NULL) {
  if(is.null(peak_df)) 
    return(NULL)
  peak_df <- dplyr::filter(peak_df, .data$qtl_lod >= minLOD)
  if(!is.null(chrs))
    peak_df <- dplyr::filter(peak_df, .data$qtl_chr %in% chrs)
  if(!nrow(peak_df))
    return(NULL)
  
  # Set up list by chr of positions and peaks.
  
  # Rounded off sequence of map positions by chr.
  round_pos <- function(x, peak_window) {
    rng <- round(range(x) / peak_window) * peak_window
    out <- seq(rng[1], rng[2], by = peak_window)
    names(out) <- out
    out
  }
  chr_pos <- purrr::map(map, round_pos, peak_window)
  for(chr in names(chr_pos)) {
    names(chr_pos[[chr]]) <- paste(chr, names(chr_pos[[chr]]), sep = ":")
  }

  out_chr <- purrr::transpose(list(pos = chr_pos,
                                   peaks = split(peak_df, peak_df$qtl_chr)))

  peaks_class <- function(posi, peak_df, peak_window=1) {
    if(is.null(peak_df))
      return(NULL)
    # count peaks at position by class
    peaks_by_class <- split(peak_df, peak_df$phenotype_class)
    out <- data.frame(purrr::map(peaks_by_class,
                                 outer_window,
                                 posi, peak_window),
                      check.names = FALSE)
    if(!nrow(out))
      return(NULL)
    # This is adding extra column sometimes. Fix.
    if(max(out) == 0)
      return(NULL)
    rownames(out) <- posi
    out
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
  attr(out, "window") <- peak_window
  class(out) <- c("hotspot", "list")
  out
}
#' @export
#' @method summary hotspot
#' @rdname hotspot
summary.hotspot <- function(object, ...) {
  if(is.null(object)) return(NULL)
  
  map <- object$map
  scan <- object$scan
  
  # Combine hotspots that overlap but do not have same pos
  # by selecting multiple hotspots in phenoNamesApp.
  
  # Use all columns except "all". 
  if(nrow(scan) == length(unlist(map))) {
    chr_id <- names(map)
    scan <-                 
      # Rename `lod` column as `count`.
      dplyr::rename(
        # Scan1 summary
        qtl2ggplot::summary_scan1(scan, map, chr = chr_id),
        count = .data$lod)
    # Sum by `chr` and `pos` across `pheno` into `all`.
    scan_all <- dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(scan, .data$chr, .data$pos, .data$marker),
        pheno = "all",
        count = sum(.data$count)))
    # Bind rows and arrange.
    scan <- dplyr::arrange(
      dplyr::bind_rows(scan, scan_all),
      .data$chr, .data$pos)
    
    out <- dplyr::select(
      dplyr::arrange(
        dplyr::filter(
          dplyr::select(
            dplyr::mutate(
              # Drop `marker` column.
              dplyr::select(
                tidyr::pivot_wider(
                  scan,
                  names_from = "pheno", values_from = "count"),
                -.data$marker),
              hotspot = encode_hotspot(.data$chr, .data$pos, .data$all)),
            hotspot, dplyr::everything()),
          .data$all > 0),
        dplyr::desc(.data$all), .data$chr, .data$pos),
      -all)
    attr(out, "window") <- attr(object, "window")
    out
  } else {
    NULL
  }
}
#' @export
#' @method subset hotspot
#' @rdname hotspot
subset.hotspot <- function(x, chr = NULL, nonzero = NULL, ...) {
  if(!is.null(chr)) {
    x <- list(scan = subset(x$scan, x$map, chr),
                map = x$map[chr])
    class(x) <- c("hotspot", "list")
  }
  # drop chr with all zeroes
  if(!is.null(nonzero)) {
    # Ignore if `nonzero` does not match a column of `x$scan`.
    if(!all(nonzero %in% colnames(x$scan))) return(x)
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
#' @export
#' @method cbind hotspot
#' @rdname hotspot
cbind.hotspot <- function(..., scannames = colnames(scan)) {
  objects <- list(...)
  if(length(objects) <= 1) return(objects[[1]])
  scan <- objects[[1]]$scan
  for(i in 2:length(objects)) {
    scan2 <- objects[[i]]$scan
    # Sum up the "all" (first) column
    scan[,1] <- scan[,1] + scan2[,1]
    # cbind of scan1 objects.
    scan <- cbind(scan, scan2[,-1, drop = FALSE])
  }
  colnames(scan) <- scannames
  out <- objects[[1]]
  out$scan <- scan
  out
}
