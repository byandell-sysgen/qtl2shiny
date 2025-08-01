plot_hot <- function(peak_set, scan_obj, window_Mbp) {
  if(is.null(peak_set)) return(NULL)
  # reduce to chrs with positive counts
  if(!any(peak_set %in% colnames(scan_obj$scan))) return(NULL)
  peak_set <- peak_set[!is.na(match(peak_set, colnames(scan_obj$scan)))]
  scan_obj <- subset(scan_obj, nonzero = peak_set)
  
  # want to order max column of peak_set
  map <- scan_obj$map
  out_peaks <- scan_obj$scan
  
  ## Build up count of number of peaks
  pheno_types <- colnames(out_peaks)
  lodcolumns <- match(peak_set, pheno_types)
  lodcolumns <- lodcolumns[!is.na(lodcolumns)]

  # Reorder by decreasing count. Want to have highest count in back.
  if(length(lodcolumns) > 1) {
    o <- order(-apply(out_peaks[, lodcolumns, drop = FALSE], 2, sum))
    lodcolumns <- lodcolumns[o]
  }
  pheno_types <- pheno_types[lodcolumns]

  # Set up colors
  col <- seq_along(pheno_types)
  names(col) <- pheno_types
  
  nchr <- length(map)
  xaxt <- ifelse(nchr < 5, "y", "n")
  traits <- ifelse(length(pheno_types) == 1, pheno_types, "traits")
  
  # Kludge 
  if(nrow(out_peaks) == length(unlist(map)) & length(lodcolumns)) {
    qtl2ggplot::ggplot_scan1(out_peaks, map, lodcolumn=lodcolumns,
         col = col[lodcolumns],
         ylab = "phenotype count",
         ylim = c(0, max(out_peaks[,lodcolumns], na.rm = TRUE)),
         xaxt = xaxt,
         gap = 25 / nchr) +
      ## add mtext for peak_set
      ggplot2::ggtitle(paste0("number of ", traits,
                              " in ", window_Mbp, "Mbp window"))
  } else {
    plot_null("no data")
  }
}