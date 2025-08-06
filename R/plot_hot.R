plot_hot <- function(hotspot_obj, class = 1, window_Mbp = 1) {
  if(is.null(class)) return(NULL)
  # reduce to chrs with positive counts
  classes <- colnames(hotspot_obj$scan)
  if(is.character(class)) {
    if(!any(class %in% classes)) return(NULL)
    class <- class[!is.na(match(class, classes))]
  } else {
    class <- classes[class]
  }
  hotspot_obj <- subset(hotspot_obj, nonzero = class)
  
  # want to order max column of class
  map <- hotspot_obj$map
  out_peaks <- hotspot_obj$scan
  
  ## Build up count of number of peaks
  pheno_types <- colnames(out_peaks)
  lodcolumns <- match(class, pheno_types)
  lodcolumns <- lodcolumns[!is.na(lodcolumns)]

  # Reorder by decreasing count. Want to have highest count in back.
  if(length(lodcolumns) > 1) {
    o <- order(-apply(out_peaks[, lodcolumns, drop = FALSE], 2, sum))
    lodcolumns <- lodcolumns[o]
  }
  pheno_types <- pheno_types[lodcolumns]

  # Set up colors
  col <- seq_len(ncol(out_peaks))
  names(col) <- colnames(out_peaks)
  
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
      ## add mtext for class
      ggplot2::ggtitle(paste0("hotspot size for ", traits,
                              " in ", window_Mbp, "Mbp window"))
  } else {
    plot_null("no data")
  }
}