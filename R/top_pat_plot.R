# Plot of top patterns.
top_pat_plot <- function(pheno,
                         scan_obj,
                         chr_id,
                         map,
                         xlim,
                         drop_hilit = 1.5,
                         facet = "pheno",
                         snp_action = "basic",
                         cex = 4, ...) {
  if(is.null(pheno) || is.null(snp_action)) return(plot_null())
  mytitle <- FALSE
  if(length(pheno) == 1) {
    mytitle <- paste(pheno, "chr", chr_id)
    if(snp_action != "basic")
      mytitle <- paste(mytitle, snp_action)
  }
  legend.title <- "pattern"
  if(length(pheno) == 1) {
    facet <- NULL
  } else {
    if(facet == "pattern")
      legend.title <- "pheno"
  }

  lodcol <- match(pheno, colnames(scan_obj))
  if(any(is.na(lodcol))) # Probably all sex
    lodcol <- match("AddSex", colnames(scan_obj))
  if(any(is.na(lodcol)))
    return(plot_null("scan name mismatch"))
  colnames(scan_obj)[lodcol] <- pheno
  scan_obj <- subset(scan_obj, 
                     lodcolumn = lodcol)
  
  ggplot2::autoplot(
    scan_obj, map, seq_along(pheno),
    xlim = xlim, main = mytitle,
    patterns = "hilit", drop_hilit = drop_hilit,
    facet = facet, legend.title = legend.title, cex = cex, ...)
}
