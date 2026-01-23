# Top SNP Association Plot Wrapper
top_snp_asso <- function(scan_obj, snpinfo, xlim,
                         snp_action="add+dom",
                         phename = dimnames(scan_obj)[[2]],
                         minLOD = NULL,
                         show_all_snps = FALSE, cex = 4) {
  if(is.null(phename) | is.null(scan_obj) | is.null(xlim))
    return(print(plot_null()))
  
  drop_hilit <- min(1.5, max(unclass(scan_obj)) - 3)
  if(!is.null(minLOD)) {
    minLOD <- as.numeric(minLOD)
    if(!is.na(minLOD))
      drop_hilit <- max(unclass(scan_obj)) - minLOD
  }
  facet <- NULL
  facet <- if(length(phename) > 1) 
    facet <- "pheno"
  
  ggplot2::autoplot(
    scan_obj, snpinfo, seq_along(phename),
    show_all_snps = show_all_snps,
    drop_hilit = drop_hilit,
    xlim = xlim,
    facet = facet,
    cex = cex)
}
