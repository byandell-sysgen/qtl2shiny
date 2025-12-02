## Plot pseudogene and gene locations along with SNPs.
## Ordered by pseudogenes first, then genes
## negative (blue) strand, then unknown (grey), then positive (red) strand.
## Filtering removes feature_tbl class, so need to be explicit.
##
plot_gene_region <- function(pheno, gene_region_tbl, top_snps_tbl, wrng,
                             use_snp, snp_action = "basic") {
  if(use_snp) {
    top_snps_rng <- subset(top_snps_tbl, 
                           wrng[1], wrng[2],
                           pheno)
    if(!nrow(top_snps_rng))
      top_snps_rng <- NULL
  } else {
    top_snps_rng <- NULL
  }

  if(nrow(gene_region_tbl)) {
    p <- ggplot2::autoplot(subset(gene_region_tbl, wrng[1], wrng[2]),
              top_snps_tbl = top_snps_rng)
    if(use_snp)
      p <- p + 
        ggplot2::ggtitle(paste("SNPs for", pheno, snp_action))
  } else {
    p <- plot_null()
  }
  p
}
