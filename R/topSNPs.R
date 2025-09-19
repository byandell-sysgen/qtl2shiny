topSNPs <- function(top_snps_tbl, snpinfo,
                    snp_scan_obj, gene_exon_tbl,
                    pheno_name) {
  if(is.null(pheno_name) | is.null(top_snps_tbl)) return(NULL)
  out <- qtl2pattern::merge_feature(top_snps_tbl, snpinfo,
                                    snp_scan_obj, 1.5, 0, gene_exon_tbl)
  # Check if all sexes done. In that case, choose AddSex.
  if(!(pheno_name %in% names(out))) {
    m <- grep("AddSex", names(out))
    if(is.na(m))
      return(NULL)
    names(out)[m] <- pheno_name
  }
  out
}