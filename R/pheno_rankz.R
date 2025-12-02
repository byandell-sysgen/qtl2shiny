pheno_rankz <- function(pheno_mx, raw_data = FALSE) {
  rout <- row.names(pheno_mx)
  if(!raw_data) {
    pheno_mx <- apply(pheno_mx, 2, rankZ)
  }
  row.names(pheno_mx) <- rout
  pheno_mx
}