pheno_read <- function(project_df, analyses_df, transform = TRUE) {
  # Read the phenos we need.
  phenos <- analyses_df$pheno
  pheno_data <- read_project(project_df, "pheno_data", phenos)
  
  if(transform) {
    transform <- analyses_df$transf
  } else {
    transform <- NULL
  }
  qtl2mediate::pheno_trans(pheno_data, 
              phenos, 
              transform,
              analyses_df$offset,
              analyses_df$winsorize)
}