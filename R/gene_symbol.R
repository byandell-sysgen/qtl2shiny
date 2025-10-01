gene_symbol_peak <- function(peak_df, na_keep = FALSE) {
  # I do need keep original `phenotype` for phenos!
  peak_df$phenotype_original <- peak_df$phenotype
  if("gene_symbol" %in% names(peak_df)) {
    peak_df$phenotype <- peak_df$gene_symbol
    if("gene_id" %in% names(peak_df)) {
      # Make `phenotype` as `symbol_id` with simplfied `gene_id`.
      # See `https://github.com/byandell-sysgen/foundrHarmony/blob/main/R/LivRna.R`.
      peak_df <-
        dplyr::mutate(
          peak_df,
          phenotype = 
            ifelse(
              is.na(.data$gene_id),
              # No `gene_id` value.
              .data$phenotype,
              # Encode as `symbol_ggg` with `ggg` the non-zero ID numbers.
              paste(.data$phenotype,
                    str_remove(.data$gene_id, "ENSMUSG0+"), sep = "_")))
      if("transcript_id" %in% names(peak_df)) {
        # Isoforms.
        if(!na_keep) {
          # Remove transcripts with missing `transcript_symbol`.
          # Make sure it is transcript with a `transcript_id`.
          peak_df <- dplyr::filter(
            peak_df,
            !is.na(.data$transcript_id) & !is.na(.data$transcript_symbol))
        }
        # Similar idea; already have `gene_id`, so just add `transcript_id`.
        peak_df <-
          dplyr::mutate(
            peak_df,
            phenotype = 
              ifelse(
                is.na(.data$transcript_id),
                # No `transcript_id` value.
                .data$phenotype,
                # Encode as `symbol_ggg_ttt` with `ttt` the non-zero ID numbers.
                paste(.data$transcript_symbol,
                      str_remove(.data$gene_id, "ENSMUSG0+"),
                      str_remove(.data$transcript_id, "ENSMUST0+"), sep = "_")))
      }
    }
  }
  peak_df
}
gene_symbol_pheno <- function(pheno_mx, peak_df = NULL, drop_miss = TRUE) {
  if(is.null(peak_df) ||
     !("phenotype_original" %in% names(peak_df))) return(pheno_mx)
  phenames <- colnames(pheno_mx)
  m <- match(phenames, peak_df$phenotype_original, nomatch = 0)
  phenames[m > 0] <- peak_df$phenotype[m]
  colnames(pheno_mx) <- phenames
  if(drop_miss) {
    pheno_mx[,phenames, drop = FALSE]
  } else {
    if(any(m == 0))
      message("missing phenames for original phenotype")
    pheno_mx
  }
}
