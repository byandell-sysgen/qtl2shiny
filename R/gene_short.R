create_peak_gene_short <- function(peak_df) {
  # Add column `gene_short` with gene name if available.
  peak_df$gene_short <- peak_df$phenotype
  if("gene_symbol" %in% names(peak_df)) {
    peak_df$gene_short <- peak_df$gene_symbol
    if("gene_id" %in% names(peak_df)) {
      # Make `gene_short` as `symbol_id` with simplfied `gene_id`.
      # See `https://github.com/byandell-sysgen/foundrHarmony/blob/main/R/LivRna.R`.
      peak_df <-
        dplyr::mutate(
          peak_df,
          gene_short = 
            ifelse(
              is.na(.data$gene_id),
              # No `gene_id` value.
              .data$gene_short,
              # Encode as `symbol_ggg` with `ggg` the non-zero ID numbers.
              paste(.data$gene_short,
                    str_remove(.data$gene_id, "ENSMUSG0+"), sep = "_")))
      if("transcript_id" %in% names(peak_df)) {
        # Isoforms.
        # Similar idea; already have `gene_id`, so just add `transcript_id`.
        peak_df <-
          dplyr::mutate(
            peak_df,
            gene_short = 
              ifelse(
                is.na(.data$transcript_id),
                # No `transcript_id` value.
                .data$gene_short,
                # Encode as `symbol_ggg_ttt` with `ttt` the non-zero ID numbers.
                paste(.data$transcript_symbol,
                      str_remove(.data$gene_id, "ENSMUSG0+"),
                      str_remove(.data$transcript_id, "ENSMUST0+"), sep = "_")))
      }
    }
  }
  peak_df
}
replace_peak_gene_short <- function(peak_df) {
  if(!("gene_short" %in% names(peak_df))) return(peak_df)
  dplyr::rename(
    dplyr::select(peak_df, -phenotype),
    phenotype = "gene_short")
}
pheno_name_gene_short <- function(peak_df, phenames) {
  if(!("gene_short" %in% names(peak_df))) return(phenames)
  m <- match(phenames, peak_df$phenotype, nomatch = 0)
  if(any(m == 0)) message("missing phenames for gene_short")
  phenames[m > 0] <- peak_df$gene_short[m]
  phenames
}
