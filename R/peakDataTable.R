# Want alternative table of top phenotypes if only one chr.
# Use set_par$dataset and input$chr_ct
# length(chr_ct) == 1 & !("all" %in% chr_ct)
# dplyr::filter(peak_df(), chr == chr_ct, pheno_type %in% dataset)
# dplyr::select(pheno, pheno_type, chr, pos, lod)
#' @importFrom rlang .data
#' @importFrom dplyr across arrange desc filter mutate where
#' 
peakDataTable <- function(peak_df, hotspot_df = NULL, local = TRUE,
                          chr_id = NULL) {
  if(is.null(peak_df)) return(NULL)
  if(!is.null(hotspot_df)) {
    # Filter to selected `chr_id`
    chrs <- unique(hotspot_df$chr)
    if(local & !is.null(chr_id))
      chrs <- chr_id
    peak_df <- dplyr::filter(peak_df, .data$qtl_chr %in% chrs)
    
    # If not `all` then filter peaks to the selected `classes`.
    classes <- unique(hotspot_df$pheno)
    if(!("all" %in% classes)) {
      peak_df <- dplyr::filter(peak_df, .data$phenotype_class %in% classes)
      if(!nrow(peak_df)) return(NULL)
    }
  }
  # Use `gene_short` instead of `phenotype` if available.
  peak_df <- replace_peak_gene_short(peak_df)
  
  dplyr::arrange(
    dplyr::mutate(
      dplyr::select(
        peak_df,
        .data$phenotype, .data$phenotype_class, .data$subject, .data$model,
        .data$qtl_chr, .data$qtl_pos, .data$qtl_lod),
      dplyr::across(dplyr::where(is.double), signif, digits = 4)),
    dplyr::desc(.data$qtl_lod))
}