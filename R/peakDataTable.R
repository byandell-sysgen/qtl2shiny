# Want alternative table of top phenotypes if only one chr.
# Use set_par$dataset and input$chr_ct
# length(chr_ct) == 1 & !("all" %in% chr_ct)
# dplyr::filter(peak_df(), chr == chr_ct, pheno_type %in% dataset)
# dplyr::select(pheno, pheno_type, chr, pos, lod)
#' @importFrom rlang .data
#' 
peakDataTable <- function(scan_tbl, peak_df) {
    chrs <- unique(scan_tbl$chr)
    pheno_types <- unique(scan_tbl$pheno)
    peak_df <- dplyr::filter(peak_df, .data$chr %in% chrs)
    pheno_groups <- unique(peak_df$pheno_group)
    if(all(pheno_types %in% pheno_groups)) {
      pheno_types <- dplyr::filter(
        dplyr::distinct(peak_df, .data$pheno_type, .data$pheno_group),
        .data$pheno_group %in% pheno_types)$pheno_type
    }
    dplyr::arrange(
      dplyr::select(
        dplyr::filter(peak_df,
                      .data$pheno_type %in% pheno_types),
        .data$pheno, .data$pheno_type, .data$chr, .data$pos, .data$lod),
      desc(.data$lod))
}