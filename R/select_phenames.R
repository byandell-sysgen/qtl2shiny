#' @importFrom dplyr arrange desc distinct filter
#' @importFrom rlang .data
#' 
select_phenames <- function(phenames, peak_df, local,
                            chr_id, peak_Mbp, window_Mbp) {
  selected <- phenames
  if(shiny::isTruthy(peak_df) && nrow(peak_df)) {
    peak_df <- dplyr::filter(peak_df, 
                              .data$qtl_chr == chr_id)
    if(shiny::isTruthy(local)) {
      peak_df <- dplyr::filter(peak_df, 
                                .data$qtl_pos >= peak_Mbp - window_Mbp,
                                .data$qtl_pos <= peak_Mbp + window_Mbp)
    }
    phenames <- dplyr::distinct(
      dplyr::arrange(
        peak_df,
        dplyr::desc(.data$qtl_lod)),
      .data$phenotype)$phenotype
  }

  if("all" %in% selected)
    selected <- c(selected[!(selected %in% c("all","none"))],
                  phenames)
  if("none" %in% selected)
    selected <- ""
  if(!is.null(selected)) {
    selected <- sort(unique(selected))
    selected <- selected[selected %in% phenames]
  }
  
  ## Update phenames to include selected (but not "")
  phenames <- unique(c(selected, phenames))
  phenames <- phenames[phenames != ""]
  # Limit to first 1000
  nphe <- length(phenames)
  phenames <- phenames[seq_len(min(1000, nphe))]
  
  choices <- c("all","none", phenames)
  label = ifelse(nphe <= 1000,
                 "Choose phenotypes",
                 paste("Top 1000 of", nphe))
  list(label = label, choices = choices, selected = selected)
}