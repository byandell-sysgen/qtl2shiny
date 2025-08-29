#' @importFrom dplyr arrange desc distinct filter
#' @importFrom rlang .data
#' 
select_phenames <- function(phenames, peak_df) {
  # See foundrHarmony/R/LivRna.R for compression of gene symbol, id and transcript id.
  selected <- phenames
  if(shiny::isTruthy(peak_df) && nrow(peak_df)) {
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
  if(is.null(selected))
    selected <- phenames[1]
  
  choices <- c("all","none", phenames)
  label <- "Choose phenotypes"
  list(label = label, choices = choices, selected = selected)
}