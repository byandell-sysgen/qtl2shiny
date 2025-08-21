#' Read Phenotypes
#'
#' @param project_df dataframe of project information
#' @param class name of class
#' @param columns name of columns from phenotype file
#' @param legacy use legacy data if `TRUE`
#'
#' @returns dataframe
#' @export
read_pheno <- function(project_df, class, columns = NULL, legacy = FALSE) {
  if(length(class) > 1) {
    message("testing multiple classes for ", length(class))
  }
  if(!is.null(columns)) {
    # assume columns is list, or make it so.
    columns <- as.list(columns)
    if(length(columns) != length(class)) {
      message("columns must be list same length as class",
              length(class), length(columns))
      return(NULL)
    }
    names(columns) <- classi
  }
  out <- list()
  for(classi in class) {
    # Code for multiple classes. Watch out for columns.
    # Read phenotype data from project.
    dataname <- paste0("pheno_", classi)
    out[[classi]] <- read_project(project_df, dataname, "pheno",
                        columns = columns, legacy = legacy)
    if(!is.null(columns[[classi]])) {
      if(!all(columns[[classi]] %in% colnames(out[[classi]]))) return(NULL)
      # Pick columns post hoc.
      out[[classi]] <- out[[classi]][, columns[[classi]], drop = FALSE]
    }
  }
  if(length(out) > 1) {
    # Need to check if colnames collide. If so, use `class`.
    outs <- as.data.frame(out[[1]])
    for(i in seq(2, length(out))) {
      outs <- merge(outs, as.data.frame(out[[i]]), by = "row.names")
    }
    out <- as.matrix(outs)
  } else {
    out <- out[[1]]
  }
  out
}