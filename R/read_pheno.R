#' Read Phenotypes
#'
#' @param project_df dataframe of project information
#' @param class name of class
#' @param columns name of columns from phenotype file
#' @param legacy use legacy data if `TRUE`
#'
#' @returns dataframe
#' @export
#' @importFrom tibble column_to_rownames
read_pheno <- function(project_df, class, columns = NULL, legacy = FALSE) {
  out <- list()
  for(classi in class) {
    # Code for multiple classes. Watch out for columns.
    # Read phenotype data from project.
    dataname <- paste0("pheno_", classi)
    # `read_project` with filetype `fst` can use `columns` argument.
    result <- read_project(project_df, dataname, "pheno", legacy = legacy)
    if(is.null(result)) return(NULL)
    out[[classi]] <- result
  }
  
  # Merge multiple phenotypes by `row.names`  
  if(length(out) > 1) {
    outs <- as.data.frame(out[[1]])
    for(i in seq(2, length(out))) {
      # Apparently, `merge` adds column `Row.names` to result.
      outs <- merge(outs, as.data.frame(out[[i]]), by = "row.names")
      # This has to be moved back to be the actual `row.names`.
      outs <- tibble::column_to_rownames(outs, "Row.names")
    }
    out <- as.matrix(outs)
  } else {
    out <- out[[1]]
  }
  out <- as.matrix(out)
  
  # Assume for now `columns` are unique across `class`
  # Need to check if colnames collide. If so, use `class`.
  if(!is.null(columns)) {
    m <- match(columns, colnames(out), nomatch = 0)
    if(all(m == 0)) return(NULL)
    out <- out[, m, drop = FALSE]
  }
  out
}