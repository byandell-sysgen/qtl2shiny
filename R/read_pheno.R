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
    message("only using first class of ", length(class))
  }
  # Read phenotype data from project.
  dataname <- paste0("pheno_", class[1])
  out <- read_project(project_df, dataname, "pheno",
                      columns = columns, legacy = legacy)
  if(!is.null(columns)) {
    if(!all(columns %in% colnames(out))) return(NULL)
    # Pick columns post hoc.
    if(is.data.frame(out) | is.matrix(out))
      out <- out[, columns, drop = FALSE]
  }
  out
  # Code for multiple classes. Watch out for columns.
}