#' Read Query for RDS Object
#' 
#' @param project_df table of project information
#' @param filename name of RDS file
#' 
#' @return data frame read from RDS object.
#' 
#' @importFrom qtl2pattern create_probs_query_func
read_query_rds <- function(project_df, filename) {
  if(is.null(project_df))
    return(NULL)
  datapath <- project_df$directory
  projectfile <- file.path(datapath,
                           project_df$taxa,
                           project_df$project,
                           filename)
  if(!file.exists(projectfile))
    projectfile <- file.path(datapath,
                             project_df$taxa,
                             filename)
  assertthat::assert_that(file.exists(projectfile))
  readRDS(projectfile)
}