#' Project Classes
#'
#' Find names of project classes using phenotype files.
#' 
#' @param project_df 
#'
#' @returns character list of classes
#' @export
#' @importFrom stringr str_remove
project_classes <- function(project_df) {
  classes <- stringr::str_remove(
    stringr::str_remove(project_phenos(project_df), "^pheno_"),
    ".rds$")
  classes[!(classes %in% c("data","type"))]
}
project_phenos <- function(project_df) {
  list.files(
    paste0(rev(project_df), collapse="/"),
    pattern = "^pheno_.*.rds$")
}
