#' Project Classes
#'
#' Find names of project classes using phenotype files.
#' 
#' @param project_df dataframe with project information
#' @param project_class character string with project class
#'
#' @returns character list of classes
#' @export
#' @importFrom stringr str_remove
#' @importFrom dplyr distinct filter
#' @importFrom tidyr unite
project_classes <- function(project_df) {
  classes <- stringr::str_remove(
    stringr::str_remove(project_phenos(project_df), "^pheno_"),
    ".rds$")
  classes[!(classes %in% c("data","type"))]
}
project_phenos <- function(project_df) {
  list.files(
    file.path(paste0(rev(project_df), collapse="/"), "pheno"),
    pattern = "^pheno_.*.rds$")
}
#' @export
#' @rdname project_classes
project_subject_model <- function(project_df, project_class) {
  tidyr::unite(
    dplyr::distinct(
      dplyr::filter(project_peaks(project_df),
                    .data$class %in% project_class),
      subjects, covars),
    subject_model)$subject_model
}