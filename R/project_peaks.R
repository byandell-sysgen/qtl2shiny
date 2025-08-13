project_peaks <- function(project_df) {
  filenames <-
    list.files(file.path(paste0(rev(project_df), collapse="/"), "peaks"))
  subject_names <- c("all_mice", "HC_mice", "HF_mice", "male_mice", "female_mice")
  covar_names <- c("additive", "diet_interactive", "sex_interactive")
  
  out <- data.frame(
    # Remove `project` and file MIME.
    class = stringr::str_remove(
      stringr::str_remove(filenames,
                          paste0(project_df$project, "_")),
      "_peaks.rds$"))
  out <- dplyr::mutate(out,
                       subjects = stringr::str_extract(.data$class, paste(subject_names, collapse ="|")),
                       covars = stringr::str_extract(.data$class, paste(covar_names, collapse ="|")),
                       # Strip out `subjects` and `covars` from `class`.
                       class = stringr::str_remove(
                         .data$class,
                         paste0("_", .data$subjects, "_", .data$covars)),
                       filename = file.path("peaks", filenames))
  out
}
