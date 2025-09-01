#' Read Peaks for Phenotypes by Class
#'
#' @param class name of class
#' @param legacy use legacy data if `TRUE`
#' @param project_df dataframe of project information
#' @param ... additional arguments
#'
#' @returns dataframe of peaks
#' @export
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tidyr separate_wider_delim
read_peaks <- function(project_df, class = NULL, subject_model = NULL, legacy = FALSE) {
  if(is.null(class)) return(NULL)
  if(is.null(subject_model)) subject_model <- "all_mice_additive"
  classes <- rep(class, rep(length(subject_model), length(class)))
  subjmod <- rep(subject_model, length(class))
  out <- list()
  for(i in seq_along(classes)) {
    dataname <- peak_class(project_df, classes[i], subjmod[i])
    result <- read_project(project_df, dataname, "peaks", legacy = legacy)
    if(is.null(result)) return(NULL)
    # remove Which_mice column if present
    if("Which_mice" %in% names(result)) result[["Which_mice"]] <- NULL
    # `result$phenotype_class` should agree with `class` but often does not.
    # Fix this here:
    result$phenotype_class <- classes[i]
    out[[i]] <- dplyr::mutate(result, subject_model = subjmod[i])
  }
  out <- dplyr::bind_rows(out) |>
    tidyr::separate_wider_delim(subject_model, "_mice_", 
                                names = c("subject", "model"))
}
peak_class <- function(project_df, 
  class_name = NULL,
  subject_model = "all_mice_additive") {
  
  if(is.null(class_name) | is.null(subject_model)) {
    stop("must supply a valid class name and subject_model")
  }
  paste(project_df$project, class_name, subject_model, "peaks", sep = "_")
}
# deprecated
read_peak_class <- function(project_df, 
                            class_name = NULL,
                            ...) {
  peak_class_file <- paste0(peak_class(project_df, class_name, ...), ".rds")
  
  out <- readRDS(file.path(paste(rev(project_df), collapse = "/"), "peaks",
                           peak_class_file))
  if("Which_mice" %in% names(out))
    out <- dplyr::select(out, -Which_mice)
  out
}
#' Create Peaks Directory with Phenotypes by Class
#'
#' @param project_df data frame with project information
#' @param force force creation if `TRUE`
#'
#' @returns character list of created RDS files
#' @export
#' @importFrom dplyr mutate rename select
#' @rdname read_peaks
create_peak_class <- function(project_df, force = FALSE) {
  peaksdir <- file.path(paste(rev(project_df), collapse="/"), "peaks")
  if(dir.exists(peaksdir) & !force) {
    warning("Directory peaks already exists")
    return(dir(peaksdir))
  }
  
  # Get old `analyses` to extract `pheno` and `addcovar` information.
  analyses_old <- read_project(project_df, "analyses")
  wh <- grep("sex", names(analyses_old))
  (covar_cols <- names(analyses_old)[- seq_len(wh - 1)])
  addcovar_ch <- apply(analyses_old[,covar_cols], 1,
    function(x) paste0("~", paste0(covar_cols[x], collapse = "+")))

  # Rename peaks columns and set up `addcovar` and `intcovar`.
  peaks_old <- read_project(project_df, "peaks")
  peaks_ref <- peaks_old |>
    dplyr::select(pheno_group, pheno, lod, chr, pos) |>
    dplyr::rename(phenotype_class = "pheno_group",
                  phenotype = "pheno",
                  qtl_lod = "lod",
                  qtl_chr = "chr",
                  qtl_pos = "pos") |>
    dplyr::mutate(addcovar = addcovar_ch[match(phenotype, analyses_old$pheno)],
                  intcovar = "none")
  peaks_ref <- split(peaks_ref, peaks_ref$phenotype_class)
  # Create and populate peaks directory.
  dir.create(peaksdir)
  for(i in names(peaks_ref))
    saveRDS(peaks_ref[[i]], 
            file.path(peaksdir, paste0(project_df$project, "_", i,
                                       "_all_mice_additive_peaks.rds")))
  return(dir(peaksdir))
}
