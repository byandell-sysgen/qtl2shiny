#' Read Peaks for Phenotypes by Class
#'
#' @param class name of class
#' @param legacy use legacy data if `TRUE`
#' @param project_df dataframe of project information
#' @param ... additional arguments
#'
#' @returns dataframe of peaks
#' @export
#' @importFrom dplyr select
read_peaks <- function(project_df, class = NULL, legacy = FALSE, ...) {
  if(length(class) > 1) {
    message("only using first class of ", length(class))
  }
  dataname <- peak_class(project_df, class[1], ...)
  read_project(project_df, dataname, "peaks", legacy = legacy)
  ## Here add code for multiple classes
}
peak_class <- function(project_df, 
  class_name = NULL,
  subject_model = NULL,
  ...) {
  
  if(is.null(class_name)) {
    stop("must supply a valid class name")
  }
  # Some names changed between pheno and peaks databases for project.
  if("liver_psi" %in% class_name) 
    class_name[match("liver_psi", class_name)] <- "liver_splice_juncs"
  if(any(c("plasma_metabolites_13C", "plasma_metabolites_2H") %in% class_name))
    class_name[match(c("plasma_metabolites_13C", "plasma_metabolites_2H"),
                         class_name, nomatch = 0)] <- "plasma_metabolites"
  
  if(is.null(subject_model)) subject_model <- "all_mice_additive"

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
