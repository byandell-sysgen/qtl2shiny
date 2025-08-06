#' Read Peaks for Phenotypes by Class
#'
#' @param project_df dataframe
#' @param class_name character string of phenotype class
#' @param subjects character string of subjects used in analysis
#' @param covars character string of covariate selection
#' @param ... additional parameters not used
#'
#' @returns dataframe of peaks
#' @export
#' @importFrom dplyr select
peak_class <- function(project_df, 
  class_name = NULL,
  subjects = c("all_mice", "HC_mice", "HF_mice", "male_mice", "female_mice"),
  covars = c("additive", "diet_interactive", "sex_interactive"),
  ...) {
  
  if(is.null(class_name)) {
    stop("must supply a valid class name")
  }
  # Some names changed between pheno and peaks databases for project.
  if(class_name == "liver_psi") class_name <- "liver_splice_juncs"
  if(class_name %in% c("plasma_metabolites_13C", "plasma_metabolites_2H"))
    class_name <- "plasma_metabolites"
  subjects <- match.arg(subjects)
  covars <- match.arg(covars)
  
  paste(project_df$project, class_name, subjects, covars, "peaks", sep = "_")
}
#' @export
#' @rdname peak_class
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
#' @rdname peak_class
create_peak_class <- function(project_df, force = FALSE) {
  peaksdir <- file.path(paste(rev(project_df), collapse="/"), "peaks")
  if(dir.exists(peaksdir) & !force) {
    warning("Directory peaks already exists")
    return(dir(peaksdir))
  }
  analyses_old <- read_project(project_df, "analyses")
  peaks_old <- read_project(project_df, "peaks")
  peaks_ref <- peaks_old |>
    dplyr::select(pheno_group, pheno, lod, chr, pos) |>
    dplyr::rename(phenotype_class = "pheno_group",
                  phenotype = "pheno",
                  qtl_lod = "lod",
                  qtl_chr = "chr",
                  qtl_pos = "pos") |>
    dplyr::mutate(addcovar = addcovar[match(phenotype, analyses_old$pheno)],
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
