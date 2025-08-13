#' Read Project Data
#' 
#' @param project_df table of project information
#' @param dataname name of data object to read
#' @param columns columns to select from data object
#' @param rownames row names to filter from data object (all if \code{TRUE})
#' @param ... additional parameters
#' @param filetype type of file (one of c("fst","rds","csv"))
#' @param legacy look in `legacy` folder for old data if `TRUE`
#' 
#' @return data frame with \code{columns} and \code{rownames}.
#' 
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom qtl2pattern read_fast
#' @importFrom utils read.csv
#' @export
read_project <- function(project_df, dataname, columns = NULL, rownames = TRUE,
                         ...,
                         filetype, legacy = FALSE) {
  # Read project data frame or matrix in some file format.
  
  if(!nrow(project_df))
    return(NULL)
  
  # Taxa and project paths.
  taxapath <- file.path(project_df$directory,
                        project_df$taxa)
  projectpath <- file.path(taxapath,
                           project_df$project)
  if(legacy)
    projectpath <- file.path(projectpath, "legacy")
  
  # Peaks uses `columns` to get file from `peaks` folder.
  if(!is.null(columns) & dataname == "peaks") {
    projectpath <- file.path(projectpath, "peaks")
    dataname <- columns
  }
  
  # Compare file roots in project path to dataname.
  exact <- TRUE
  match_filename <- function(dataname, filepath, exact) {
    filenames <- list.files(filepath)
    fileroots <- tools::file_path_sans_ext(filenames)
    m <- if(exact) {
      grep(paste(paste0("^", dataname, "$"), collapse = "|"),
           fileroots)
    } else {
      grep(paste(dataname, collapse = "|"), fileroots)
    }
    if(length(m) > 1)
      message("multiple ", dataname, " matches")
    if(length(m))
      file.path(filepath, filenames[m])
    else
      NULL
  }
  
  datapath <- match_filename(dataname, projectpath, exact)
  if(is.null(datapath))
    datapath <- match_filename(dataname, taxapath, exact)
  if(is.null(datapath))
    return(NULL)
  if(length(datapath) > 1) {
    message("only using first datapath of ", length(datapath))
    datapath <- datapath[1]
  }
  # File type in order of preference. First use filetype if supplied.
  # Then use extensions of datapath.
  # Watch out for CSV, as may need to preserve column characteristics.
  filetypes <- c("fst","rds","csv")
  datatypes <- tools::file_ext(datapath)
  if(missing(filetype)) {
    # pick in order of filetypes
    m <- match(filetypes, datatypes, nomatch = 0)
    if(!any(m > 0)) {
      cat(paste("file type", paste(datatypes, collapse = ","), "not in approved list"),
          file = stderr())
      return(NULL)
    }
    m <- m[m>0][1]
    filetype <- datatypes[m]
  } else {
    filetype <- match.arg(filetype, filetypes)
    if(!(filetype %in% datatypes)) {
      cat(paste("file type", filetype, "not in directory"),
          file = stderr())
      return(NULL)
    }
  }
  
  # If more that one in datapath, pick by order of filetypes.
  m <- match(filetype, datatypes)
  # ** multiple datanames reduces to 1 here.
  datapath <- datapath[m]

  # Read data by MIME type.  
  switch(filetype,
    fst     = qtl2pattern::read_fast(datapath, columns, rownames),
    rds     = readRDS(datapath),
    csv     = utils::read.csv(datapath, stringsAsFactors = FALSE))
}

