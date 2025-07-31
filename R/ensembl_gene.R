ensembl_gene <- function(best, project_df, href = FALSE) {
  ensembl_species <- read_project(project_df, "taxa_info")
  if(!is.null(ensembl_species)) {
    ensembl_URL <- file.path("http://www.ensembl.org",
                             ensembl_species,
                             "Gene/Summary?g=")
    tmpfn <- ifelse(href,
                    function(x,y) {
                      if(is.na(x))
                        return("")
                      if(nchar(x))
                        as.character(shiny::a(x, href=paste0(y,x)))
                      else
                        x
                    },
                    function(x,y) {
                      if(is.na(x))
                        return("")
                      if(nchar(x))
                        paste0(y,x)
                      else
                        x
                    })
    best$ensembl_gene <-
      unlist(apply(dplyr::select(best, ensembl_gene),
                   1, tmpfn, ensembl_URL))
  }
  best
}