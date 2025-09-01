#' Shiny Probability modules
#'
#' Shiny genotype probability access.
#' 
#' @param id identifier for shiny reactive
#' @param win_par,pheno_names,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return Object of class \code{probs}.
#'
#' @export
#' @importFrom qtl2mediate get_snpprobs
#' @importFrom shiny isTruthy moduleServer reactive req setProgress withProgress
probsServer <- function(id, win_par, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns

  probs_obj <- shiny::reactive({
    shiny::req(project_df(), win_par())
    chr_id <- win_par()$chr_id
    shiny::withProgress(message = 'Read probs ...', value = 0, {
      shiny::setProgress(1)
      mid <- win_par()$peak_Mbp
      win <- win_par()$window_Mbp
      start_val <- mid - win
      end_val <- mid + win
browser()
      # Define query_probs function
      query_probs <- read_query_rds(project_df(), "query_probs.rds")
      # Note probs object keeps map with it
      query_probs(chr_id, start_val, end_val)
    })
  })
  
  probs_obj
})
}
#' @rdname probsServer
#' @export
pairProbsServer <- function(id, win_par, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns

  ## Probs object for allele pair diplotypes.
  probs_obj <- shiny::reactive({
    shiny::req(project_df(), win_par())
    chr_id <- win_par()$chr_id
    range_val <- win_par()$peak_Mbp + c(-1,1) * win_par()$window_Mbp
    shiny::withProgress(message = 'Diplotype Probs ...', value = 0, {
      shiny::setProgress(1)
      
      # Define query_probs function
      query_probs <- read_query_rds(project_df(), "query_probs.rds")
      query_probs(chr_id, range_val[1], range_val[2],
                         allele = FALSE)
    })
  })
  probs_obj
})
}
#' @rdname probsServer
#' @export
snpProbsServer <- function(id, win_par, pheno_names, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  shiny::reactive({
    shiny::req(project_df(), win_par())
    chr_id <- win_par()$chr_id 
    peak_Mbp <- win_par()$peak_Mbp 
    window_Mbp <- win_par()$window_Mbp
    shiny::req(pheno_names())
    shiny::withProgress(message = 'SNP Probs ...', value = 0, {
      shiny::setProgress(1)

      # Define query_probs function
      query_probs <- read_query_rds(project_df(), "query_probs.rds")
      probs_obj <- query_probs(chr_id,
                               peak_Mbp - window_Mbp,
                               peak_Mbp + window_Mbp,
                               allele = FALSE)
      
      # define the query_variants function
      query_variants <- read_query_rds(project_df(), "query_variants.rds")
      snpinfo <- query_variants(chr_id,
                                peak_Mbp - window_Mbp,
                                peak_Mbp + window_Mbp)
      qtl2mediate::get_snpprobs(chr_id, peak_Mbp, window_Mbp,
                                pheno_names(), 
                                probs_obj$probs,
                                probs_obj$map,
                                snpinfo)
    })
  })
})
}