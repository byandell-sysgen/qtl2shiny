# used in snpList, snpProb, allele
# SNP Scan
# SNP Probs
# Scan Patterns
# Effect Scans
# Allele Plots
appProgress <- function(prog_msg, diag_msg, tasks, ...) {
  if(exists("qtldebug")) {
    message(paste(prog_msg, diag_msg))
    if(qtldebug > 1) browser()    
  }
  shiny::withProgress(message = paste(prog_msg, "..."), value = 0, tasks)
}