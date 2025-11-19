pair_geno_table <- function(pairprobs_obj, patterns, pos_Mbp) {
  # Create table of allele_pair and patterns by subject.
  map <- pairprobs_obj$map[[1]]
  wh_map <- which.min(abs(map - pos_Mbp))[1]
  pairprobs <- pairprobs_obj$probs
  
  patterns <- dplyr::distinct(patterns, pattern, sdp)
  
  # Set up output matrix.
  out <- matrix("", nrow(pairprobs[[1]]), 1 + nrow(patterns),
                dimnames = list(dimnames(pairprobs[[1]])[[1]],
                                c("allele_pair", patterns$pattern)))
  
  # Assign allele pair based on max genoprob.
  out[,1] <- dimnames(pairprobs[[1]])[[2]][
    apply(pairprobs[[1]][,, wh_map], 1, function(x) which.max(x)[1])]
  
  # Collapse allele_pair genoprobs to patternprob.
  sdps <- patterns$sdp
  for(i in seq_along(sdps)) {
    sdp_probs <- qtl2pattern::genoprob_to_patternprob(pairprobs, sdps[i])[[1]]
    out[, 1 + i] <- c("ref","het","alt")[
      apply(sdp_probs[,, wh_map], 1, function(x) which.max(x)[1])]
  }
  out
}
