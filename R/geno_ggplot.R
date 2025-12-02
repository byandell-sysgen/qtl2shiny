geno_ggplot <- function(geno_table, pheno_mx) {
  if(is.null(geno_table) | is.null(pheno_mx)) return(plot_null())
  
  pheno_df <- as.data.frame(pheno_mx, make.names = FALSE)
  pheno_names <- names(pheno_df)
  pheno_df <- tibble::rownames_to_column(pheno_df, var = "subject")
  geno_df <- as.data.frame(geno_table, make.names = FALSE)
  geno_names <- names(geno_df)
  geno_df <- tibble::rownames_to_column(geno_df, var = "subject")
  dat <- dplyr::left_join(pheno_df, geno_df, by = "subject")
  yname <- ifelse(length(pheno_names) > 1, pheno_names[2], geno_names[2])
  # Make sure col and label are factors.
  for( i in 1:2)
    dat[[geno_names[i]]] <- factor(dat[[geno_names[i]]])
  # Group for lm lines.
  dat$group <- dat[[geno_names[2]]]
  p <- ggplot2::ggplot(dat) +
    ggplot2::aes(x = .data[[pheno_names[1]]], y = .data[[yname]],
                 col = .data[[geno_names[2]]],
                 group = .data[[geno_names[2]]],
                 label = .data[[geno_names[1]]]) +
    ggrepel::geom_text_repel(max.overlaps = Inf, min.segment.length = Inf,
                             size = 3, fontface = 2)
  if(length(pheno_names) > 1) {
    p <- p + ggplot2::geom_smooth(ggplot2::aes(group = group),
      se = FALSE, method = "lm", formula = "y ~ x", linewidth = 2, linetype = 2)
  }
  p
}
