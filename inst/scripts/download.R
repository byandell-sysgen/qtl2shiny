## Downloads.
filepath <- shiny::reactive({
  shiny::req(win_par())
  file.path(paste0("allele1_", win_par()$chr_id, "_", win_par()$peak_Mbp))
})
download_list <- shiny::reactiveValues(
  filename = shiny::isolate(filepath()),
  Plot = shiny::reactiveValues(
    allele = shiny::reactive({
      ggplot2::autoplot(
        allele_obj(), pos = input$pos_Mbp) +
        ggplot2::ggtitle(colnames(hotspot_list$pheno_mx()))
    })),
  Table = shiny::reactiveValues(
    allele = shiny::reactive({
      tidyr::pivot_wider(
        dplyr::select(
          dplyr::mutate(
            allele_obj(),
            allele = paste(.data$source, .data$allele, sep = ".")),
          -.data$probe, -.data$source),
        names_from = "allele", values_from = "effect")
    }))
)
downloadServer(ns("download"), download_list,
               selected_item = shiny::reactive("allele"),
               plot_table = shiny::reactive(input$plot_table),
               title_download = shiny::reactive("Allele"))
})
## Downloads.
filepath <- shiny::reactive({
  shiny::req(win_par())
  file.path(paste0("allele1_", win_par()$chr_id, "_", win_par()$peak_Mbp))
})
download_list <- shiny::reactiveValues(
  filename = shiny::isolate(filepath()),
  Plot = shiny::reactiveValues(
    allele = shiny::reactive({
      ggplot2::autoplot(
        allele_obj(), pos = input$pos_Mbp) +
        ggplot2::ggtitle(colnames(hotspot_list$pheno_mx()))
    })),
  Table = shiny::reactiveValues(
    allele = shiny::reactive({
      tidyr::pivot_wider(
        dplyr::select(
          dplyr::mutate(
            allele_obj(),
            allele = paste(.data$source, .data$allele, sep = ".")),
          -.data$probe, -.data$source),
        names_from = "allele", values_from = "effect")
    }))
)
downloadServer(ns("download"), download_list,
               selected_item = shiny::reactive("allele"),
               plot_table = shiny::reactive(input$plot_table),
               title_download = shiny::reactive("Allele"))
})
