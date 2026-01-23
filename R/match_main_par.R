match_main_par <- function(hotspot_list, panel, value) {
  # Look for `main_par` element in `hotspot_list`.
  # If missing, return `TRUE`
  # If present, return `TRUE` if the `panel` element matches `value`
  main_par <- hotspot_list$main_par
  if(shiny::isTruthy(main_par)) {
    value <- paste0(value, collapse = "|")
    shiny::isTruthy(grepl(value, main_par[[panel]]))
  } else {
    TRUE
  }
}
