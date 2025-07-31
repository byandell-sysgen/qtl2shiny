hotspot_wrap <- function(map, peaks, peak_window = 1, minLOD = 5.5,
                         project_df) {
  # This uses global hotspots
  if(peak_window == 1 & minLOD == 5.5 & nrow(project_df)) 
    read_project(project_df, "hotspot")
  else {
    if(shiny::isTruthy(map) && shiny::isTruthy(peaks)) {
      hotspot(map, peaks, peak_window, minLOD)
    } else
      NULL
  }
}