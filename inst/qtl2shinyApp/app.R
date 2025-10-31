## qtl2shiny/inst/qtl2shinyApp/app.R ##

projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
library(qtl2shiny)
ui <- qtl2shinyUI("qtl2shiny")
server <- function(input, output, session) {
  qtl2shinyServer("qtl2shiny", projects_df)
  # Allow reconnect with Shiny Server.
  session$allowReconnect(TRUE)
}
shiny::shinyApp(ui, server)