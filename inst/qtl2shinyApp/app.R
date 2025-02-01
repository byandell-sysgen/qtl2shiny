## qtl2shiny/inst/qtl2shinyApp/app.R ##

projects <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
ui <- shinydashboard::dashboardPage(skin="red",
  shinydashboard::dashboardHeader(title = "qtl2shiny"),
  shinydashboard::dashboardSidebar(qtl2shiny::mainInput("qtl2shiny")),
  shinydashboard::dashboardBody(qtl2shiny::mainOutput("qtl2shiny"))
)
server <- function(input, output, session) {
  qtl2shiny::mainServer("qtl2shiny", projects)
  # Allow reconnect with Shiny Server.
  session$allowReconnect(TRUE)
}
shiny::shinyApp(ui, server)