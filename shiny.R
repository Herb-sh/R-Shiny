library(shiny)
library(plotly)

source('migration.R')
source('employment-rate.R')

ui <- basicPage(
  htmlTemplate("www/index.html")
)

server <- function(input, output) {
 output$migration = renderPlot({
   plotMigration()
  })
 
 output$employmentRate = renderPlot({
   plotEmploymentRate()
 })
}

# Run the app ----
shinyApp(ui = ui, server = server)