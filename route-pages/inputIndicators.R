inputIndicatorsReady <- function(input, output, session, clicks) {
  output$migration = renderPlotly({
    plotMigration()
  })
  
  output$employmentRate = renderPlot({
    plotEmploymentRate()
  })
}