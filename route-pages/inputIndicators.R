inputIndicatorsReady <- function(input, output, session, clicks) {
  output$migration = renderPlot({
    plotMigration()
  })
  
  output$employmentRate = renderPlot({
    plotEmploymentRate()
  })
}