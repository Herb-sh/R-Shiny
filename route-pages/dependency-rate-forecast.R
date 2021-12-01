dependencyRateForecastReady <- function(input, output, session, clicks) {
  observeEvent(input$metricChange, {
    output$populationAgeForecast = renderPlot({
      populationAgeForecast(input$metricChange)
    })
  })
  
  observeEvent(input$dependencyRateType, {
    if (input$dependencyRateType == "dependency-rate-old") {
      output$populationAgeForecast = renderPlot({
        OldAgeDepend_funct()
      })
    } else {
      output$populationAgeForecast = renderPlot({
        TotalDepend_funct()
      })
    }
  })
}