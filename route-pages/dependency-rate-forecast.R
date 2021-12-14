dependencyRateForecastReady <- function(input, output, session, clicks) {
  observeEvent(input$metricChange, {
    output$populationAgeForecast = renderPlot({
      populationAgeForecast(input$metricChange)
    })
  })
  
  observeEvent(input$dependencyRateType, {
    if(input$dependencyRateType == "dependency-rate-young") {
      output$populationAgeForecast = renderPlot({
        getDependencyRatePlot('young', 'Year', 'Abhängigenquotient', 'green')
      })
    } else if (input$dependencyRateType == "dependency-rate-old") {
      output$populationAgeForecast = renderPlot({
        getDependencyRatePlot('old', 'Year', 'Altenquotient', 'orange')
      })
    } else if (input$dependencyRateType == "dependency-rate-total") {
      output$populationAgeForecast = renderPlot({
        getDependencyRatePlot('total', 'Year', 'Abhängigenquotient', 'blue')
      })
    }
  })
}