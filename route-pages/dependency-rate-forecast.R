dependencyRateForecastReady <- function(input, output, session, clicks) {
  observeEvent(input$metricChange, {
    output$populationAgeForecast = renderCombineWidgets({
      populationAgeForecast(input$metricChange)
    })
  })
  
  observeEvent(input$dependencyRateType, {
    if(input$dependencyRateType == "dependency-rate-young") {
      output$populationAgeForecast = renderCombineWidgets({
        getDependencyRatePlot('young', 'Year', 'Abhängigenquotient', 'green')
      })
    } else if (input$dependencyRateType == "dependency-rate-old") {
      output$populationAgeForecast = renderCombineWidgets({
        getDependencyRatePlot('old', 'Year', 'Altenquotient', 'orange')
      })
    } else if (input$dependencyRateType == "dependency-rate-total") {
      output$populationAgeForecast = renderCombineWidgets({
        getDependencyRatePlot('total', 'Year', 'Abhängigenquotient', 'blue')
      })
    }
  })
}