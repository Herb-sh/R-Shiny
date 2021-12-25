# Authors: Herbi Shtini & Anitta Weiss
dependencyRateForecastReady <- function(input, output, session, clicks) {
  observeEvent(input$metricChange, {
    output$dependencyRateForecast = renderCombineWidgets({
      populationAgeForecast(input$metricChange)
    })
  })
  
  observeEvent(input$populationMetricChange, {
    output$dependencyRateForecast = renderCombineWidgets({
      populationAgeForecast(input$populationMetricChange)
    })
  })
  
  observeEvent(input$dependencyRateType, {
    if(input$dependencyRateType == "dependency-rate-young") {
      output$dependencyRateForecast = renderCombineWidgets({
        getDependencyRatePlot('young', 'Year', 'Abhängigenquotient', 'green')
      })
    } else if (input$dependencyRateType == "dependency-rate-old") {
      output$dependencyRateForecast = renderCombineWidgets({
        getDependencyRatePlot('old', 'Year', 'Altenquotient', 'orange')
      })
    } else if (input$dependencyRateType == "dependency-rate-total") {
      output$dependencyRateForecast = renderCombineWidgets({
        getDependencyRatePlot('total', 'Year', 'Abhängigenquotient', 'blue')
      })
    }
  })
}