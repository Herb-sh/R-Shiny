dependencyRateForecastReady <- function(input, output, session, clicks) {
  output$populationAgeForecast = renderPlot({
    populationAgeForecast()
  })
}