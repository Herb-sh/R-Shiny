dependencyRateForecastReady <- function(input, output, session, clicks) {
  output$populationAgeForecast = renderPlotly({
    populationAgeForecast()
  })
}