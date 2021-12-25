# Authors: Anitta Weiss & Herbi Shtini
populationForecastReady <- function(input, output, session, clicks) {
  observeEvent(input$metricChange, {
    output$populationForecast = renderCombineWidgets({
      populationAgeForecast(input$metricChange)
    })
  })
}