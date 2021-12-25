# Authors: Herbi Shtini & Anitta Weiss
migrationForecastReady <- function(input, output, session, clicks) {
  observeEvent(input$metricChange, {
    output$migrationForecast = renderCombineWidgets({
      populationAgeForecast(input$metricChange)
    })
  })
}