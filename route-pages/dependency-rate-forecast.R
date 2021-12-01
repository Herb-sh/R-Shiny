dependencyRateForecastReady <- function(input, output, session, clicks) {
  #rv <- reactiveValues(metric = '')
                        
  #output$populationAgeForecast = renderPlot({
  #  populationAgeForecast(rv$metric)
  #})
  
  observeEvent(input$metricChange, {
    output$populationAgeForecast = renderPlot({
      populationAgeForecast(input$metricChange)
    })
    #rv$metric = input$metricChange
  })
}