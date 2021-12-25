startYear <- 1990
endYear <- 2020

populationReady <- function(input, output, session, clicks) {
  rvg <- reactiveValues(currentYear = startYear,
                       timer = function() {return(0)},
                       ageGroupPlot = populationAgeGroupPlot(startYear))
  
  observe({
    timer <- rvg$timer()
    
    if (isolate(rvg$currentYear) && isolate(rvg$currentYear) < endYear && timer != 0) {
      rvg$currentYear = isolate(rvg$currentYear)+1
      
      if(rvg$currentYear == endYear) {
        rvg$timer = reactiveTimer(Inf)
      }
    }
  })
  
  output$populationAgeGroup = renderPlotly({
    populationAgeGroupPlot(rvg$currentYear)
  })
  
  output$currentYear = renderText({
    rvg$currentYear
  })
  
  observeEvent(input$start, {
    rvg$timer = reactiveTimer(500)
  })
  
  observeEvent(input$pause, {
    rvg$timer = reactiveTimer(Inf)
  })
  
  observeEvent(input$reset, {
    rvg$currentYear = startYear
    rvg$timer = reactiveTimer(Inf)
    rvg$timer = function() {return(0)}
  })
  
  # Redirection
  observeEvent(input$goToPopulationForecast, {
    change_page("/dependency-rate-forecast?population")
  })
}
