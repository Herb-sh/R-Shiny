startYear <- 1990
endYear <- 2020

dependencyRateReady <- function(input, output, session, clicks) {
  
  rvr <- reactiveValues(currentYearRange = startYear,
                        timer = function() {return(0)},
                        ageRangePlot = populationAgeGroupPlot(startYear))
  observe({
    print(isolate(rvr$currentYearRange))
    timer <- rvr$timer()
    
    if (isolate(rvr$currentYearRange) && isolate(rvr$currentYearRange) < endYear && timer != 0) {
      rvr$currentYearRange = isolate(rvr$currentYearRange)+1
      
      if(rvr$currentYearRange == endYear) {
        rvr$timer = reactiveTimer(Inf)
      }
    }
  })
  
  output$populationAgeRange = renderPlotly({
    populationAgeRangePlot(rvr$currentYearRange)
  })
  
  output$rangeCurrentYear = renderText({
    rvr$currentYearRange
  })
  
  observeEvent(input$startr, {
    rvr$timer = reactiveTimer(500)
  })
  
  observeEvent(input$pauser, {
    rvr$timer = reactiveTimer(Inf)
  })
  
  observeEvent(input$resetr, {
    rvr$currentYearRange = startYear
    rvr$timer = reactiveTimer(Inf)
    rvr$timer = function() {return(0)}
  })
}