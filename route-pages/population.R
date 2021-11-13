startYear <- 1990
endYear <- 2020

populationReady <- function(input, output, session, clicks) {
  rvg <- reactiveValues(currentYear = startYear,
                       timer = function() {return(0)},
                       ageGroupPlot = populationAgeGroupPlot(startYear))
  
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
  
  observe({
    print(isolate(rvg$currentYear))
    timer <- rvg$timer()
    
    if (isolate(rvg$currentYear) && isolate(rvg$currentYear) < endYear && timer != 0) {
      rvg$currentYear = isolate(rvg$currentYear)+1
      
      if(rvg$currentYear == endYear) {
        rvg$timer = reactiveTimer(Inf)
      }
    }
  })
  
  output$populationAgeRange = renderPlotly({
    populationAgeRangePlot(rvr$currentYearRange)
  })
  
  output$populationAgeGroup = renderPlotly({
    populationAgeGroupPlot(rvg$currentYear)
  })
  
  output$currentYear = renderText({
    rvg$currentYear
  })
  
  output$rangeCurrentYear = renderText({
    rvr$currentYearRange
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
