startYear <- 1990
endYear <- 2020

populationReady <- function(input, output, session, clicks) {
  rv <- reactiveValues(currentYear = startYear,
                       timer = function() {return(0)},
                       ageGroupPlot = populationAgeGroupPlot(startYear))
  
  observe({
    print(isolate(rv$currentYear))
    timer <- rv$timer()
    
    if (isolate(rv$currentYear) && isolate(rv$currentYear) < endYear && timer != 0) {
      rv$currentYear = isolate(rv$currentYear)+1
      
      if(rv$currentYear == endYear) {
        rv$timer = reactiveTimer(Inf)
      }
    }
  })
  
  output$populationAgeGroup = renderPlotly({
    populationAgeGroupPlot(rv$currentYear)
  })
  
  output$currentYear = renderText({
    rv$currentYear
  })
  
  observeEvent(input$start, {
    rv$timer = reactiveTimer(1000)
  })
  
  observeEvent(input$pause, {
    rv$timer = reactiveTimer(Inf)
  })
  
  observeEvent(input$reset, {
    rv$currentYear = startYear
    rv$timer = reactiveTimer(Inf)
    rv$timer = function() {return(0)}
  })
}
