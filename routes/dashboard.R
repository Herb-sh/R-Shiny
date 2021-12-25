# Authors: Herbi Shtini & Anitta Weiss
dashboardReady <- function(input, output, session, clicks) {
  totalPopulation <- getPopulationByAgeGroup()
  dependencyRate <- getDependencyRate()
  
  lastYear = 2020
  
  # Population
  totalPopulationLast <- totalPopulation %>% filter(Gender=="Total", Year == lastYear) %>% select(Value) %>% sum()
  totalPopulationPrev <- totalPopulation %>% filter(Gender=="Total", Year == lastYear-1) %>% select(Value) %>% sum()
  
  output$totalPopulation = renderText({
    paste(format(round(totalPopulationLast/1000000, 2), decimal.mark=","), "M", sep="")
  })
  
  output$increase = renderText({
    round((totalPopulationLast/totalPopulationPrev - 1), 5)
  })
  
  # Dependency Rate
  totalDependencyRate <- dependencyRate %>% filter(MetricCode == "TOTD20-64", Gender=="Total", Year == lastYear) %>% select(Value)
  totalDependencyRate <- totalDependencyRate[1, "Value"]
  totalDependencyRatePrev <- dependencyRate %>% filter(MetricCode == "TOTD20-64", Gender=="Total", Year == lastYear-1) %>% select(Value)
  totalDependencyRatePrev <- totalDependencyRatePrev[1, "Value"]
  
  output$dependencyRate = renderText({
    round(totalDependencyRate*100, 1)
  })
  
  output$increaseDR = renderText({
    round((totalDependencyRate - totalDependencyRatePrev)*100, 1)
  })
  
  # Old Dependency Rate 
  totalOldDependencyRate <- dependencyRate %>% filter(MetricCode == "OAD15-64", Gender=="Total", Year == lastYear) %>% select(Value)
  totalOldDependencyRate <- totalOldDependencyRate[1, "Value"]
  totalOldDependencyRatePrev <- dependencyRate %>% filter(MetricCode == "OAD15-64", Gender=="Total", Year == lastYear-1) %>% select(Value)
  totalOldDependencyRatePrev <- totalOldDependencyRatePrev[1, "Value"]
  
  output$oldDependencyRate = renderText({
    round(totalOldDependencyRate*100, 1)
  })
  
  output$increaseOldDR = renderText({
    round((totalOldDependencyRate - totalOldDependencyRatePrev)*100, 1)
  })
  
  
  observeEvent(input$dashboardPop, {
    change_page("population")
  })
  
  observeEvent(input$dashboardDep, {
    change_page("dependency-rate")
  })
  
  observeEvent(input$dashboardOldDep, {
    change_page("dependency-rate")
  })
  
  observeEvent(input$dashboardMig, {
    change_page("migration")
  })
}