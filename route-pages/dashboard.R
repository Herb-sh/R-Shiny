dashboardReady <- function(input, output, session, clicks) {
  totalPopulation <- getPopulationByAgeGroup()
  dependencyRate <- getDependencyRate()
  
  lastYear = 2020
  
  # Population
  totalPopulationLast <- totalPopulation %>% filter(Gender=="Total", Year == lastYear) %>% select(Value) %>% sum()
  totalPopulationPrev <- totalPopulation %>% filter(Gender=="Total", Year == lastYear-1) %>% select(Value) %>% sum()
  
  output$totalPopulation = renderText({
    paste(format(round(totalPopulationLast/1000000, 2), decimal.mark=","), "m", sep="")
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
    round(totalDependencyRate, 3)
  })
  
  print(totalDependencyRate)
  print(totalDependencyRatePrev)
  output$increaseDR = renderText({
    round((totalDependencyRate - totalDependencyRatePrev), 4)
  })
}