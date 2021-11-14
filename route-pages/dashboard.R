dashboardReady <- function(input, output, session, clicks) {
  totalPopulation <- getPopulationByAgeGroup()
  
  lastYear = 2020
  totalPopulationLast <- totalPopulation %>% filter(Gender=="Total", Year == lastYear) %>% select(Value) %>% sum()
  totalPopulationPrev <- totalPopulation %>% filter(Gender=="Total", Year == lastYear-1) %>% select(Value) %>% sum()
  
  increase = totalPopulationLast/totalPopulationPrev - 1
  
  output$increase = renderText({
    increase
  })
  
  output$totalPopulation = renderText({
    totalPopulation2020
  })
}