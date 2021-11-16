ageRange <- getPopulationByAgeRange()

colorRed = "#d14956"
colorBlue= "#466a99"

populationAgeRangeWavePlot <- function(currentYear) {
  ageRangeGender <- ageRange %>% 
    filter(Gender %in% c("Women", "Men"), MetricCode %in% c("LESS_20", "20-64", "65_OVER")) %>%
    group_by(Year, MetricCode) %>%
    summarise(n = sum(Value)) %>%
    mutate(percentage = n / sum(n))
  
  ageRangeGender$MetricCode <- factor(ageRangeGender$MetricCode, levels=c("LESS_20", "20-64", "65_OVER"))
  
  plot <- ggplot(ageRangeGender, aes(x=Year, y=percentage, fill=MetricCode)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() 
  return(ggplotly(plot))
}
