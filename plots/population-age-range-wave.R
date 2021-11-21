ageRange <- getPopulationByAgeRange()

colorRed = "#d14956"
colorBlue= "#466a99"

populationAgeRangeWavePlot <- function(currentYear) {
  ageRangeGender <- ageRange %>% 
    filter(Gender %in% c("Women", "Men"), MetricCode %in% c("LESS_20", "20-64", "65_OVER")) %>%
    group_by(Year, MetricCode) %>%
    summarise(ValueSum = sum(Value)) %>%
    mutate(percentage = (ValueSum / sum(ValueSum)))

  ageRangeGender$MetricCode <- factor(ageRangeGender$MetricCode, levels=c("65_OVER", "20-64", "LESS_20"))
  
  plot <- ggplot(ageRangeGender, aes(x=Year, y=ValueSum, fill=MetricCode)) + 
    geom_area(alpha=0.6 , size=0, colour="white") +
    scale_y_continuous(labels = function(n) {
      paste(format(round(n/1000000, 2), decimal.mark=","), "m", sep="")
      #formatC(n, format="f", big.mark=",", digits=0)
    }) +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() 
  
  return(ggplotly(plot))
}
