library(plotly)

ageRange <- getPopulationByAgeRange()


populationAgeRangePlot = function(displayYear) {
  
  ageRangeYear <- ageRange %>% 
    filter(Year == displayYear, Gender == "Total", MetricCode %in% c("LESS_20", "20-64", "65_OVER"))
  
  ageRangeYear$MetricCode <- factor(ageRangeYear$MetricCode, levels=c("LESS_20", "20-64", "65_OVER"))
  
  plot <- ggplot(ageRangeYear, 
                 aes(x=MetricCode, y=Value)) +
    geom_bar(fill="#69b3a2", color="#e9ecef", alpha=0.8, stat = "identity") +
    labs(x = "Altersspanne", 
         y = "Bevölkerungsnummer") +
    scale_x_discrete(labels = c("Unter 20", "20-64", "Über 65")) +
    scale_y_continuous(labels = function(n) {
      formatC(n, format="f", big.mark=",", digits=0)
    }) +
    theme_light() +
    theme(
      panel.border = element_blank()
    )
  
  return(ggplotly(plot))
}