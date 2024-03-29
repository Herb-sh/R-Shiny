# Authors: Herbi Shtini & Anitta Weiss
ageRange <- getPopulationByAgeRange()


colorRed = "#d14956"
colorBlue= "#466a99"


populationAgeRangePlot = function(displayYear) {

  ageRangeYear <- ageRange %>% 
    filter(Year == displayYear, Gender %in% c("Women", "Men"), MetricCode %in% c("LESS_20", "20-64", "65_OVER")) %>%
    group_by(Value, Gender)
  
  ageRangeYear$MetricCode <- factor(ageRangeYear$MetricCode, levels=c("LESS_20", "20-64", "65_OVER"))
  
  plot <- ggplot(ageRangeYear, aes(x=MetricCode, y=Value, fill = factor(Gender))) +
    geom_bar(alpha=0.8, stat = "identity") +
    scale_fill_manual(name="Geschlecht", 
                      values = c(colorBlue, colorRed), 
                      breaks = c("Men", "Women"),
                      labels = c("Man", "Frau")) +
    labs(x = "Altersspanne", y = "Bevölkerungsnummer") +
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