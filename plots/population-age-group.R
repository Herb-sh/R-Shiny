library(plotly)

ageGroups <- getPopulationByAgeGroup()

ageGroupsGender <- ageGroups %>% filter(Gender %in% c("Women", "Men"))

populationAgeGroupPlot = function(displayYear) {
  ageGroupsGenderYear <- ageGroupsGender %>% filter(Year == displayYear)
  
  ageGroupsGenderYear$Value <- ifelse(ageGroupsGenderYear$Gender == "Men", -1*ageGroupsGenderYear$Value, ageGroupsGenderYear$Value)
  
  plot <- ggplot(ageGroupsGenderYear, aes(x = Metric, y = Value, fill = Gender)) +
    geom_bar(data = subset(ageGroupsGenderYear, Gender == "Women"), stat = "identity") +
    geom_bar(data = subset(ageGroupsGenderYear, Gender == "Men"), stat = "identity") +
    scale_y_continuous(labels = c("4m", "2m", "0m", "2m", "4m")) + 
    coord_flip() +
    theme_light() +
    theme(
      panel.border = element_blank()
    )
  return(ggplotly(plot))
}