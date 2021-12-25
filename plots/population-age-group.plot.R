# Authors: Herbi Shtini & Anitta Weiss
colorRed = "#d14956"
colorBlue= "#466a99"

ageGroups <- getPopulationByAgeGroup()

ageGroupsGender <- ageGroups %>% filter(Gender %in% c("Women", "Men"))

populationAgeGroupPlot = function(displayYear) {
  ageGroupsGenderYear <- ageGroupsGender %>% filter(Year == displayYear)
  
  ageGroupsGenderYear$Value <- ifelse(ageGroupsGenderYear$Gender == "Men", -1*ageGroupsGenderYear$Value, ageGroupsGenderYear$Value)
  
  plot <- ggplot(ageGroupsGenderYear, aes(x = MetricCode, xlab = Metric, y = Value, fill = Gender)) +
    geom_bar(data = subset(ageGroupsGenderYear, Gender == "Women"), stat = "identity") +
    geom_bar(data = subset(ageGroupsGenderYear, Gender == "Men"), stat = "identity") +
    scale_fill_manual(name="Geschlecht", 
                      values = c(colorBlue, colorRed), 
                      breaks = c("Men", "Women"),
                      labels = c("Man", "Frau")) +
    scale_x_discrete(breaks=c("0_4", "05_9", "10_14", "15_19", "20_24", "25_29", 
                              "30_34", "35_39", "40_44", "45_49", "50_54", "55_59",
                              "60_64", "65_69", "70_74", "75_79", "80_84", "85_OVER"),
                     labels=c("0 bis 4", "5 bis 9", "10 bis 14", "15 bis 19", "20 bis 24", "25 bis 29", 
                              "30 bis 34", "35 bis 39", "40 bis 44", "45 bis 49", "50 bis 54", "55 bis 59",
                              "60 bis 64", "65 bis 69", "70 bis 74", "75 bis 79", "80 bis 84", "Über 85")) +
    scale_y_continuous(labels = c("4m", "2m", "0m", "2m", "4m")) + 
    # gganimate specific bits:
    transition_states(
      frame,
      transition_length = 2,
      state_length = 1
    ) +
    ease_aes('sine-in-out') +
    coord_flip() +
    theme_light() +
    theme(
      panel.border = element_blank()
    )
  return(ggplotly(plot))
}