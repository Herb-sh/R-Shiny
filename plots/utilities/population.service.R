### Import packages
library(rio)
library(tidyverse)
populationFile <- import("data/historical_population.csv", 
                    encoding = "UTF-8", 
                    #quote="",
                    sep=",")

excelPath <- 'data/population.xlsx'
excelNames <- excel_sheets(excelPath)
list_all <- lapply(excelNames, function(x) {as.data.frame(read_excel(path= excelPath, sheet=x))})

population <- populationFile %>%
  select(Gender, MetricCode, Metric, Year, Value)

# Population Growth
getPopulationGrowth <- function() {
   return(population %>% filter(MetricCode == "POP_GR"))
}

# Dependency rate TOTD20-64  OAD15-64
getDependencyRate <- function() {
  return(population %>% filter(MetricCode %in% c("TOTD20-64", "OAD15-64")))
}

getDependencyRatioByYear <- function(year) {
   depRatioRow <- (population %>% filter(MetricCode %in% c("TOTD20-64"), Year == year, Gender == "Total"))
  return(depRatioRow$Value)
}

# Share of Population by Age Range
getPopulationShareByAgeRange <- function() {
  return(population %>% filter(MetricCode %in% c("LESS_15_SHARE", "15-24_SHARE", "15-64_SHARE", "65_OVER_SHARE")))
}

# Get Population Age Range
getPopulationByAgeRange <- function() {
  return(population %>% filter(MetricCode %in% c("LESS_20", "15-64", "20-64", "50_OVER", "65_OVER")))
}  
  
# Get Population Age Group
getPopulationByAgeGroup <- function() {
  return(population %>% filter(MetricCode %in% c("0_4", "05_9", "10_14", "15_19", "20_24", "25_29", 
                                                 "30_34", "35_39", "40_44", "45_49", "50_54", "55_59",
                                                 "60_64", "65_69", "70_74", "75_79", "80_84", "85_OVER")))
}
