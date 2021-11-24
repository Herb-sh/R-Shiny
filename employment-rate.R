### Import packages
library(rio)
library(tidyverse)
#
source("utilities.R")

employment <- import("data/employment-rate.csv", 
                    encoding = "UTF-8", 
                    quote="")

#Getting only a part of the whole document
employmenClean <- employment[c(3, 4), c(2, 4:34)]

#Cleaning
employmenCleanFormat <- setLabHeaders(employmenClean, -1, -1)
employmenCleanFormat <- employmenCleanFormat %>% gather(Key, Value)

#Plot
plotEmploymentRate <- function () {
  p <- ggplot(employmenCleanFormat) +
       geom_line(mapping=aes(y=Value, x=Key), group=1) +
       labs(x="Jahre", y= "Personen 1=1000") +
       theme(axis.text.x = element_text(angle=65, vjust=0.6))
  return(p)
}
