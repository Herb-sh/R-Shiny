### Import packages
library(rio)
library(tidyverse)
#
options( scipen = 999 )

source("utilities.R")

employment <- import("data/employment-rate.csv", 
                    encoding = "UTF-8", 
                    quote="")

# Getting only a part of the whole document
employmenClean <- employment[c(3, 4), c(2, 4:34)]

#Cleaning
employmenCleanFormat <- setXYHeaders(employmenClean)

#


#
ggplot(employmenCleanFormat) +
geom_line(mapping=aes(y="Erwerbspersonen (15 Jahre und alter)", group = 1))
