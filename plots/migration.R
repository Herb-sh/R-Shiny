### Import packages
library(rio)
library(tidyverse)
#
source("utilities.R")

options( scipen = 999 )

migration <- import("data/migration.csv", 
                    encoding = "UTF-8", 
                    quote="")

# Getting only a part of the whole document
netMigrationTotal <- migration[c(5, 8:28), c(1, 8:10)]

#Cleaning
netMigrationTotalFormat <- setFertilityRateHeaders(netMigrationTotal)
# Transforming values from char to number
netMigrationTotalFormat$Insgesamt <- as.numeric(as.character(netMigrationTotalFormat$Insgesamt))

#Plot
plotMigration <- function () {
  p <- ggplot(netMigrationTotalFormat) +
    geom_line(mapping=aes(x=years, y=Insgesamt, group = 1),
              stat="identity") + 
    labs(y="Bevölkerungszahl",
         x="Jahre",
         caption="source: destatis") + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
  return(p)
}