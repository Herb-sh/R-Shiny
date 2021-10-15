### Import packages
library(rio)
library(tidyverse)
#
options( scipen = 999 )

source("utilities.R")
#
# migration <- read.csv2("data/germany-migration.csv",
#                       skip=3, header = TRUE, stringsAsFactors = FALSE)

migration <- import("data/migration.csv", 
                    encoding = "UTF-8", 
                    quote="")

# Getting only a part of the whole document
netMigrationTotal <- migration[c(5, 8:28), c(1, 8:10)]

#Cleaning
netMigrationTotalFormat <- setXYHeaders(netMigrationTotal)
# Transforming values from char to number
netMigrationTotalFormat$Insgesamt <- as.numeric(as.character(netMigrationTotalFormat$Insgesamt))

#Plot
ggplot(netMigrationTotalFormat) +
  geom_line(mapping=aes(x="Erwerbspersonen (15 Jahre und alter)", y=Insgesamt, group = 1),
       stat="identity") + 
       labs(title="Line chart", 
       subtitle="Erwerbspersonen (15 Jahre und alter)", 
       caption="source: destatis")
