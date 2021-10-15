### Import packages
library(rio)
library(tidyverse)
#
options( scipen = 999 )

source("utilities.R")

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
  geom_line(mapping=aes(x=years, y=Insgesamt, group = 1),
            stat="identity") + 
            labs(title="Fertilitätsrate", 
                 subtitle="Nettomigration pro Jahr", 
                 y="Nettomigration",
                 x="Jahre",
                 caption="source: destatis") + 
            theme(axis.text.x = element_text(angle=65, vjust=0.6))
