if (!require("plotly")) install.packages("plotly"); library(plotly)

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

netMigrationTotalFormat <- netMigrationTotalFormat %>% 
  mutate(color = ifelse(Insgesamt>0, "#3AC0DA", "#eea29a"))

#Plot
plotMigration <- function () {
  plot <- ggplot(netMigrationTotalFormat, 
                 aes(x=years, y=Insgesamt,
                     text=paste("Jahr: ", years, " Bevölkerungszahl: ", Insgesamt))) +
   # geom_line(aes(x=years, y=Insgesamt, group = 1), stat="identity") +
    geom_segment( aes(x=years, xend=years, y=0, yend=Insgesamt, color = color), size=4, alpha=0.9) +
    #geom_segment( aes(x=years, xend=years, y=0, yend=weiblich, color = color), size=1, alpha=0.9) +
    scale_colour_identity() +
    theme_light() +
    labs(y="Bevölkerungszahl",
         x="Jahre",
         caption="source: destatis") +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      axis.text.x = element_text(angle=65, vjust=0.6)
    )
  
  return(ggplotly(plot, tooltip = c("text")))
}