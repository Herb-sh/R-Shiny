# migration <- read.csv2("/Users/herbishtini/Documents/R-development/Projekt/data/germany-migration.csv",
#                       skip=3, header = TRUE, stringsAsFactors = FALSE)

migration <- import("/Users/herbishtini/Documents/R-development/Projekt/data/germany-migration.csv", 
                    encoding = "UTF-8", 
                    quote="")

# Getting only a part of the whole document
netMigrationTotal <- migration[c(5, 8:28), c(1, 8:10)]

#Cleaning
netMigrationTotalFormat <- header.true(netMigrationTotal)

# Transforming values from char to number
netMigrationTotalFormat$Insgesamt <- as.numeric(as.character(netMigrationTotalFormat$Insgesamt))

ggplot(netMigrationTotalFormat) +
  geom_line(mapping=aes(x=years, y=Insgesamt, group = 1),
       stat="identity") + 
       labs(title="Bar Chart", 
       subtitle="Net migration per year", 
       caption="source: destatis") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
