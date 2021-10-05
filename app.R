# install.packages("rio")

#library(rio)

# migration <- read.csv2("/Users/herbishtini/Documents/R-development/Projekt/data/germany-migration.csv",
#                       skip=3, header = TRUE, stringsAsFactors = FALSE)
source("/Users/herbishtini/Documents/R-development/Projekt/utilities.R")

if (!migration)
migration <- import("/Users/herbishtini/Documents/R-development/Projekt/data/germany-migration.csv", 
                    encoding = "UTF-8", 
                    quote="")

netMigrationTotal <- migration[c(5, 8:28), c(1, 8:10)]
netMigrationTotalFormat <- header.true(netMigrationTotal)

View(netMigrationTotalFormat) # Net migration leaving+coming male/female/total

plot(netMigrationTotalFormat$Insgesamt, 
     main="Net migration",
     ylabel="Change",
     xlabel="Years")




#plot(netMigrationTotal$V10, 
#           main="Net migration",
#           ylab="Change",
#           xlab="Years")