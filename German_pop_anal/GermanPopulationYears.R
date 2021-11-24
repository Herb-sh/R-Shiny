#install.packages("ggplot2") 
#These packages are used for data profiling
#install.packages("devtools")
#install.packages("usethis")



# data source: https://stats.oecd.org/index.aspx?DataSetCode=POP_PROJ


#library(tidyquant)
library(tidyverse)
#library(gt)
#library(devtools)
library(inspectdf)
library(readr)
library(dplyr)
#library(skimr)
#library(reshape2)
#library("extrafont")

#1. IMPORT AN VIEWING----

path <- 'C:/Users/zinta/Documents/AALEN LECTURES/BusinessAnalitics/r-project-main/data/Historical_German_Population.csv'
germ_data <- read_csv (path)
head(germ_data)
view(germ_data)

#1. SELECT Columns ---
germ_data <- subset(germ_data, select = c(`Sex`,`Age`, `Value`,`Time`))
names(germ_data)
summary(germ_data)
glimpse(germ_data)

view(germ_data)



#2. INSPECT DATA
# Observation:  Time and Value are numeric and Sex and Age are in character

inspect_types(germ_data) %>% show_plot()

#2a. SIZE

inspect_mem(germ_data)  %>% show_plot() 

#Observation: 'The size of the ages are greater than the others -> there are ore characters -->there might be repeating info'
# indeed ages include are grouped and aggregation-

#2b.  % OF NA VALUES 
inspect_na(germ_data) %>% show_plot()
#R: There is none 

#2c. VISUALIZING OF FULL DISTRIBUTION (NUMERICAL AND CATERGORICAL)
inspect_cat(germ_data) %>% show_plot()


#3. Further manipulation 
# converting population into millions
germ_data$Value<- round(germ_data$Value/1000000,2)
head(germ_data)
view(germ_data)

#Selecting rows base on the age column 

# first let determine the type 

str(germ_data)

#Observation: Age is a string

# using trying manipulation to identify the subset of the data that i need . 


##
plot  <- germ_data %>%
  select(Age, Time, Value,Sex) %>%
  filter( ., Age == 'Total' & Sex == "Total") %>%
  #filter(Sex %in% c("Women", "Men")) %>%
  ggplot(aes(x = Time, y = Value)) +
  geom_point(shape=21, color="orange", fill="#ffa500", size=3) +
  #geom_text(aes(label=Value),hjust=0, vjust=0) +
  geom_line( color="orange") +
  ggtitle("German Population [1990-2020]") +
  xlab("Year") +
  ylab("Polulation") +
  ylim(75,85) +
  theme_bw() +
  theme(
        plot.title = element_text(size=13, color="orange"),
        axis.title.x = element_text(size=13, color="orange"),
        axis.title.y = element_text(size=13, color="orange"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

plot



arrange(time) 



