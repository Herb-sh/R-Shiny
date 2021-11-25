# BUSINESS SCIENCE  
# EPISODE 3
# POPULATION ANALYTICS: EXCEL TO R & THE CORRELATION FUNNEL
#rm(list=ls()) 

# unemployement location: 
#https://www.bmfsfj.de/resource/blob/113952/83dbe067b083c7e8475309a88da89721/aeltere-menschen-in-deutschland-und-in-der-eu-englisch-data.pdf
# renter location dataset: https://www.bmas.de/DE/Service/Statistiken-Open-Data/Daten-zur-Rente/daten-zur-rente.html

#install.packages("reshape2")
#install.packages("plyr")
#install.packages("ggplot2")

install.packages("devtools")
devtools::install_github("ramnathv/rCharts")



# Libraries
library(tidyverse)
library(readxl)
library(recipes)
library(tidyquant)
library(ggrepel)
library(dplyr)
library(skimr)
library(rCharts)


# 1.0 READ MULTIPLE EXCEL SHEETS ----
path   <- 'data/Combined-data/Rente_All.xlsx'
sheet_names <- excel_sheets(path)


# 2.0 INVESTIGATE DATA FOR EACH SHEET ----

list_all <- lapply(sheet_names, function(x) {as.data.frame(read_excel(path= path, sheet=x))})
German_pop <- list_all[[1]]
Employed <- list_all[[2]]
NewRenter <- list_all[[3]]
immigration <- list_all[[4]]
  
 
#A.FORCASTING POPULATION GROWTH  rate UNTIL 2050 USING PROPHET 

library(lubridate)
library(prophet)

#1.  filter the German pop only taking the range 'TOTAL'
Germ_total <- filter(German_pop, Gender=='Total', Age=='Total')

#2. DEPENDENCY RATE
Germ_depen_rate <- filter(German_pop, Gender=='Total', Age =='Total dependency ratio ((<20 & 65+) / 20-64)') 
Germ_depen_rate$Year <- str_c( Germ_depen_rate$Year,"01", '01', sep='-')
Germ_depen_rate$Year <- as.Date(c(Germ_depen_rate$Year))
Germ_depen_rate = subset(Germ_depen_rate, select = c(`Year`,`PopCount`))


#3. German Population growth rate

Germ_growth_rate <- filter(German_pop, Gender=='Total', Age =='Annual growth rate of total population') 
Germ_growth_rate$Year <- str_c( Germ_growth_rate$Year,"01", '01', sep='-')
Germ_growth_rate$Year <- as.Date(c(Germ_growth_rate$Year))
Germ_growth_rate = subset(Germ_growth_rate, select = c(`Year`,`PopCount`))

#4 German Unemployment rate

Germ_ump_fract= subset (Employed, select = c('Year', 'unemp_fract')) 
Germ_ump_fract$Year <- str_c( Germ_ump_fract$Year,"01", '01', sep='-')
Germ_ump_fract$Year <- as.Date(c(Germ_ump_fract$Year))
 

# MERGING ALL 2,3 and 4 
mergedCol <- inner_join(Germ_depen_rate, Germ_growth_rate,  by='Year')
mergedCol <- inner_join(mergedCol, Germ_ump_fract,  by='Year')

# PREDICTION WITH PROPHET 
#  Prophet naming convention ( ds, y)
names(mergedCol)[names(mergedCol)=='Year'] <-'ds'
names(mergedCol)[names(mergedCol)=='PopCount.x'] <-'y'
names(mergedCol)[names(mergedCol)=='PopCount.y'] <-'growth_rate'


### BUILDING THE MODELING NOW; projection over the next 30 years
#1.Model for german pop growth

names(Germ_growth_rate)[names(Germ_growth_rate)=='Year'] <-'ds'
names(Germ_growth_rate)[names(Germ_growth_rate)=='PopCount'] <-'y'
model0 <- prophet(yearly.seasonality=TRUE)
model0 <-fit.prophet(model0,Germ_growth_rate)
future0 <- make_future_dataframe(model0, periods= 30, freq ='year')
tail(future0)

#forecast 
forecast0 <- predict(model0, future0)
tail(forecast0[c('ds','yhat','yhat_lower','yhat_upper')])

#Plot the Estimates
Germ_growth_rateplot <- dyplot.prophet(model0, forecast0)
Germ_growth_rateplot

# rename yhat to be used in the next model
names(forecast0)[names(forecast0)=='yhat'] <-'growth_rate'

#####################################################################
#2. build the model for unemployement rate 

names(Germ_ump_fract)[names(Germ_ump_fract)=='Year'] <-'ds'
names(Germ_ump_fract)[names(Germ_ump_fract)=='unemp_fract'] <-'y'
model1 <- prophet(yearly.seasonality=TRUE)
model1 <-fit.prophet(model1,Germ_ump_fract)
future1 <- make_future_dataframe(model1, periods= 30, freq ='year')
tail(future1)

#forecast 
forecast1 <- predict(model1, future1)
tail(forecast1[c('ds','yhat','yhat_lower','yhat_upper')])

#Plot the Estimates
Germ_ump_fractplot <- dyplot.prophet(model1, forecast1)
Germ_ump_fractplot

# rename yhat to be used in the next model
names(forecast1)[names(forecast1)=='yhat'] <-'unemp_fract'



######################################################################
# Building the model of dependency rate plus projection 

model2 <- prophet(yearly.seasonality=TRUE)
model2 <- add_regressor(forecast0, 'growth_rate', prior.scale = NULL, standardize = "auto", mode = NULL)
model2 <- add_regressor(forecast1, 'unemp_fract', prior.scale = NULL, standardize = "auto", mode = NULL)
model2 <-fit.prophet(model2,mergedCol)

# create future dataframe 
future2 <- make_future_dataframe(model2, periods= 30, freq ='year')
tail(future2)

#forecast
forecast2 <- predict(model2, future2)
tail(forecast2[c('ds','yhat','yhat_lower','yhat_upper')])
  
#Plot the Estimates
plotDependencyRateplot <- dyplot.prophet(model2, forecast2)
plotDependencyRateplot 
 
 
###################################################################

 


 