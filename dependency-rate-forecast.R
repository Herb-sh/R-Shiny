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

#install.packages("devtools")
#devtools::install_github("ramnathv/rCharts")



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
Employed_all <- list_all[[2]]
Employed = subset(Employed_all, select = c(`Year`,`Employed_total`))
NewRenter <- list_all[[3]]
Immigration <- list_all[[4]]
LifeExpentency <-list_all[[5]]
Fertility <-list_all[[6]]

#Annual growth rate of total population

#view(Fertility) 
#view(German_pop)

#*******A.FORCASTING POPULATION STRUCTURE UNTIL 2050 USING PROPHET***********#

library(lubridate)
library(prophet)

#1******** FORECAST TOTAL POPULATION ESTIMATION BIS 2050 *****************************

# filter the German pop only taking the range 'TOTAL'
Germ_total <- filter(German_pop, Gender=='Total', Age=='Total')

Germ_total$Year <- str_c(Germ_total$Year,"01", '01', sep='-')
Germ_total$Year <- as.Date(c(Germ_total$Year))
Germ_total = subset(Germ_total, select = c(`Year`,`PopCount`))

names(Germ_total)[names(Germ_total)=='Year'] <-'ds'
names(Germ_total)[names(Germ_total)=='PopCount'] <-'y'
model_pop <- prophet(yearly.seasonality=TRUE)
model_pop <-fit.prophet(model_pop,Germ_total)
future_pop <- make_future_dataframe(model_pop, periods= 30, freq ='year')
tail(future_pop)

#forecast 
forecast_pop <- predict(model_pop, future_pop)
tail(forecast_pop[c('ds','yhat','yhat_lower','yhat_upper')])

#Plot the Estimates
Germ_totalplot <- dyplot.prophet(model_pop, forecast_pop)
Germ_totalplot

#***********************************************************************************
#2******** FORECAST WORKING POPULATION ESTIMATION BIS 2050 **************************


Employed$Year <- str_c(Employed$Year,"01", '01', sep='-')
Employed$Year <- as.Date(c(Employed$Year))
 
names(Employed)[names(Employed)=='Year'] <-'ds'
names(Employed)[names(Employed)=='Employed_total'] <-'y'

model_Empl <-  prophet(yearly.seasonality=TRUE)
model_Empl <-  fit.prophet(model_Empl,Employed)
future_Empl <- make_future_dataframe(model_Empl, periods= 30, freq ='year')
tail(future_Empl)

#forecast 
forecast_Empl <- predict(model_Empl, future_Empl)
tail(forecast_Empl[c('ds','yhat','yhat_lower','yhat_upper')])

#Plot the Estimates
Employedplot <- dyplot.prophet(model_Empl, forecast_Empl)
Employedplot



#3.*****FORCAST DEPENDENCY QUOTIENT( RANGE OF POP NOT CONTRIBUTING TO THE LABOR FORCE)******##########
#2. DEPENDENCY RATE OR QUOTIENT
Germ_depen_rate <- filter(German_pop, Gender=='Total', Age =='Total dependency ratio ((<20 & 65+) / 20-64)') 
Germ_depen_rate$Year <- str_c( Germ_depen_rate$Year,"01", '01', sep='-')
Germ_depen_rate$Year <- as.Date(c(Germ_depen_rate$Year))
Germ_depen_rate = subset(Germ_depen_rate, select = c(`Year`,`PopCount`))


names(Germ_depen_rate )[names(Germ_depen_rate)=='Year'] <-'ds'
names(Germ_depen_rate )[names(Germ_depen_rate)=='PopCount'] <-'y'

model_depen_rate <- prophet(yearly.seasonality=TRUE)
model_depen_rate  <-fit.prophet(model_depen_rate , Germ_depen_rate)

# create future dataframe 
future_depen_rate<- make_future_dataframe(model_depen_rate, periods= 30, freq ='year')
tail(future_depen_rate)

#forecast
future_depen_rate <- predict(model_depen_rate, future_depen_rate)
tail(future_depen_rate[c('ds','yhat','yhat_lower','yhat_upper')])

#Plot the Estimates
Germ_depen_rateplot <- dyplot.prophet(model_depen_rate, future_depen_rate)
Germ_depen_rateplot 

#*********************************************************************************************************************
#**** TWO FACTORS CAN CHANGE THIS STRUCTURE ARE:  IMMIGRATION, FERTILITY AND LIFE EXPENTENCY ########################## 
#3.WE ARE FOCUSING ON THEIR EFFECTS ON THE DEPENDENCY RATE, THEN WE WILL LOOK AT EFFECT ON ON AND OLD PEOPLE

#A FIRST I CREATE THE FUTURE PREDICTION FOR EACH OF THEM 

#A1. IMMIGRATION

Immigration <- Immigration %>% select (!c("Growth Rate")) 
Immigration$Year <- str_c( Immigration$Year,"01", '01', sep='-')
Immigration$Year <- as.Date(c(Immigration$Year))

names(Immigration)[names(Immigration)=='Year'] <-'ds'
names(Immigration )[names(Immigration)=="Net Migration Rate"] <-'y'

model_imm <- prophet(yearly.seasonality=TRUE)
model_imm  <-fit.prophet(model_imm , Immigration)

# create future dataframe 
future_imm<- make_future_dataframe(model_imm, periods= 30, freq ='year')
tail(future_imm)

#forecast
future_imm <- predict(model_imm, future_imm)
tail(future_imm[c('ds','yhat','yhat_lower','yhat_upper')])

#Plot the Estimates
Immigrationplot <- dyplot.prophet(model_imm, future_imm)
Immigrationplot 


##**************************
#A2. FERTILITY
 
Fertility <- Fertility %>% select (!c("Growth Rate")) 
Fertility$Year <- str_c( Fertility$Year,"01", '01', sep='-')
Fertility$Year <- as.Date(c(Fertility$Year))

names(Fertility)[names(Fertility)=='Year'] <-'ds'
names(Fertility )[names(Fertility)=="Fertility_rate_women"] <-'y'

model_fert <- prophet(yearly.seasonality=TRUE)
model_fert  <-fit.prophet(model_fert , Fertility)

# create future dataframe 
future_fert<- make_future_dataframe(model_fert, periods= 30, freq ='year')
tail(future_fert)

#forecast
future_fert <- predict(model_fert, future_fert)
tail(future_fert[c('ds','yhat','yhat_lower','yhat_upper')])

#Plot the Estimates
Fertilityplot <- dyplot.prophet(model_fert, future_fert)
Fertilityplot 



##**************************
#A3 LIFE EXPENTENCY

LifeExpentency$Year <- str_c(LifeExpentency$Year,"01", '01', sep='-')
LifeExpentency$Year <- as.Date(c(LifeExpentency$Year))

names(LifeExpentency)[names(LifeExpentency)=='Year'] <-'ds'
names(LifeExpentency)[names(LifeExpentency)=="LifeExpentency_total_year"] <-'y'
LifeExpentency <- head(LifeExpentency, - 1) # deleting the last row 

model_lifeEx <- prophet(yearly.seasonality=TRUE)
model_lifeEx  <-fit.prophet(model_lifeEx , LifeExpentency)

# create future dataframe 
future_lifeEx<- make_future_dataframe(model_lifeEx, periods= 30, freq ='year')
tail(future_lifeEx)

#forecast
future_lifeEx <- predict(model_lifeEx, future_lifeEx)
tail(future_lifeEx[c('ds','yhat','yhat_lower','yhat_upper')])

#Plot the Estimates
Fertilityplot <- dyplot.prophet(model_lifeEx, future_lifeEx)
Fertilityplot 



###### B combining the lifeExpentency, fertility and immigration to predict dependency ratio
### *************************************************************************************
# This portion of the code could be cleaner

#### FIRST WORKING WITH THE HISTORICAL DATA 


Merged_Germ_depen_rate = Germ_depen_rate %>% inner_join(Fertility,by="ds") %>%
                        inner_join(LifeExpentency, by='ds') %>%
                        inner_join( Immigration, by='ds')
 
# renames 
names(Merged_Germ_depen_rate )[names(Merged_Germ_depen_rate)=='y.x'] <-'y'
names(Merged_Germ_depen_rate )[names(Merged_Germ_depen_rate)=='y.y'] <-'Fertility'
names(Merged_Germ_depen_rate )[names(Merged_Germ_depen_rate)=='y.x.x'] <-'Life_expect'
names(Merged_Germ_depen_rate )[names(Merged_Germ_depen_rate)=='y.y.y'] <-'Immigrat'

# modeling and prediction

model_merged <- prophet (yearly.seasonality=TRUE) # defining the model
model_merged <- add_regressor(model_merged , 'Fertility', prior.scale = NULL, standardize = "False", mode = NULL)
model_merged <- add_regressor(model_merged , 'Life_expect', prior.scale = NULL, standardize = "False", mode = NULL)
model_merged <- add_regressor(model_merged , 'Immigrat', prior.scale = NULL, standardize = "False", mode = NULL)
model_merged <- fit.prophet(model_merged , Merged_Germ_depen_rate)


# create future dataframe, because i am using the add_regressor 
future_Merged_Germ_depen_rate <- make_future_dataframe(model_merged, periods= 30, freq ='year')

future_fert <- head(future_fert, - 1) # deleting the last row 
future_imm <- head(future_imm, - 2) # deleting the last row 

future_Merged_Germ_depen_rate$Fertility <-future_fert$yhat
future_Merged_Germ_depen_rate$Life_expect <-future_lifeEx$yhat 
future_Merged_Germ_depen_rate$Immigrat<- future_imm$yhat

#forecast

forecast <- predict(model_merged, future_Merged_Germ_depen_rate)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
#Plot and  Estimates
pp <- plot(model_merged, forecast)
pp2 <- pp + geom_point(col = 'black') + ggtitle('Adjusted Forecast') + 
      xlab('years') +theme_bw()+ ylab('Ratio of the Dependent Population')
qq2 <- ggplot_build(pp2)
plot(ggplot_gtable(qq2))


#####################################################################



######################################################################

 
 
###################################################################

 


 