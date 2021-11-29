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
library(lubridate)
library(prophet)


# 1.0 READ MULTIPLE EXCEL SHEETS ----
path   <- 'data/Combined-data/Rente_All.xlsx'
sheet_names <- excel_sheets(path)


# 2.0 INVESTIGATE DATA FOR EACH SHEET ----

list_all <- lapply(sheet_names, function(x) {as.data.frame(read_excel(path= path, sheet=x))})
German_pop <- list_all[[1]]
Employed_all <- list_all[[2]]
Employed = subset(Employed_all, select = c(`Year`,`Employed`))
NewRenter <- list_all[[3]]
Immigration <- list_all[[4]]
LifeExpentency <-list_all[[5]]
Fertility <-list_all[[6]]

 
### SELECT an MERGE THIS DATA INTO ONE DATA FRAME:
Germ_total <- filter(German_pop, Gender=='Total', Age=='Total')
Germ_total <- Germ_total %>% select (!c("Country", 'Gender', 'Age'))
Immigration <- Immigration %>% select (!c("Growth Rate")) 

df = Germ_total%>% inner_join(Fertility,by="Year") %>%
   inner_join(LifeExpentency, by='Year') %>%
   inner_join( Immigration, by='Year')




#********BUILDING FUNCTION***************************************************#
#*******A.FORCASTING POPULATION STRUCTURE UNTIL 2050 USING PROPHET***********#

forecast_funct <- function (inputColumn){
     
      #inputColumn="Net Migration Rate"
                               
      df<- df %>% select(c('Year',inputColumn))                                        
      df$Year <- str_c(df$Year,"01", '01', sep='-')
      df$Year <- as.Date(c(df$Year))
 
      names(df)[names(df)=='Year'] <-'ds'
      names(df)[names(df)==inputColumn] <-'y'
      model_pop <- prophet(yearly.seasonality=TRUE)
      model_pop <- fit.prophet(model_pop,df)
      future_pop <- make_future_dataframe(model_pop, periods= 30, freq ='year')
      tail(future_pop)

      #forecast 
      forecast_pop <- predict(model_pop, future_pop)
      tail(forecast_pop[c('ds','yhat','yhat_lower','yhat_upper')])
 
 
      #Plot and  Estimates
      pp <- plot(model_pop, forecast_pop)
      pp2 <- pp + geom_point(col = 'black') +  
      xlab('years') +theme_bw()+ ylab(inputColumn)
      qq2 <- ggplot_build(pp2)
      plot(ggplot_gtable(qq2))
      

return (plot(ggplot_gtable(qq2)))
}
#***********************************************************************************
 