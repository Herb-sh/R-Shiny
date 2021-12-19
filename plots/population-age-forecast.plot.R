# 1.0 READ MULTIPLE EXCEL SHEETS ----
path   <- 'data/Rente_All.xlsx'
sheet_names <- excel_sheets(path)


# 2.0 INVESTIGATE DATA FOR EACH SHEET ----

list_all <- lapply(sheet_names, function(x) {as.data.frame(read_excel(path= path, sheet=x))})
German_pop <- list_all[[1]]
Employed_all <- list_all[[2]]
Employed = subset(Employed_all, select = c(`Year`,`Employed`))
Immigration <- list_all[[4]]
LifeExpentency <-list_all[[5]]
Fertility <-list_all[[6]]


### SELECT and MERGE THIS DATA INTO ONE DATA FRAME:
Germ_total <- filter(German_pop, Gender=='Total', Age=='Total')
Germ_total <- Germ_total %>% select (!c("Country", 'Gender', 'Age'))
Immigration <- Immigration %>% select (!c("Growth Rate")) 

df = Germ_total%>% inner_join(Fertility,by="Year") %>%
  inner_join(LifeExpentency, by='Year') %>%
  inner_join( Immigration, by='Year')




#********BUILDING FUNCTION***************************************************#
#*******A.FORCASTING POPULATION STRUCTURE UNTIL 2050 USING PROPHET***********#

populationAgeForecast <- function (inputColumn){
  
  #inputColumn = "Fertility Rate"
  
  df<- df %>% select(c('Year',inputColumn))                                        
  df$Year <- str_c(df$Year,"01", '01', sep='-')
  df$Year <- as.Date(c(df$Year))
  
  names(df)[names(df)=='Year'] <-'ds'
  names(df)[names(df)==inputColumn] <-'y'
  
  model_pop <- prophet(yearly.seasonality=TRUE)
  model_pop <- fit.prophet(model_pop,df)
  future_pop <- make_future_dataframe(model_pop, periods= 10, freq ='year')
  tail(future_pop)
  
  #forecast 
  forecast_pop <- predict(model_pop, future_pop)
  tail(forecast_pop[c('ds','yhat','yhat_lower','yhat_upper')])
  
  #Plot forecast

 # pp <- plot(model_pop, forecast_pop, xlab="Year", ylab=inputColumn)
 # pp1 <-pp+ title(main="My Title")
  
  pp <- dyplot.prophet(model_pop,forecast_pop, main =sprintf('FORECAST: %s', inputColumn)) 
 
  ###---------------------------
  # Performing the projection on historical data to determine the accuracy of the forecast
  
  df_error<- head(df,20)
  model_error <- prophet(yearly.seasonality=TRUE, montly.seasonality=TRUE)
  model_error<- fit.prophet(model_error,df_error)
  future_error <- make_future_dataframe(model_error, periods= 11, freq ='year')
  tail(future_error)
  
  forecast_error <- predict(model_error, future_error)
  # merge the actual data with the predicted data
  forecast_error$Actual <-df$y
  tail(forecast_error[c('ds','yhat','yhat_lower','yhat_upper','Actual')])
  
  err_df<- forecast_error %>% select ( 'ds', 'yhat','Actual')
  names(err_df)[names(err_df)=='ds'] <-'Year'
  names(err_df)[names(err_df)=='yhat'] <-'Predicted'
  
  #ERROR RSME CALCULAION
  
  se<- (err_df$Actual - err_df$Predicted)
  se<-head(se,-1)
  rmse <- round(sqrt(mean((se)^2)), digits =2)
  

#library(plotly)
#library(tidyr)
# library(plyr)
#library(manipulateWidget)
  
  err_df <- as.data.frame(err_df)
  fig_err <- plot_ly(data =err_df, y = ~Predicted, x= ~Year, type = 'scatter', mode = 'lines', name ="Predicted" )
  fig_err <- fig_err %>% add_trace( y= ~ Actual, name = "Actual")
  fig_err <- fig_err %>% layout(
                 title =sprintf("Prophet Model Evaluation -RMSE: %f",rmse),
                 #plot_bgcolor = "#e5ecf6",
                yaxis = list(title = inputColumn),
                legend=list(title=list(text='<b> PROPHET MODEL </b>')))
  
  return( combineWidgets(ncol =2,  pp, fig_err))
}
