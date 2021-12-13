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


### SELECT an MERGE THIS DATA INTO ONE DATA FRAME:
Germ_total <- filter(German_pop, Gender=='Total', Age=='Total')
Germ_total <- Germ_total %>% select (!c("Country", 'Gender', 'Age'))
Immigration <- Immigration %>% select (!c("Growth Rate")) 

df = Germ_total%>% inner_join(Fertility,by="Year") %>%
  inner_join(LifeExpentency, by='Year') %>%
  inner_join( Immigration, by='Year')




#********BUILDING FUNCTION***************************************************#
#*******A.FORCASTING POPULATION STRUCTURE UNTIL 2050 USING PROPHET***********#

populationAgeForecast <- function (inputColumn){
  
  
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
  pp <- plot(model_pop, forecast_pop, title='FORECAST', xlab='Year',  ylab=(inputColumn)) 
  
  
  
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
  
  #PLOT
  err<-ggplot(err_df, aes(Year)) +  
    geom_line(aes(y=Predicted, colour="red"), size=2) +  # first layer
    geom_line(aes(y=Actual, colour="purple"), size=2) +
    xlab('Year') + ylab(inputColumn) +
    ggtitle(sprintf("Prophet Prediction Performance \\
                  Forecast vs Actual RMSE: %f",rmse)) +
    scale_color_identity(name = "Model fit",
                         breaks = c("red", "purple"),
                         labels = c("Predicted", "Actual"),
                         guide = "legend")
  
  return(grid.arrange(pp, err, ncol=1))
}
