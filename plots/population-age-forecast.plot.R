populationDataFrame <- list_all[[1]]
employedAll <- list_all[[2]]
employed = subset(employedAll, select = c(`Year`,`Employed`))
immigration <- list_all[[4]]
lifeExpentency <-list_all[[5]]
fertility <-list_all[[6]]


### SELECT and MERGE THIS DATA INTO ONE DATA FRAME:
Germ_total <- filter(populationDataFrame, Gender=='Total', Age=='Total')
Germ_total <- Germ_total %>% select (!c("Country", 'Gender', 'Age'))
immigration <- immigration %>% select (!c("Growth Rate")) 

df = Germ_total%>% inner_join(fertility,by="Year") %>%
  inner_join(lifeExpentency, by='Year') %>%
  inner_join( immigration, by='Year')

df$Population<- round((df$Population)/1000000,2)


#********BUILDING FUNCTION***************************************************#
#*******A.FORCASTING POPULATION STRUCTURE UNTIL 2050 USING PROPHET***********#

populationAgeForecast <- function (InputColumn){

  if (InputColumn == 'Fertility Rate'){
    plot_tilte='Prognose: Fertilitätsrate'
  }else if (InputColumn == 'LifeExpentency (year)') {
    plot_tilte='Prognose: Durchschnittliche Lebenserwartung'
  }else if (InputColumn == "Net Migration Rate" ){
    plot_tilte='Prognose: Ein- und Auswandereungszahl pro Jahr.'
  }else if (InputColumn == "Population") {
    plot_tilte='Prognose: Gesamtbevölkerungszahl (Million)'
  }
  
  graphLabel=InputColumn
  
  df<- df %>% select(c('Year',InputColumn))                                        
  df$Year <- str_c(df$Year,"01", '01', sep='-')
  df$Year <- as.Date(c(df$Year))
  
  names(df)[names(df)=='Year'] <-'ds'
  names(df)[names(df)==InputColumn] <-'y'
  
  model_pop1 <- prophet(yearly.seasonality=TRUE)
  model_pop1 <- fit.prophet(model_pop1,df)
  future_pop <- make_future_dataframe(model_pop1, periods= 10, freq ='year')
  tail(future_pop)
  
  #forecast 
  forecast_pop <- predict(model_pop1, future_pop)
  tail(forecast_pop[c('ds','yhat','yhat_lower','yhat_upper')])
  
  #Plot forecast
  pp <-dyplot.prophet(model_pop1,forecast_pop, main =sprintf(plot_tilte), ylab = graphLabel, xlab='Year')
  
  
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
  
  
  err_df <- as.data.frame(err_df)
  fig_err <- plot_ly(data =err_df, y = ~Predicted, x= ~Year, type = 'scatter', mode = 'lines', name ="Predicted" )
  fig_err <- fig_err %>% add_trace( y= ~ Actual, name = "Actual") %>% 
    layout(
      title = sprintf("Bewertung des Vorhersagemodells, RMSE: %f",rmse), 
      yaxis = list(title = plot_tilte),
      legend=list(title=list(text='<b> MODELL_PROPHET </b>')))
  
  return(combineWidgets(ncol =2,  pp, fig_err))
}