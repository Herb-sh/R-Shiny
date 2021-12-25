populationDataFrame  <- list_all[[1]]

# Total dependency ratio ((<20 & 65+) / 20-64)
totalDepend <- filter(populationDataFrame, Gender=='Total', Age=='Total dependency ratio ((<20 & 65+) / 20-64)')
totalDepend  <- totalDepend  %>% select (!c("Country", 'Gender', 'Age'))


# Old age dependency ratio (65 and over/15-64)
oldDepend <- filter(populationDataFrame, Gender=='Total', Age=='Old age dependency ratio (65 and over/15-64)')
oldDepend <- oldDepend %>% select (!c("Country", 'Gender', 'Age'))

# Young age dependency ratio ( less than 20 )
youngDepend <- merge(x = totalDepend, y = oldDepend, by = "Year")
youngDepend <- mutate(youngDepend, Population = Population.x - Population.y) %>% select(Year, Population)


# FORECASTING TOTAL DEPENDENCY RATES

getDependencyRatePlot <- function (metric, xlab, ylab, col){
  
  df <- totalDepend
  graphLabel='Gesamtanteil der abängigen Bevölkerung (%)'
  
  if (metric == 'old') {
    df <- oldDepend
    graphLabel = 'Anteil der Altergruppe (>65) an der Gesamtbevölkerung (%)'
  } else if (metric == 'young') {
    df <- youngDepend
    graphLabel = 'Anteil der Altergruppe (<20) an der Gesamtbevölkerung (%)'
  }
  
  inputColumn = "Population"
  
  df<- df %>% select(c('Year',inputColumn))                                        
  df$Year <- str_c(df$Year, "01", '01', sep='-')
  df$Year <- as.Date(c(df$Year))
  df$Population <- df$Population * 100
  
  names(df)[names(df)=='Year'] <-'ds'
  names(df)[names(df)==inputColumn] <-'y'
  
  model_pop <- prophet(yearly.seasonality=TRUE)
  model_pop <- fit.prophet(model_pop, df)
  future_pop <- make_future_dataframe(model_pop, periods= 15, freq ='year')
  tail(future_pop)
  
  #forecast 
  forecast_pop <- predict(model_pop, future_pop)
  tail(forecast_pop[c('ds','yhat','yhat_lower','yhat_upper')])
  
  #Plot and  Estimates
  pp <-dyplot.prophet(model_pop,forecast_pop, main =sprintf('FORECAST'), ylab = graphLabel, xlab='Year')
  
  
  #******************************************************************************
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
  
  err_df<- forecast_error %>% select ('ds', 'yhat','Actual')
  names(err_df)[names(err_df)=='ds'] <-'Year'
  names(err_df)[names(err_df)=='yhat'] <-'Predicted'
  
  #ERROR RSME CALCULAION
  se<- (err_df$Actual - err_df$Predicted)
  se<-head(se,-1)
  rmse <- round(sqrt(mean((se)^2)), digits =2)
  
  
  err_df <- as.data.frame(err_df)
  fig_err <- plot_ly(data =err_df, y = ~Predicted, x= ~Year, type = 'scatter', mode = 'lines', name ="Predicted" )
  fig_err <- fig_err %>% add_trace(y= ~ Actual,mode='lines', name = "Actual") %>% 
    layout(
      title =sprintf("Bewertung des Vorhersagemodells, RMSE: %f",rmse),
      yaxis = list(title = graphLabel),
      legend=list(title=list(text='<b> MODELL_PROPHET </b>')))
  
  return(combineWidgets(ncol = 2,  pp, fig_err))
}