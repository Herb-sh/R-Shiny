# 1.0 READ MULTIPLE EXCEL SHEETS ----
path   <- 'data/Rente_All.xlsx'
sheet_names <- excel_sheets(path)


# 2.0 CREATING OLD AND TOTAL DEPENDENCY DATASETS ----

list_all <- lapply(sheet_names, function(x) {as.data.frame(read_excel(path= path, sheet=x))})
German_population  <- list_all[[1]]

# Total dependency ratio ((<20 & 65+) / 20-64)
Total_depend <- filter(German_pop, Gender=='Total', Age=='Total dependency ratio ((<20 & 65+) / 20-64)')
Total_depend  <- Total_depend  %>% select (!c("Country", 'Gender', 'Age'))


# Old age dependency ratio (65 and over/15-64)
OLD_depend <- filter(German_pop, Gender=='Total', Age=='Old age dependency ratio (65 and over/15-64)')
OLD_depend <- OLD_depend %>% select (!c("Country", 'Gender', 'Age'))


# FORECASTING TOTAL DEPENDENCY RATES

TotalDepend_funct <- function (){
  
  df<-Total_depend
  inputColumn ="Population"
  
  df<- df %>% select(c('Year',inputColumn))                                        
  df$Year <- str_c(df$Year,"01", '01', sep='-')
  df$Year <- as.Date(c(df$Year))
  
  names(df)[names(df)=='Year'] <-'ds'
  names(df)[names(df)==inputColumn] <-'y'
  model_pop <- prophet(yearly.seasonality=TRUE)
  model_pop <- fit.prophet(model_pop,df)
  future_pop <- make_future_dataframe(model_pop, periods= 15, freq ='year')
  tail(future_pop)
  
  #forecast 
  forecast_pop <- predict(model_pop, future_pop)
  tail(forecast_pop[c('ds','yhat','yhat_lower','yhat_upper')])
  
  
  #Plot and  Estimates
  pp <- plot(model_pop, forecast_pop)
  pp2 <- pp + geom_point(col = 'green') +  
    xlab('Year') +theme_bw()+ ylab('Total Dependency Ratio')
  TotalDepend <- ggplot_build(pp2)
  #plot(ggplot_gtable(qq2))
  
  
  return (plot(ggplot_gtable(TotalDepend)))
}


### SECOND FUNCTION FORECAST OLD AGE DEPENDENCY RATIO

OldAgeDepend_funct <- function ( ){
  
  df<-OLD_depend 
  inputColumn ="Population"
  
  df<- df %>% select(c('Year',inputColumn))                                        
  df$Year <- str_c(df$Year,"01", '01', sep='-')
  df$Year <- as.Date(c(df$Year))
  
  names(df)[names(df)=='Year'] <-'ds'
  names(df)[names(df)==inputColumn] <-'y'
  model_pop <- prophet(yearly.seasonality=TRUE)
  model_pop <- fit.prophet(model_pop,df)
  future_pop <- make_future_dataframe(model_pop, periods= 15, freq ='year')
  tail(future_pop)
  
  #forecast 
  forecast_pop <- predict(model_pop, future_pop)
  tail(forecast_pop[c('ds','yhat','yhat_lower','yhat_upper')])
  
  
  #Plot and  Estimates
  pp <- plot(model_pop, forecast_pop)
  pp2 <- pp + geom_point(col = 'purple') +  
    xlab('Year') +theme_bw()+ ylab('65+ old Dependency Ratio')
  OLD_dependplot <- ggplot_build(pp2)
  #plot(ggplot_gtable(qq2))
  
  
  return (plot(ggplot_gtable(OLD_dependplot)))
}

