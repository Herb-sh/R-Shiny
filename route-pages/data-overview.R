dataOverviewReady <- function(input, output, session, clicks) {
  
  populationTable <- population %>% select(Gender, Metric, Year, Value)
  
  output$table = DT::renderDataTable(DT::datatable({
    populationTable
  }))
  
}