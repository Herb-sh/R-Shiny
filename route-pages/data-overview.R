dataOverviewReady <- function(input, output, session, clicks) {
  
  output$table = DT::renderDataTable(DT::datatable({
    population
  }))
  
}