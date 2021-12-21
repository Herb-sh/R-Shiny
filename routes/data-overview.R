dataOverviewReady <- function(input, output, session, clicks) {
  
  observeEvent(input$dataOverviewChange, {
    if (input$dataOverviewChange == 'all') {
      output$table = DT::renderDataTable(DT::datatable({
        generateTable()
      }))
    } else {
      output$table = DT::renderDataTable(DT::datatable({
        generateTable(input$dataOverviewChange)
      }))
    }
  })

  output$table = DT::renderDataTable(DT::datatable({
    generateTable()
  }))
  
  generateTable <- function(metricCode='all') {
    generated <- population 
    if (metricCode != 'all') {
      generated <- generated %>% filter(MetricCode == metricCode)
    }
    return(generated %>% select(Gender, Metric, Year, Value))
  }
}