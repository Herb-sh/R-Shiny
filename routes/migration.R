# Authors: Anitta Weiss & Herbi Shtini
migrationReady <- function(input, output, session, clicks) {
  output$migration = renderPlotly({
    plotMigration()
  })
}