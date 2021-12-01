options(repos = list(CRAN="http://cran.rstudio.com/"))
### Install required packages
required_packages<-c("rio",
                     "tidyverse",
                     "shiny",
                     "quantmod", 
                     "plotly",
                     "shiny.router",
                     "zoo",
                     "gganimate",
                     "viridis",
                     "hrbrthemes",
                     "devtools",
                     "prophet",
                     "lubridate",
                     "readxl",
                     "recipes",
                     "tidyquant",
                     "ggrepel",
                     "dplyr",
                     "skimr",
                     "rCharts")

# try to load packages and install missing ones
for (package in required_packages) {
   # require tries to load a package, and returns a boolean indicating success
   if (!require(package, character.only = TRUE)) {
      install.packages(package , dependencies = TRUE)
      library(package, character.only = TRUE)
   }
}

# Plots
source("plots/utilities/population.service.R")
source('plots/migration.plot.R')
source('plots/employment-rate.plot.R')
source('plots/population-age-group.plot.R')
source('plots/population-age-range.plot.R')
source('plots/population-age-forecast.plot.R')
source('plots/population-age-range-wave.plot.R')
# Routes
source('route-pages/dashboard.R')
source('route-pages/data-overview.R')
source('route-pages/population.R')
source('route-pages/dependency-rate.R')
source('route-pages/dependency-rate-forecast.R')
source('route-pages/migration.R')

# Configs
#options(shiny.port = 4200)

page <- function(body, title) {
   return(fluidPage(
      tags$div(class="pagetitle",
         tags$h1(title)
      ),
      body
   ))
}

router <- make_router(
   default = route("/", page(htmlTemplate("www/pages/dashboard.html"), "Dashboard"), dashboardReady),
   route("data-overview", page(htmlTemplate("www/pages/data-overview.html"), "Datenübersicht"), dataOverviewReady),
   route("population", page(htmlTemplate("www/pages/population.html"), "Bevölkerungsentwicklung"), populationReady),
   route("migration", page(htmlTemplate("www/pages/migration.html"), "Einwanderung"), migrationReady),
   route("dependency-rate",  page(htmlTemplate("www/pages/dependency-rate.html"), "Abhängigkeitsquote"), dependencyRateReady),
   route("dependency-rate-forecast",  page(htmlTemplate("www/pages/dependency-rate-forecast.html"), "Abhängigkeitsquote prognose"), dependencyRateForecastReady),
   route("faq", page(htmlTemplate("www/pages/faq.html"), "FAQ"), NaN),
   page_404 = page404(
      message404 = "Sorry, we could not display this page!"
   )
)

# Make output for our router in main UI of Shiny app.
ui <- shinyUI(fluidPage(
   tags$html(
      tags$head <- htmlTemplate("www/parts/head.html"),
      tags$body(
         htmlTemplate("www/parts/header.html"),
         htmlTemplate("www/parts/left-menu.html"),
         tags$main(id="main", class="main",
             router$ui   
         ),
         htmlTemplate("www/parts/body-script.html")
      )
   )
))

server <- function(input, output, session) {
   router$server(input, output, session)
}

# Run the app ----
shinyApp(ui = ui, server = server)
