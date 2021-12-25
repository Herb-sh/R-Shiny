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
                     
                     "gridExtra",
                     "prophet",
                     "readxl",
                     "DT",
                     "manipulateWidget"
                    )

# try to load packages and install missing ones
for (package in required_packages) {
   # require tries to load a package, and returns a boolean indicating success
   if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
   }
}

# Authors: Herbi Shtini & Anitta Weiss

# Plots
source("plots/utilities/population.service.R")
source('plots/migration.plot.R')
source('plots/population-age-group.plot.R')
source('plots/population-age-range.plot.R')
source('plots/population-age-forecast.plot.R')
source('plots/dependency-rate-forecast.plot.R')
source('plots/population-age-range-wave.plot.R')
# Routes
source('routes/dashboard.R')
source('routes/data-overview.R')
source('routes/population.R')
source('routes/population-forecast.R')
source('routes/dependency-rate.R')
source('routes/dependency-rate-forecast.R')
source('routes/migration.R')
source('routes/migration-forecast.R')


page <- function(body, title) {
   return(fluidPage(
      tags$div(class="pagetitle",
         tags$h1(title)
      ),
      body
   ))
}

router <- make_router(
   default = route("/", page(htmlTemplate("www/pages/dashboard.html"), ""), dashboardReady),
   route("data-overview", page(htmlTemplate("www/pages/data-overview.html"), "Datenübersicht"), dataOverviewReady),
   route("population", page(htmlTemplate("www/pages/population.html"), "Bevölkerungsentwicklung"), populationReady),
   route("migration", page(htmlTemplate("www/pages/migration.html"), "Migrationsrate"), migrationReady),
   route("dependency-rate",  page(htmlTemplate("www/pages/dependency-rate.html"), "Abhängigenquotient"), dependencyRateReady),
   route("dependency-rate-forecast",  page(htmlTemplate("www/pages/dependency-rate-forecast.html"), ""), dependencyRateForecastReady), # Abhängigenquotient Prognose
   route("faq", page(htmlTemplate("www/pages/faq.html"), "FAQ"), NaN),
   page_404 = page404(
      message404 = "Sorry, we could not display this page!"
   )
)

# Generating UI with dynamic current page content in it(router$ui).
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