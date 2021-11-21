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
                     "hrbrthemes")

# try to load packages and install missing ones
for (package in required_packages) {
  # require tries to load a package, and returns a boolean indicating success
  if (!require(package, character.only = TRUE)) {
    install.packages(package , dependencies = TRUE)
    require(package, character.only = TRUE)
  }
}


# Plots
source("plots/utilities/population.service.R")
source('plots/migration.R')
source('plots/employment-rate.R')
source('plots/population-age-group.R')
source('plots/population-age-range.R')
source('plots/population-age-range-wave.R')
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
      tags$html(
         tags$head <- htmlTemplate("www/parts/head.html"),
         tags$body(
            htmlTemplate("www/parts/header.html"),
            htmlTemplate("www/parts/left-menu.html"),
            tags$main(id="main", class="main",
                      tags$div(class="pagetitle",
                            tags$h1(title)
                      ),
                      body
            ),
            htmlTemplate("www/parts/body-script.html")
         )
      )
   ))
}

router <- make_router(
   default = route("/", page(htmlTemplate("www/pages/dashboard.html"), "Dashboard"), dashboardReady),
   route("data-overview", page(htmlTemplate("www/pages/data-overview.html"), "Datenübersicht"), dataOverviewReady),
   route("population", page(htmlTemplate("www/pages/population.html"), "Bevölkerungsentwicklung"), populationReady),
   route("migration", page(htmlTemplate("www/pages/migration.html"), "Einwanderung"), migrationReady),
   route("dependency-rate",  page(htmlTemplate("www/pages/dependency-rate.html"), "Abhängigenquotient"), dependencyRateReady),
   route("dependency-rate-forecast",  page(htmlTemplate("www/pages/dependency-rate-forecast.html"), "Abhängigenquotient prognose"), dependencyRateForecastReady),
   route("faq", page(htmlTemplate("www/pages/faq.html"), "FAQ"), NaN)
)

ui <- basicPage(
   router$ui
)

server <- function(input, output, session) {
   router$server(input, output, session)
}

# Run the app ----
shinyApp(ui = ui, server = server)