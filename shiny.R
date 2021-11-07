### Install required packages
if (!require("rio")) install.packages("rio"); library(rio)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("shiny")) install.packages("shiny"); library(shiny)
if (!require("quantmod")) install.packages("quantmod"); library(quantmod)
if (!require("plotly")) install.packages("plotly"); library(plotly)
if (!require("shiny.router")) install.packages("shiny.router"); library(shiny.router)
if (!require("zoo")) install.packages('zoo'); library(zoo)

# Plots
source('plots/migration.R')
source('plots/employment-rate.R')
# Routes
source('route-pages/dashboard.R')
source('route-pages/inputIndicators.R')
source('route-pages/outputIndicators.R')
source('route-pages/population.R')


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
   route("population", page(htmlTemplate("www/pages/population.html"), "Bevölkerungsentwicklung"), populationReady),
   route("input-indicators", page(htmlTemplate("www/pages/input-indicators.html"), "input-indicators"), inputIndicatorsReady),
   route("output-indicators",  page(htmlTemplate("www/pages/output-indicators.html"), "output-indicators"), outputIndicatorsReady),
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