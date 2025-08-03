# ------------------------------------------------------------------------------
# Author: Giulia Sepeda
# GitHub: https://github.com/giuusepeda
# Project: shiny_substance_use_winnipeg
# File: app.R
# Created: 26/07/2025
# Description: shiny app file for dashboard drug use in Winnipeg, CA
#
# 🚫 This code is part of a personal portfolio project.
# Unauthorized use, copying, or distribution is prohibited.
# For inquiries: giuliasepeda@gmail.com
# ------------------------------------------------------------------------------


library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Substance Use Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Incident Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Service Map", tabName = "map", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview", h2("Incident Overview")),
      tabItem("map", h2("Service Map")),
      tabItem("about", h2("About"))
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
