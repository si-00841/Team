setwd("C:/Rcode/ocean_project")

library(shiny)

source("C:/Rcode/ocean_project/global.R")
source("C:/Rcode/ocean_project/ui.R")
source("C:/Rcode/ocean_project/server.R")

shinyApp(ui=ui, server=server)