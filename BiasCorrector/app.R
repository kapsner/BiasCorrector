library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(ggplot2)
library(magrittr)
library(polynom)

# source functions
source("Functions.R", echo = F, encoding = "UTF-8")
source("App_Utilities.R", echo = F, encoding = "UTF-8")
source("app/ui.R", echo = F, encoding = "UTF-8")
source("app/server.R", echo = F, encoding = "UTF-8")

setup()

# TODO write file requirements FAQ (transform all constraints within this algorithm to human readable format)

# maximum filesize in MB:
maxfilesize <- 100
options(shiny.maxRequestSize = maxfilesize*1024^2)

shinyApp(ui, server)