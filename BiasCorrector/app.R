library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(ggplot2)
library(magrittr)
library(polynom)


# setup
setup <- function(){
  # options(shiny.port = 1234)
  # options(shiny.host = "0.0.0.0")
  # options(shiny.launch.browser = FALSE)
  
  # initialize logfile here
  logfilename <<- paste0("./biascorrector.log")
  suppressMessages(suppressWarnings(file.create(logfilename)))
}

setup()


# app entrypoint here
shinyAppDir("app")


# TODOs:
# TODO write file requirements FAQ (transform all constraints within this algorithm to human readable format)