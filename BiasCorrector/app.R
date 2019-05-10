library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(data.table)
library(ggplot2)
library(magrittr)
library(polynom)

# app entrypoint here
shinyAppDir("app")


# TODOs:
# TODO write file requirements FAQ (transform all constraints within this algorithm to human readable format)
# TODO maybe remove rowmeans from plots, when CpG sites = 1
# TODO link readme to github page
# TODO in the end, generate markdown pdf with plots in temporary directory


# for package:
# imports: ggplot2, data.table
