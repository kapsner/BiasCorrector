library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(data.table)
library(ggplot2)
library(magrittr)
library(polynom)
library(ggpubr)
library(ggsci)

# app entrypoint here
shinyAppDir("app")


# TODOs:
# TODO write file requirements FAQ (transform all constraints within this algorithm to human readable format)
# TODO maybe remove rowmeans from plots, when CpG sites = 1
# TODO link readme to github page
# TODO in the end, generate markdown pdf with plots in temporary directory
# TODO check weired error, when correcting calibration data (corrected results), 
## especially with true_methylation==0
# TODO add demo-data
# TODO in type one data add checkbox, to select either all hyperbolic or all cubic at once

# for package:
# imports: ggplot2, data.table
