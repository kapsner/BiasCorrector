# BiasCorrector: Correct PCR-bias in DNA methylation analyses
# Copyright (C) 2019 Lorenz Kapsner
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
