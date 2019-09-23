# BiasCorrector: A GUI to Correct PCR Bias in DNA Methylation Analyses
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


#' @title Launch BiasCorrector
#'
#' @param port The port, BiasCorrector is running on (default: 3838)
#' @param logfilename A character string. The name of the logfile (default = biascorrector.log).
#' @param tempdir A character string. Defaults to `tempdir()`.
#' @param plotdir A character string. Defaults to 'plots'. This directory is being created inside tempdir.
#' @param csvdir A character string. Defaults to 'csv'. This directory is being created inside tempdir.
#'
#' @return BiasCorrector shiny application
#'
#' @import shiny shinydashboard
#' @importFrom magrittr "%>%"
#' @importFrom data.table .N ":="
#'
#' @export
#'
launchApp <- function(port=3838, logfilename = "biascorrector.log", tempdir = tempdir(), plotdir = "plots", csvidr = "csv"){
  
  # directories
  tempdir <- tempdir()
  plotdir <- paste0(tempdir, "/", plotdir)
  csvdir <- paste0(tempdir, "/", csvdir)
  
  # logfilename
  logfilename <- paste0(tempdir, "/", logfilename)
  
  # maximum filesize in MB
  maxfilesize <- maxfilesize
  
  # set shiny option here
  options(shiny.maxRequestSize = maxfilesize*1024^2)
  options(shiny.port = port)
  
  shiny::shinyAppDir(appDir = system.file("application", package = "BiasCorrector"))
}
