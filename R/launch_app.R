# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
# Copyright (C) 2019-2022 Lorenz Kapsner
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
#' @param maxfilesize A positive integer. The maximum file size allowed
#'   for upload.
#' @param logfilename A character string. The name of the logfile
#'   (default = biascorrector.log).
#' @param plotdir A character string. Defaults to 'plots'. This directory
#'   is being created inside tempdir.
#' @param csvdir A character string. Defaults to 'csv'. This directory is
#'   being created inside tempdir.
#' @param parallel A boolean. If TRUE (default), initializing
#'   `future::plan("multiprocess")` before running the code.
#'
#' @return The function returns the BiasCorrector shiny application.
#'
#' @import shiny shinydashboard
#' @importFrom magrittr "%>%"
#' @importFrom data.table .N ":="
#'
#' @examples
#' if (interactive()) {
#' launch_app()
#' }
#'
#' @export
#'
launch_app <- function(port = 3838,
                       plotdir = "plots",
                       csvdir = "csv",
                       logfilename = "biascorrector.log",
                       maxfilesize = 100,
                       parallel = TRUE) {
  #" stopifnot(
  #"   is.numeric(maxfilesize),
  #"   maxfilesize > 0,
  #"   is.character(logfilename),
  #"   is.character(plotdir),
  #"   is.character(csvdir),
  #"   is.numeric(port)
  #" )
  tempdir <- tempdir()

  global_env_hack <- function(key,
                              val,
                              pos) {
    assign(
      key,
      val,
      envir = as.environment(pos)
    )
  }

  global_env_hack("tempdir",
                  tempdir,
                  1L)
  global_env_hack("plotdir",
                  paste0(tempdir, "/", plotdir, "/"),
                  1L)
  global_env_hack("csvdir",
                  paste0(tempdir, "/", csvdir, "/"),
                  1L)
  # logfilename
  global_env_hack("logfilename",
                  paste0(tempdir, "/", logfilename),
                  1L)
  # maximum filesize in MB
  global_env_hack("maxfilesize",
                  maxfilesize,
                  1L)
  global_env_hack("parallel",
                  parallel,
                  1L)

  # set shiny option here
  options(shiny.maxRequestSize = maxfilesize * 1024^2)
  options(shiny.port = port)

  shiny::shinyAppDir(
    appDir = system.file("application",
                         package = "BiasCorrector")
  )
}
