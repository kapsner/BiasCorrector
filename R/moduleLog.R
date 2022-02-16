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


#' @title module_log_server
#'
#' @inheritParams module_calibrationfile_server
#'
#' @return The function returns a shiny server module.
#'
#' @seealso \url{https://shiny.rstudio.com/articles/modules.html}
#'
#' @examples
#' if (interactive()) {
#' rv <- list()
#' logfilename <- paste0(tempdir(), "/log.txt")
#' shiny::callModule(
#'   module_log_server,
#'   "moduleLog",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_log_server
module_log_server <- function(input,
                              output,
                              session,
                              rv,
                              input_re,
                              ...) {
  arguments <- list(...)

  # logfileviewer
  observe({
    file <- reactiveFileReader(500, session,
                               arguments$logfilename,
                               readLines)
    rv$logfile <- file()

    output$download_logfile <- downloadHandler(
      filename = function() {
        paste0("BC_logfile.txt")
      },
      content = function(file) {
        write(rv$logfile, file)
      },
      contentType = "text/csv"
    )
  })

  output$log_out <- reactive({
    paste(paste0(rv$logfile, collapse = "\n"))
  })
}


#' @title module_log_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @return The function returns a shiny ui module.
#'
#' @seealso \url{https://shiny.rstudio.com/articles/modules.html}
#'
#' @examples
#' if (interactive()) {
#' shinydashboard::tabItems(
#'   shinydashboard::tabItem(
#'     tabName = "log",
#'     module_log_ui(
#'       "moduleLog"
#'     )
#'   )
#' )
#' }
#'
#' @export
#'
# module_log_ui
module_log_ui <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    box(
      title = "Log",
      verbatimTextOutput(ns("log_out")),
      tags$head(tags$style(
        paste0(
          "#moduleLog-log_out{overflow-y:scroll; ",
          "max-height: 70vh; background: ghostwhite;}"
        )
      )),
      width = 9
    ),
    box(
      title = "Download Log File",
      div(
        class = "row",
        style = "text-align: center;",
        shinyjs::disabled(downloadButton(
          ns("download_logfile"),
          "Download Log File",
          paste0(
            "white-space: normal; ",
            "text-align:center; ",
            "padding: 9.5px 9.5px 9.5px 9.5px; ",
            "margin: 6px 10px 6px 10px;"
          )
        ))
      ),
      tags$hr(),
      width = 3
    )
  ))
}
