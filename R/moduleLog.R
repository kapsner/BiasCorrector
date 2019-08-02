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


#' @title moduleLogServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleLogServer
moduleLogServer <- function(input, output, session, rv, input_re){
  # logfileviewer
  observe({
    file <- reactiveFileReader(500, session,
                               logfilename,
                               readLines)
    rv$logfile <- file()

    output$downloadLogfile <- downloadHandler(
      filename = function(){
        paste0("BC_logfile.txt")
      },
      content = function(file){
        write(rv$logfile, file)
      },
      contentType = "text/csv"
    )
  })

  output$log_out <- reactive({
    paste(paste0(rv$logfile, collapse = "\n"))
  })
}


#' @title moduleLogUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleLogUI
moduleLogUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        title = "Log",
        verbatimTextOutput(ns("log_out")),
        tags$head(tags$style("#moduleLog-log_out{overflow-y:scroll; max-height: 80vh; background: ghostwhite;}")),
        width = 9
      ),
      box(title = "Download Log File",
          div(class="row", style="text-align: center;", shinyjs::disabled(downloadButton(ns("downloadLogfile"),
                                                                                         "Download Log File",
                                                                                         style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))),
          tags$hr(),
          width=3
      ))
  )
}
