# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
# Copyright (C) 2019-2020 Lorenz Kapsner
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


#' @title module_experimentalfile_server
#'
#' @inheritParams module_calibrationfile_server
#'
#' @export
#'
# module_experimentalfile_server
module_experimentalfile_server <- function(input,
                                           output,
                                           session,
                                           rv,
                                           ...) {
  arguments <- list(...)
  # error handling with fileimport
  observeEvent(
    eventExpr = {
      if (!is.null(rv$fileimport_experimental)) {
        TRUE
      } else {
        return()
      }
    },
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0("(app) Entered observeEvent after fileimport ",
                         "of experimental file"),
        logfilename = arguments$logfilename
      )
      # if type 1 data
      if (rv$type_locus_sample == "1") {
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimport_experimental,
                        options = list(scrollX = TRUE,
                                       pageLength = 20,
                                       dom = "ltip"),
                        rownames = F) %>%
            DT::formatRound(columns = c(2:ncol(rv$fileimport_experimental)),
                            digits = 3)
        })
        output$exp_samples <- reactive({
          len <- unique(rv$fileimport_experimental[, get("sample_id")])
          message <- paste0("Number of unique samples: ",
                            length(len))
          rBiasCorrection::write_log(message = message,
                                     logfilename = arguments$logfilename)
          message
        })
        output$exp_samples_raw <- reactive({
          len <- sort(unique(rv$fileimport_experimental[, get("sample_id")]))
          message <- paste0("Unique sample IDs:\n",
                            paste(len, collapse = "\n"))
          rBiasCorrection::write_log(message = message,
                                     logfilename = arguments$logfilename)
          message
        })
        # if type 2 data
      } else if (rv$type_locus_sample == "2") {
        output$experimental_data <- DT::renderDataTable({
          # https://stackoverflow.com/questions/58526047/customizing-how-
          # datatables-displays-missing-values-in-shiny
          row_callback <- c(
            "function(row, data) {",
            "  for(var i=0; i<data.length; i++) {",
            "    if(data[i] === null) {",
            "      $('td:eq('+i+')', row).html('NA')",
            "        .css({",
            "'color': 'rgb(151,151,151)',",
            "'font-style': 'italic'});",
            "    }",
            "  }",
            "}"
          )

          DT::datatable(rv$fileimport_experimental,
                        options = list(scrollX = TRUE,
                                       pageLength = 20,
                                       dom = "ltip",
                                       rowCallback = DT::JS(row_callback)),
                        rownames = F) %>%
            DT::formatRound(columns = c(2:ncol(rv$fileimport_experimental)),
                            digits = 3)
        })
        output$exp_samples <- reactive({
          len <- unique(rv$fileimport_experimental[, get("locus_id")])
          message <- paste0("Number of unique loci: ",
                            length(len))
          rBiasCorrection::write_log(message = message,
                                     logfilename = arguments$logfilename)
          message
        })
        output$exp_samples_raw <- reactive({
          len <- sort(unique(rv$fileimport_experimental[, get("locus_id")]))
          message <- paste0("Unique locus IDs:\n",
                            paste(len, collapse = "\n"))
          rBiasCorrection::write_log(message = message,
                                     logfilename = arguments$logfilename)
          message
        })
      }
      # Download experimental data
      output$download_experimental <- downloadHandler(
        filename = function() {
          paste0("raw_experimental_data.csv")
        },
        content = function(file) {
          rBiasCorrection::write_csv(table = rv$fileimport_experimental,
                                     filename = file)
        },
        contentType = "text/csv"
      )
    }
  )
}


#' @title module_experimentalfile_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_experimentalfile_ui
module_experimentalfile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        9,
        box(
          title = "Experimental Data",
          DT::dataTableOutput(ns("experimental_data")),
          width = 12
        )
      ),
      column(
        3,
        box(verbatimTextOutput(ns("exp_samples")),
            verbatimTextOutput(ns("exp_samples_raw")),
            tags$head(
              tags$style(
                paste0("#exp_samples_raw{overflow-y:scroll; ",
                       "max-height: 10vh; background: ghostwhite;}"))),
            tags$hr(),
            div(class = "row",
                style = "text-align: center",
                downloadButton(
                  ns("download_experimental"),
                  "Download experimental file",
                  style = paste0(
                    "white-space: normal; ",
                    "text-align:center; ",
                    "padding: 9.5px 9.5px 9.5px 9.5px; ",
                    "margin: 6px 10px 6px 10px;")
                )),
            tags$hr(),
            width = 12
        )
      )
    )
  )
}
