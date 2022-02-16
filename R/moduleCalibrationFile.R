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

#' @title module_calibrationfile_server
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive
#'   expression: input_re = reactive({input})
#' @param ... Further arguments, such as `logfilename`, `csvdir` and `plotdir`
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
#'   module_calibrationfile_server,
#'   "moduleCalibrationFile",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_calibrationfile_server
module_calibrationfile_server <- function(input,
                                          output,
                                          session,
                                          rv,
                                          input_re,
                                          ...) {
  arguments <- list(...)
  # error handling with fileimport
  observeEvent(
    eventExpr = {
      req(isTRUE(rv$type2cal_uploaded) || isTRUE(rv$type1cal_uploaded))
    },
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0("(app) Entered observeEvent after fileimport of ",
                         "calibration file"),
        logfilename = arguments$logfilename
      )
      # if type 1 data
      if (rv$type_locus_sample == "1") {
        # check here, if there have been deleted rows containing missin values
        tryCatch(#
          expr = {
            omitnas_modal(rv$omitnas, "calibration")
            rv$omitnas <- NULL
          },
          error = function(e) {
            e
          }
        )
        output$calibration_data <- renderUI({
          # the prefix "moduleCalibrationFile" is necessary, otherwise,
          # one is not able to load the datatable here
          DT::dataTableOutput("moduleCalibrationFile-dt1")
        })
        output$dt1 <- DT::renderDataTable({

          DT::datatable(rv$fileimport_calibration,
                        options = list(scrollX = TRUE,
                                       pageLength = 20,
                                       dom = "ltip",
                                       rowCallback = DT::JS(rv$row_callback)),
                        rownames = FALSE) %>%
            DT::formatRound(columns = c(2:ncol(rv$fileimport_calibration)),
                            digits = 3)
        })
        output$cal_samples <- reactive({
          len <- unique(rv$fileimport_calibration[, get("true_methylation")])
          message <- paste0("Number of unique calibration samples: ",
                            length(len))
          rBiasCorrection::write_log(message = message,
                                     logfilename = arguments$logfilename)
          message
        })
        output$cal_samples_raw <- reactive({
          len <- unique(rv$fileimport_calibration[, get("true_methylation")])
          message <- paste0("Unique calibration steps (% methylation):\n",
                            paste(len, collapse = "\n"))
          rBiasCorrection::write_log(message = message,
                                     logfilename = arguments$logfilename)
          message
        })

        # aggregated data
        output$calibration_data_aggregated <- DT::renderDataTable({
          DT::datatable(rv$aggregated_calibration,
                        options = list(scrollX = TRUE,
                                       pageLength = 20,
                                       dom = "ltip",
                                       rowCallback = DT::JS(rv$row_callback)),
                        rownames = FALSE) %>%
            DT::formatRound(columns = c(3:ncol(rv$aggregated_calibration)),
                            digits = 3)
        })

        # Download calibration data
        output$download_calibration <- downloadHandler(
          filename = function() {
            paste0("raw_calibration_data.csv")
          },
          content = function(file) {
            rBiasCorrection::write_csv(rv$fileimport_calibration, file)
          },
          contentType = "text/csv"
        )

        # Download aggregated calibration data
        output$download_calibration_aggr <- downloadHandler(
          filename = function() {
            paste0("aggregated_calibration_data.csv")
          },
          content = function(file) {
            rBiasCorrection::write_csv(rv$aggregated_calibration, file)
          },
          contentType = "text/csv"
        )
        # if type 2 data
      } else if (rv$type_locus_sample == "2") {
        # render assignment of calibration steps
        output$calibration_data <- renderUI({
          select_output_list <- lapply(seq_len(
            nrow(rv$calibr_steps)
          ), function(g) {
            selectname <- paste0("select", g)
            div(
              class = "row",
              div(
                class = "col-sm-6", style = "text-align: left",
                h5(tags$b(paste0(rv$calibr_steps[g, get("name")], ":")))
              ),
              div(
                class = "col-sm-6", style = "text-align: center",
                numericInput(
                  inputId = selectname,
                  min = 0,
                  max = 100,
                  label = NULL,
                  step = 0.01,
                  value = rv$calibr_steps[g, get("step")],
                  width = "100%"
                )
              ),
              tags$hr(style = "margin: 0.5%")
            )
          })
          select_output_list <- list(
            select_output_list,
            div(
              class = "row", style = "text-align: center",
              actionButton("confirm_steps",
                           "Confirm assignment of calibration steps")
            )
          )
          do.call(tagList, select_output_list)
        })
        output$cal_samples <- reactive({
          message <- paste0("Unique calibration samples: ",
                            nrow(rv$calibr_steps))
          rBiasCorrection::write_log(message = message,
                                     logfilename = arguments$logfilename)
          message
        })
        output$cal_samples_raw <- reactive({
          message <- paste0("Unique calibration steps:\n",
                            paste(levels(
                              factor(rv$calibr_steps[, get("step")])
                            ),
                            collapse = "\n"))
          rBiasCorrection::write_log(message = message,
                                     logfilename = arguments$logfilename)
          message
        })
      }
    }
  )

  # confirm-Button for Type2-Data
  observeEvent(input_re()$confirm_steps, {
    rv$choices_list <- data.table::data.table(
      "name" = character(),
      "step" = numeric()
    )
    lapply(seq_len(nrow(rv$calibr_steps)), function(g) {
      selectname <- paste0("select", g)
      rv$choices_list <- rbind(
        rv$choices_list,
        cbind("name" = rv$calibr_steps[g, get("name")],
              "step" = as.numeric(
                eval(parse(text = paste0("input_re()$", selectname))))
        )
      )
    })
    message(rv$choices_list)
    # assign rv$fileimport_calibration
    filecheck <- type2_fileconfirm(rv$fileimport_list,
                                   rv$choices_list,
                                   rv)
    if (is.character(filecheck)) {
      open_modal(filecheck, rv)
    } else {
      # store correct formatted calibration data in reactive list
      rv$fileimport_calibration <- filecheck
      removeUI(selector = "#moduleCalibrationFile-calibration_data",
               immediate = TRUE)
      # create reactive selectinput:
      sel_in <- reactive({
        selectInput(inputId = "selectType2",
                    label = "Select locus:",
                    multiple = FALSE,
                    selectize = FALSE,
                    choices = names(rv$fileimport_calibration))
      })
      # create reactive df-selection:
      df <- reactive({
        temp <- rv$fileimport_calibration[[input_re()$selectType2]]
      })
      output$calibration_select <- renderUI({
        s <- sel_in()
        do.call(tagList, list(tags$hr(), s))
      })
      # render the UI output
      output$calibration_data2 <- renderUI({
        output$dt2 <- DT::renderDataTable({
          temp <- df()
          DT::datatable(temp,
                        options = list(scrollX = TRUE,
                                       pageLength = 20,
                                       dom = "ltip",
                                       rowCallback = DT::JS(rv$row_callback)),
                        rownames = FALSE) %>%
            DT::formatRound(columns = c(2:ncol(temp)), digits = 3)
        })
        # merge selectInput and dataframe to list
        output_list <- list(DT::dataTableOutput("moduleCalibrationFile-dt2"))
        # print out list!
        do.call(tagList, output_list)
      })
      # Download experimental data
      output$download_calibration <- downloadHandler(
        filename = function() {
          paste0("raw_calibration_data_",
                 gsub("[[:punct:]]", "", input_re()$selectType2),
                 ".csv")
        },
        content = function(file) {
          rBiasCorrection::write_csv(df(), file)
        },
        contentType = "text/csv"
      )
    }
  })
}



#' @title module_calibrationfile_ui
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
#'     tabName = "calibration",
#'     module_calibrationfile_ui(
#'       "moduleCalibrationFile"
#'     )
#'   )
#' )
#' }
#'
#' @export
#'
# module_calibrationfile_ui
module_calibrationfile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        9,
        box(
          tabsetPanel(
            tabPanel(
              "Calibration Data",
              uiOutput(ns("calibration_data")),
              uiOutput(ns("calibration_data2"))
            ),
            tabPanel(
              "Aggregated Calibration Data",
              DT::dataTableOutput(ns("calibration_data_aggregated"))
            )
          ),
          width = 12
        )
      ),
      column(
        3,
        box(verbatimTextOutput(ns("cal_samples")),
            verbatimTextOutput(ns("cal_samples_raw")),
            tags$head(tags$style(paste0("#cal_samples_raw{overflow-y:scroll; ",
                                        "max-height: 10vh; ",
                                        "background: ghostwhite;}"))),
            uiOutput(ns("calibration_select")),
            tags$hr(),
            div(class = "row",
                style = "text-align: center;",
                shinyjs::disabled(
                  downloadButton(ns("download_calibration"),
                                 "Download calibration file",
                                 style = paste0(
                                   "white-space: normal; ",
                                   "text-align:center; ",
                                   "padding: 9.5px 9.5px 9.5px 9.5px; ",
                                   "margin: 6px 10px 6px 10px;")
                  ))),
            tags$hr(),
            div(class = "row",
                style = "text-align: center;",
                downloadButton(ns("download_calibration_aggr"),
                               "Download aggregated calibration file",
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
