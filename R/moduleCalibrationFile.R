# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
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

#' @title moduleCalibrationFileServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleCalibrationFileServer
moduleCalibrationFileServer <- function(input, output, session, rv, input_re){
  # error handling with fileimport
  observeEvent({
    if (isTRUE(rv$type2cal_uploaded) | isTRUE(rv$type1cal_uploaded)) TRUE
    else return()}, {
      PCRBiasCorrection::writeLog_("(app) Entered observeEvent after fileimport of calibration file", logfilename = logfilename)

      # if type 1 data
      if (rv$type_locus_sample == "1"){

        # check here, if there have been deleted rows containing missin values
        tryCatch({
          omitnasModal(rv$omitnas, "calibration")
          rv$omitnas <- NULL
        }, error = function(e){
          print(e)
        })

        # output$dt1 <- DT::renderDataTable({
        #   DT::datatable(rv$fileimportCal, options = list(scrollX = TRUE, pageLength = 20)) %>%
        #     formatRound(columns=c(2:ncol(rv$fileimportCal)), digits=3)
        # })

        output$calibration_data <- renderUI({
          # the prefix "moduleCalibrationFile" is necessary, otherwise, one is not able to load the datatable here
          DT::dataTableOutput("moduleCalibrationFile-dt1")
        })

        output$dt1 <- DT::renderDataTable({
          DT::datatable(rv$fileimportCal, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
            DT::formatRound(columns=c(2:ncol(rv$fileimportCal)), digits=3)
        })

        output$cal_samples <- reactive({
          len <- unique(rv$fileimportCal[,get("true_methylation")])
          message <- paste0("Number of unique calibration samples: ", length(len))
          PCRBiasCorrection::writeLog_(message, logfilename = logfilename)
          message
        })

        output$cal_samples_raw <- reactive({
          len <- unique(rv$fileimportCal[,get("true_methylation")])
          message <- paste0("Unique calibration steps (% methylation):\n", paste(len, collapse = "\n"))
          PCRBiasCorrection::writeLog_(message, logfilename = logfilename)
          message
        })

        # Download experimental data
        output$downloadCalibration <- downloadHandler(
          filename = function(){
            paste0("raw_calibration_data.csv")
          },
          content = function(file){
            PCRBiasCorrection::writeCSV_(rv$fileimportCal, file)
          },
          contentType = "text/csv"
        )

        # if type 2 data
      } else if (rv$type_locus_sample == "2"){

        # render assignment of calibration steps
        output$calibration_data <- renderUI({
          select_output_list <- lapply(1:nrow(rv$calibr_steps), function(g) {
            selectname <- paste0("select", g)
            div(class="row",
                div(class="col-sm-6", style="text-align: left",
                    h5(tags$b(paste0(rv$calibr_steps[g, get("name")], ":")))),
                div(class="col-sm-6", style="text-align: center",
                    numericInput(inputId = selectname,
                                 min = 0,
                                 max = 100,
                                 label = NULL,
                                 step = 0.01,
                                 value = rv$calibr_steps[g, get("step")],
                                 width = "100%")),
                tags$hr(style="margin: 0.5%"))
          })
          select_output_list <- list(select_output_list,
                                     div(class="row", style="text-align: center",
                                         actionButton("confirm_steps", "Confirm assignment of calibration steps")
                                     ))
          do.call(tagList, select_output_list)
        })

        output$cal_samples <- reactive({
          message <- paste0("Unique calibration samples: ", nrow(rv$calibr_steps))
          PCRBiasCorrection::writeLog_(message, logfilename = logfilename)
          message
        })

        output$cal_samples_raw <- reactive({
          message <- paste0("Unique calibration steps:\n", paste(levels(factor(rv$calibr_steps[,get("step")])), collapse = "\n"))
          PCRBiasCorrection::writeLog_(message, logfilename = logfilename)
          message
        })
      }
    })

  # confirm-Button for Type2-Data
  observeEvent(input_re()$confirm_steps, {
    rv$choices_list <- data.table::data.table("name" = character(), "step" = numeric())
    lapply(1:nrow(rv$calibr_steps), function(g) {
      selectname <- paste0("select", g)
      rv$choices_list <- rbind(rv$choices_list, cbind("name" = rv$calibr_steps[g,get("name")], "step" = as.numeric(eval(parse(text=paste0("input_re()$", selectname))))))
    })
    print(rv$choices_list)

    # assign rv$fileimportCal
    filecheck <- type2FileConfirm(rv$fileimportList, rv$choices_list, rv)
    if (is.character(filecheck)){
      openModal(filecheck, rv)
    } else {

      # store correct formatted calibration data in reactive list
      rv$fileimportCal <- filecheck
      removeUI(selector = "#moduleCalibrationFile-calibration_data", immediate = T)

      # create reactive selectinput:
      selIn <- reactive({
        selectInput(inputId="selectType2", label = "Select locus:", multiple = F, selectize = F, choices = names(rv$fileimportCal))
      })

      # create reactive df-selection:
      df <- reactive({
        temp <- rv$fileimportCal[[input_re()$selectType2]]
      })

      output$calibration_select <- renderUI({
        s <- selIn()
        do.call(tagList, list(tags$hr(), s))
      })

      # render the UI output
      output$calibration_data2 <- renderUI({

        output$dt2 <- DT::renderDataTable({
          temp <- df()
          DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
            DT::formatRound(columns=c(2:ncol(temp)), digits=3)
        })

        # merge selectInput and dataframe to list
        output_list <- list(DT::dataTableOutput("moduleCalibrationFile-dt2"))

        # print out list!
        do.call(tagList, output_list)
      })

      # Download experimental data
      output$downloadCalibration <- downloadHandler(
        filename = function(){
          paste0("raw_calibration_data_", gsub("[[:punct:]]", "", input_re()$selectType2), ".csv")
        },
        content = function(file){
          PCRBiasCorrection::writeCSV_(df(), file)
        },
        contentType = "text/csv"
      )
    }
  })
}



#' @title moduleCalibrationFileUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleCalibrationFileUI
moduleCalibrationFileUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(9,
             box(title = "Calibration Data",
                 uiOutput(ns("calibration_data")),
                 uiOutput(ns("calibration_data2")),
                 width = 12
             )),
      column(3,
             box(verbatimTextOutput(ns("cal_samples")),
                 verbatimTextOutput(ns("cal_samples_raw")),
                 tags$head(tags$style("#cal_samples_raw{overflow-y:scroll; max-height: 10vh; background: ghostwhite;}")),
                 uiOutput(ns("calibration_select")),
                 tags$hr(),
                 div(class="row", style="text-align: center;", shinyjs::disabled(downloadButton(ns("downloadCalibration"),
                                                                                               "Download calibration file",
                                                                                               style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))),
                 tags$hr(),
                 width = 12
             )
      )
    )
  )


}
