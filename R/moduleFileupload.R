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


#' @title module_fileupload_server
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
#'   module_fileupload_server,
#'   "moduleEileUpload",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_fileupload_server
module_fileupload_server <- function(input,
                                   output,
                                   session,
                                   rv,
                                   input_re,
                                   ...) {
  arguments <- list(...)
  # TODO original selection of data type
  # observe Radiobuttonevents
  #% observeEvent(
  #%   eventExpr = input_re()[["moduleFileupload-type_locus_sample"]],
  #%   handlerExpr = {
  #%     waround <- input_re()[["moduleFileupload-type_locus_sample"]]
  #%     rv$type_locus_sample <- waround
  #%   }
  #% )
  observe({
    req(rv$type_locus_sample)
    output$type_locus_sample <- reactive({
      return(TRUE)
    })
    outputOptions(output, "type_locus_sample", suspendWhenHidden = FALSE)
  })

  # loading example data
  observeEvent(
    eventExpr = input_re()[["moduleFileupload-load_example_data"]],
    handlerExpr = {
    # mimic normal fileimport
    # experimental file
    rv$exp_filereq <- T
    rv$sample_locus_name <- "Example_Locus"
    rBiasCorrection::write_log(
      message = paste0("Locus name: Example_Locus\n(--> stored as: ",
                       rv$sample_locus_name, ")"),
      logfilename = arguments$logfilename)
    waround_ex <- rBiasCorrection::example.data_experimental[["dat"]]
    rv$fileimport_experimental <- waround_ex

    # calibration file
    waround_ca <- rBiasCorrection::example.data_calibration[["dat"]]
    rv$fileimport_calibration <- waround_ca
    rv$vec_cal <- rBiasCorrection::example.data_calibration[["vec_cal"]]

    # calculate aggregated inputs
    rv$aggregated_experimental <- rBiasCorrection::aggregated_input(
      datatable = rv$fileimport_experimental,
      description = "experimental",
      vec_cal = rv$vec_cal,
      type = 1
    )

    rv$aggregated_calibration <- rBiasCorrection::aggregated_input(
      datatable = rv$fileimport_calibration,
      description = "calibration",
      vec_cal = rv$vec_cal
    )

    # set upload flag
    rv$type1cal_uploaded <- TRUE
  })


  # Experimental file
  observe({
    req(input_re()[["moduleFileupload-experimentalFile"]])

    if (isFALSE(rv$exp_filereq)) {
      rBiasCorrection::write_log(
        message = "(app) Entered observation for experimental file.",
        logfilename = arguments$logfilename)
      # check file ending
      rv$ending <- strsplit(
        input_re()[["moduleFileupload-experimentalFile"]]$name,
        ".",
        fixed = TRUE)[[1]]

      # if type 1 data
      if (rv$type_locus_sample == "1") {
        # render fileInput with option "multiple = F"
        output$fileinput_cal <- renderUI({
          fileInput("calibrationFile",
                    paste0("Please choose one CSV file containing ",
                           "the calibration DNA samples."),
            multiple = FALSE,
            accept = c(".csv", "text/csv")
          )
        })

        # check userinput of locusname
        if (input_re()[["moduleFileupload-locusname"]] == "") {
          open_modal("locusname", rv)
        } else {
          rv$exp_filereq <- T
          rv$sample_locus_name <- rBiasCorrection::handle_text_input(
            input_re()[["moduleFileupload-locusname"]])
          rBiasCorrection::write_log(
            message = paste0("Locus name: ",
                             input_re()[["moduleFileupload-locusname"]],
                             "\n(--> stored as: ",
                             rv$sample_locus_name, ")"),
            logfilename = arguments$logfilename)
          #% shinyjs::disable("moduleFileupload-locusname")
          #% removeUI(selector = "#locusname", immediate = T)
        }

        # if type 2 data
      } else if (rv$type_locus_sample == "2") {
        # render fileInput with option "multiple = TRUE"
        output$fileinput_cal <- renderUI({
          fileInput("calibrationFile",
                    paste0("Please choose at least 4 different CSV files ",
                           "containing the calibration data (one file per ",
                           "distinct calibration DNA sample; for specific ",
                           "file naming please refer to our FAQ)."),
            multiple = TRUE,
            accept = c(".csv", "text/csv")
          )
        })

        # check userinput of samplename
        if (input_re()[["moduleFileupload-samplename"]] == "") {
          open_modal("samplename", rv)
        } else {
          rv$exp_filereq <- T
          rv$sample_locus_name <- rBiasCorrection::handle_text_input(
            input_re()[["moduleFileupload-samplename"]]
            )
          rBiasCorrection::write_log(
            message = paste0("Sample name: ",
                             input_re()[["moduleFileupload-samplename"]],
                             "\n(--> stored as: ",
                             rv$sample_locus_name, ")"),
            logfilename = arguments$logfilename)
          #% shinyjs::disable("moduleFileupload-samplename")
          #% removeUI(selector = "#samplename", immediate = T)
        }
      }
    }

    if (rv$exp_filereq == TRUE && is.null(rv$fileimport_experimental)) {
      #% removeUI(selector = "#tag1", immediate = T)
      #% shinyjs::disable("moduleFileupload-type_locus_sample")

      if (rv$ending[2] %in% c("csv", "CSV")) {
        file <- reactiveFileReader(1000, session,
          input_re()[["moduleFileupload-experimentalFile"]]$datapath,
          data.table::fread,
          header = TRUE
        )
        tryCatch(
          expr = {
            rv$fileimport_experimental <- rBiasCorrection::clean_dt(
              datatable = file(),
              description = "experimental",
              type = rv$type_locus_sample,
              logfilename = arguments$logfilename
              )[["dat"]]

            #% updateTabItems(session, "tabs", "panel_1")
          },
          error = function(e) {
            e
            # error handling fileimport
            open_modal("experimentalFile", rv)
          }
        )

        # test, if we imported valid file
        if (is.null(rv$fileimport_experimental)) {
          # error handling fileimport
          open_modal("experimentalFile", rv)
        } else {
          # check here, if there have been deleted rows containing
          # missing values
          tryCatch(
            expr = {
              omitnas_modal(rv$omitnas, "experimental")
              rv$omitnas <- NULL
            },
            error = function(e) {
              rBiasCorrection::write_log(
                message = paste0("Errormessage: ", e),
                logfilename = arguments$logfilename)
            }
          )

          # workaround to tell ui, that experimental file is there
          output$file_uploaded <- reactive({
            return(TRUE)
          })
          outputOptions(output, "file_uploaded",
                        suspendWhenHidden = FALSE)
        }
      } else {
        # error handling fileimport
        open_modal("experimentalFile", rv)
      }
    }
  })


  # calibration file
  ###### Calibration data
  observe({
    req(input_re()[["calibrationFile"]])

    rv$ending <- NULL

    if (is.null(rv$fileimport_calibration) |
        is.null(rv$fileimport_list)) {
      # if calibration file is of data type 1
      if (rv$type_locus_sample == "1") {

        # check file ending
        rv$ending <- strsplit(
          input_re()[["calibrationFile"]]$name,
          ".",
          fixed = TRUE)[[1]]

        # if ending suggests it might be a csv file
        if (rv$ending[2] %in% c("csv", "CSV")) {
          file <- reactiveFileReader(1000, session,
            input_re()[["calibrationFile"]]$datapath,
            data.table::fread,
            header = TRUE
          )

          # try to import file
          tryCatch(
            expr = {
              if (is.null(rv$fileimport_calibration)) {
                cal_type_1 <- rBiasCorrection::clean_dt(
                  datatable = file(),
                  description = "calibration",
                  type = "1",
                  logfilename = arguments$logfilename)
                rv$fileimport_calibration <- cal_type_1[["dat"]]
                rv$vec_cal <- cal_type_1[["vec_cal"]]
              }
            },
            error = function(e) {
              e
              # error handling fileimport
              open_modal("calibrationFile", rv)
            }
          )

          # go on, if we imported valid file
          if (!is.null(rv$fileimport_calibration)) {

            # try to check, if colnames of experimental data are
            # same as those of calibration data
            tryCatch(
              expr = {
                # check, if colnames of experimental and calibration
                # data are equal:
                if (!all.equal(colnames(rv$fileimport_calibration)[-1],
                               colnames(rv$fileimport_experimental)[-1])) {
                  # error handling fileimport
                  open_modal("calibrationFile", rv)
                }
              },
              error = function(e) {
                e
                # error handling fileimport
                open_modal("calibrationFile", rv)
              }
            )

            # check here, if there are calibration steps
            # outside the range 0 <= CS <= 100
            if (rv$fileimport_calibration[, min(
              as.numeric(
                as.character(get("true_methylation")))
              )] < 0 ||
              rv$fileimport_calibration[, max(
                as.numeric(
                  as.character(get("true_methylation")))
                )] > 100) {
              open_modal("calibrange", rv)
            } else {

              # check here, if there have been deleted rows
              # containing missin values
              #% tryCatch(expr = {
              #%   omitnasModal(rv$omitnas, "calibration")
              #%   rv$omitnas <- NULL
              #% }, error = function(e) {
              #%   print(e)
              #% })

              # calculate aggregated inputs
              rv$aggregated_experimental <- rBiasCorrection::aggregated_input(
                datatable = rv$fileimport_experimental,
                description = "experimental",
                vec_cal = rv$vec_cal,
                type = 1
              )

              rv$aggregated_calibration <- rBiasCorrection::aggregated_input(
                datatable = rv$fileimport_calibration,
                description = "calibration",
                vec_cal = rv$vec_cal
              )

              rv$type1cal_uploaded <- TRUE
            }

            # if we have the value "NULL" in our file-variable;
            # this happens, when cleanDT returns error
          } else {
            # error handling fileimport
            open_modal("calibrationFile", rv)
          }

          # else, if ending is no csv-file
        } else {
          # error handling fileimport
          open_modal("calibrationFile", rv)
        }


        # if calibration file is of data type 2
      } else if (rv$type_locus_sample == "2") {
        if (isFALSE(rv$type2cal_uploaded)) {

          # loop through calibration files
          for (i in seq_len(nrow(input_re()[["calibrationFile"]]))) {
            # check file ending
            rv$ending <- strsplit(
              input_re()[["calibrationFile"]]$name[i],
              ".",
              fixed = TRUE
              )[[1]]

            file <- reactiveFileReader(1000, session,
              input_re()[["calibrationFile"]]$datapath[i],
              data.table::fread,
              header = TRUE
            )

            if (rv$ending[2] %in% c("csv", "CSV")) {
              waround_fup <- rBiasCorrection::clean_dt(
                datatable = file(),
                description = "calibration",
                type = "2",
                logfilename = arguments$logfilename
                )[["dat"]]
              ind <- input_re()[["calibrationFile"]]$name[i]
              rv$fileimport_list[[ind]] <- waround_fup
            } else {
              # error handling fileimport
              open_modal("csv", rv)
            }
          }

          # nolint start
          # chech type 2 file requirements here
          # filecheck <- rBiasCorrection::type2_filereq(
          #   filelist = rv$fileimport_list,
          #   rv = rv,
          #   logfilename = arguments$logfilename)
          #
          # if (is.character(filecheck)) {
          #   open_modal(filecheck, rv)
          # } else if (isTRUE(filecheck)) {
          #   rv$type2cal_uploaded <- TRUE
          # }
          # nolint end
        }
      }
    }
  })
}


#' @title module_fileupload_ui
#'
#' @param id A character. The identifier of the shiny object
#' @param ... Further arguments, such as `maxfilesize`
#'
#' @return The function returns a shiny ui module.
#'
#' @seealso \url{https://shiny.rstudio.com/articles/modules.html}
#'
#' @examples
#' if (interactive()) {
#' shinydashboard::tabItems(
#'   shinydashboard::tabItem(
#'     tabName = "fileupload",
#'     module_fileupload_ui(
#'       "moduleFileUpload",
#'       maxfilesize = maxfilesize
#'     )
#'   )
#' )
#' }
#'
#' @export
#'
# module_fileupload_ui
module_fileupload_ui <- function(id,
                                 ...) {
  arguments <- list(...)

  ns <- NS(id)

  tagList(
    fluidRow(
      # type of data box
      box(
        # TODO original selection of data type
        #% title = "Type of Data",
        #% # Radiobuttons: Type of data
        #% radioButtons(
        #%   inputId = ns("type_locus_sample"),
        #%   label = h5(paste0("Please specify the type of DNA methylation ",
        #%                     "data to be corrected for measurement biases")),
        #%   choices = list(
        #%     paste0("One locus in many samples ",
        #%            "(e.g. pyrosequencing data)") = 1,
        #%     paste0("Many loci in one sample ",
        #%            "(e.g. next-generation sequencing ",
        #%            "data or microarray data)") = 2),
        #%   selected = character(0)),
        #%
        #% tags$hr(),
        #%
        #% conditionalPanel(
        #%   condition = "input['moduleFileupload-type_locus_sample'] == 1",
        #%   textInput(ns("locusname"),
        #%             label = NULL,
        #%             placeholder = "Locus name")
        #% ),
        #%
        #% conditionalPanel(
        #%   condition = "input['moduleFileupload-type_locus_sample'] == 2",
        #%   textInput(ns("samplename"),
        #%             label = NULL,
        #%             placeholder = "Sample-ID")
        #% ),
        #% conditionalPanel(
        #%   condition = "input['moduleFileupload-type_locus_sample'] != null",
        #%   verbatimTextOutput(ns("samplelocus_out"))
        #% ), width = 6)
        title = "File upload",
        h5("Please type in the ID of the interrogated locus"),
        textInput(ns("locusname"),
          label = NULL,
          placeholder = "Locus ID"
        ),
        conditionalPanel(
          condition = "output['moduleFileupload-type_locus_sample']",
          verbatimTextOutput(ns("samplelocus_out"))
        ),
        width = 6
      ),
      box(
        title = "Description",
        h5(paste0("This application is a graphical user interface (GUI) ",
                  "to the algorithms implemented in the R-package ",
                  "'rBiasCorrection'.")),
        h5(paste0("If you use these 'BiasCorrector' or 'rBiasCorrection' ",
                  "packages to correct DNA methylation data for a ",
                  "publication, please refer to the 'Info'-tab to find out ",
                  "how to cite them.")),
        tags$hr(),
        h5(paste0("You can test this application with example data by ",
                  "pressing the 'Load Example Data'-button below.")),
        div(
          class = "row", style = "text-align: center",
          actionButton(
            ns("load_example_data"),
            "Load Example Data",
            style = paste0(
              "white-space: normal; ",
              "text-align:center; ",
              "padding: 9.5px 9.5px 9.5px 9.5px; ",
              "margin: 6px 10px 6px 10px;")
          )
        )
      )
    ),

    # experimental fileupload box
    fluidRow(
      conditionalPanel(
        condition = "output['moduleFileupload-type_locus_sample']",

        box(
          title = "Data Input: Experimental Data",
          h5(paste0("Please upload the CSV file* containing ",
                    "the experimental data.")),

          # Input: Select a file
          fileInput(
            ns("experimentalFile"),
            paste0("Please choose one CSV file containing the experimental ",
                   "data that are to be corrected."),
            multiple = FALSE,
            accept = c(".csv", "text/csv")
          ),
          h6(paste("Max. file size: ",
                   arguments$maxfilesize,
                   " MB")),

          h6(paste0("*For the specific CSV file requirements ",
                    "please refer to our"),
             a("FAQ!",
               href = paste0("https://github.com/kapsner/",
                             "rBiasCorrection/blob/master/FAQ.md"))),
          width = 6
        )
      ),

      # calibration fileupload box
      conditionalPanel(
        condition = "output['moduleFileupload-file_uploaded']",

        box(
          title = "Data Input: Calibration Data",
          h5(paste0("Please upload the CSV files* containing ",
                    "the calibration data.")),

          uiOutput(ns("fileinput_cal")),

          #% fileInput("calibrationFile",
          #%           paste0("Calibration data: choose one CSV file ",
          #%                  "containing the calibration data"),
          #%           multiple = rv$import_type2,
          #%           accept = c(".csv")),

          h6(paste("Max. file size: ",
                   arguments$maxfilesize,
                   " MB")),
          h6(paste0("*For the specific CSV file requirements ",
                    "please refere to our"),
             a("FAQ!",
               href = paste0("https://github.com/kapsner/",
                             "rBiasCorrection/blob/master/FAQ.md"))),
          width = 6
        )
      )
    )
  )
}
