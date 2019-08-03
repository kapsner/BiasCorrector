# BiasCorrector: Correct PCR-bias in DNA methylation analyses
# Copyright (C) 201A GUI to Correct PCR Bias in DNA Methylation Analyses9 Lorenz Kapsner
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


#' @title moduleFileuploadServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleFileuploadServer
moduleFileuploadServer <- function(input, output, session, rv, input_re){

  # TODO original selection of data type
  # # observe Radiobuttonevents
  # observeEvent(input_re()[["moduleFileupload-type_locus_sample"]], {
  #   rv$type_locus_sample <- input_re()[["moduleFileupload-type_locus_sample"]]
  # })
  observe({
    req(rv$type_locus_sample)
    output$type_locus_sample <- reactive({
      return(TRUE)
    })
    outputOptions(output, 'type_locus_sample', suspendWhenHidden=FALSE)
  })

  # loading example data
  observeEvent(input_re()[["moduleFileupload-load_example_data"]], {
    # mimic normal fileimport
    # experimental file
    rv$expFileReq = T
    rv$sampleLocusName = "Example_Locus"
    PCRBiasCorrection::writeLog_(paste0("Locus name: Example_Locus\n(--> stored as: ", rv$sampleLocusName, ")"), logfilename = logfilename)
    rv$fileimportExp <- PCRBiasCorrection::example.data_experimental[["dat"]]

    # calibration file
    rv$fileimportCal <- PCRBiasCorrection::example.data_calibration[["dat"]]
    rv$vec_cal <- PCRBiasCorrection::example.data_calibration[["vec_cal"]]

    # set upload flag
    rv$type1cal_uploaded <- TRUE
  })


  # Experimental file
  observe({
    req(input_re()[["moduleFileupload-experimentalFile"]])

    if (isFALSE(rv$expFileReq)){
      PCRBiasCorrection::writeLog_("(app) Entered observation for experimental file.", logfilename = logfilename)
      # check file ending
      rv$ending <- strsplit(input_re()[["moduleFileupload-experimentalFile"]]$name, ".", fixed = T)[[1]]

      # if type 1 data
      if (rv$type_locus_sample == "1"){
        # render fileInput with option "multiple = F"
        output$fileInputCal <- renderUI({
          fileInput("calibrationFile", "Please choose one CSV file containing the calibration DNA samples.",
                    multiple = FALSE,
                    accept = c(".csv", "text/csv"))
        })

        # check userinput of locusname
        if (input_re()[["moduleFileupload-locusname"]] == ""){
          openModal("locusname", rv)
        } else {
          rv$expFileReq = T
          rv$sampleLocusName = PCRBiasCorrection::handleTextInput_(input_re()[["moduleFileupload-locusname"]])
          PCRBiasCorrection::writeLog_(paste0("Locus name: ", input_re()[["moduleFileupload-locusname"]], "\n(--> stored as: ", rv$sampleLocusName, ")"), logfilename = logfilename)
          #shinyjs::disable("moduleFileupload-locusname")
          #removeUI(selector = "#locusname", immediate = T)
        }

        # if type 2 data
      } else if (rv$type_locus_sample == "2"){
        # render fileInput with option "multiple = TRUE"
        output$fileInputCal <- renderUI({
          fileInput("calibrationFile", "Please choose at least 4 different CSV files containing the calibration data (one file per distinct calibration DNA sample; for specific file naming please refer to our FAQ).",
                    multiple = TRUE,
                    accept = c(".csv", "text/csv"))
        })

        # check userinput of samplename
        if (input_re()[["moduleFileupload-samplename"]] == ""){
          openModal("samplename", rv)
        } else {
          rv$expFileReq = T
          rv$sampleLocusName = PCRBiasCorrection::handleTextInput_(input_re()[["moduleFileupload-samplename"]])
          PCRBiasCorrection::writeLog_(paste0("Sample name: ", input_re()[["moduleFileupload-samplename"]], "\n(--> stored as: ", rv$sampleLocusName, ")"), logfilename = logfilename)
          #shinyjs::disable("moduleFileupload-samplename")
          #removeUI(selector = "#samplename", immediate = T)
        }
      }
    }

    if (rv$expFileReq == T && is.null(rv$fileimportExp)){
      #removeUI(selector = "#tag1", immediate = T)
      #shinyjs::disable("moduleFileupload-type_locus_sample")

      if (rv$ending[2] %in% c("csv", "CSV")){
        file <- reactiveFileReader(1000, session,
                                   input_re()[["moduleFileupload-experimentalFile"]]$datapath,
                                   data.table::fread, header = T)
        tryCatch({
          rv$fileimportExp <- PCRBiasCorrection::cleanDT_(file(), description = "experimental", type = rv$type_locus_sample, logfilename = logfilename)[["dat"]]

          #updateTabItems(session, "tabs", "panel_1")
        }, error = function(e){
          print(e)
          # error handling fileimport
          openModal("experimentalFile", rv)
        })

        # test, if we imported valid file
        if (is.null(rv$fileimportExp)){
          # error handling fileimport
          openModal("experimentalFile", rv)
        } else {
          # check here, if there have been deleted rows containing missing values
          tryCatch({
            omitnasModal(rv$omitnas, "experimental")
            rv$omitnas <- NULL
          }, error = function(e){
            PCRBiasCorrection::writeLog_(paste0("Errormessage: ", e), logfilename = logfilename)
          })

          # workaround to tell ui, that experimental file is there
          output$fileUploaded <- reactive({
            return(TRUE)
          })
          outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
        }
      } else {
        # error handling fileimport
        openModal("experimentalFile", rv)
      }
    }
  })


  # calibration file
  ###### Calibration data
  observe({
    req(input_re()[["calibrationFile"]])

    rv$ending <- NULL

    if (is.null(rv$fileimportCal) | is.null(rv$fileimportList)){
      # if calibration file is of data type 1
      if (rv$type_locus_sample == "1"){

        # check file ending
        rv$ending <- strsplit(input_re()[["calibrationFile"]]$name, ".", fixed = T)[[1]]

        # if ending suggests it might be a csv file
        if (rv$ending[2] %in% c("csv", "CSV")){
          file <- reactiveFileReader(1000, session,
                                     input_re()[["calibrationFile"]]$datapath,
                                     data.table::fread, header = T)

          # try to import file
          tryCatch({
            if (is.null(rv$fileimportCal)){
              cal_type_1 <- PCRBiasCorrection::cleanDT_(file(), "calibration", type = "1", logfilename = logfilename)
              rv$fileimportCal <- cal_type_1[["dat"]]
              rv$vec_cal <- cal_type_1[["vec_cal"]]
            }
          }, error = function(e){
            print(e)
            # error handling fileimport
            openModal("calibrationFile", rv)
          })

          # go on, if we imported valid file
          if (!is.null(rv$fileimportCal)){

            # try to check, if colnames of experimental data are same as those of calibration data
            tryCatch({
              # check, if colnames of experimental and calibration data are equal:
              if(!all.equal(colnames(rv$fileimportCal)[-1], colnames(rv$fileimportExp)[-1])){
                # error handling fileimport
                openModal("calibrationFile", rv)
              }
            }, error = function(e){
              print(e)
              # error handling fileimport
              openModal("calibrationFile", rv)
            })

            # check here, if there are calibration steps outside the range 0 <= CS <= 100
            if (rv$fileimportCal[,min(as.numeric(as.character(get("true_methylation"))))] < 0 || rv$fileimportCal[,max(as.numeric(as.character(get("true_methylation"))))] > 100){
              openModal("calibrange", rv)
            } else {

              # # check here, if there have been deleted rows containing missin values
              # tryCatch({
              #   omitnasModal(rv$omitnas, "calibration")
              #   rv$omitnas <- NULL
              # }, error = function(e){
              #   print(e)
              # })

              rv$type1cal_uploaded <- TRUE
            }

            # if we have the value "NULL" in our file-variable; this happens, when cleanDT returns error
          } else {
            # error handling fileimport
            openModal("calibrationFile", rv)
          }

          # else, if ending is no csv-file
        } else {
          # error handling fileimport
          openModal("calibrationFile", rv)
        }


        # if calibration file is of data type 2
      } else if (rv$type_locus_sample == "2"){

        if (isFALSE(rv$type2cal_uploaded)){

          # loop through calibration files
          for (i in 1:nrow(input_re()[["calibrationFile"]])){
            # check file ending
            rv$ending <- strsplit(input_re()[["calibrationFile"]]$name[i], ".", fixed = T)[[1]]

            file <- reactiveFileReader(1000, session,
                                       input_re()[["calibrationFile"]]$datapath[i],
                                       data.table::fread, header = T)

            if (rv$ending[2] %in% c("csv", "CSV")){
              rv$fileimportList[[input_re()[["calibrationFile"]]$name[i]]] <- PCRBiasCorrection::cleanDT_(file(), "calibration", type = "2", logfilename = logfilename)[["dat"]]
            } else {
              # error handling fileimport
              openModal("csv", rv)
            }
          }

          # chech type 2 file requirements here
          filecheck <- PCRBiasCorrection::type2FileReq_(rv$fileimportList, rv, logfilename = logfilename)

          if (is.character(filecheck)){
            openModal(filecheck, rv)
          } else if (isTRUE(filecheck)){
            rv$type2cal_uploaded <- TRUE
          }
        }
      }
    }
  })
}


#' @title moduleFileuploadUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleFileuploadUI
moduleFileuploadUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      # type of data box
      box(
        # TODO original selection of data type
        # title = "Type of Data",
        # # Radiobuttons: Type of data
        # radioButtons(inputId = ns("type_locus_sample"), label = h5("Please specify the type of DNA methylation data to be corrected for measurement biases"),
        #              choices = list("One locus in many samples (e.g. pyrosequencing data)" = 1,
        #                             "Many loci in one sample (e.g. next-generation sequencing data or microarray data)" = 2),
        #              selected = character(0)),
        #
        # tags$hr(),
        #
        # conditionalPanel(
        #   condition = "input['moduleFileupload-type_locus_sample'] == 1",
        #   textInput(ns("locusname"),
        #             label = NULL,
        #             placeholder = "Locus name")
        # ),

        # conditionalPanel(
        #   condition = "input['moduleFileupload-type_locus_sample'] == 2",
        #   textInput(ns("samplename"),
        #             label = NULL,
        #             placeholder = "Sample-ID")
        # ),
        # conditionalPanel(
        #   condition = "input['moduleFileupload-type_locus_sample'] != null",
        #   verbatimTextOutput(ns("samplelocus_out"))
        # ), width = 6)

        title = "File upload",
        h5("Please insert the name of the locus under investigation"),
        textInput(ns("locusname"),
                  label = NULL,
                  placeholder = "Locus name"),
        conditionalPanel(
          condition = "output['moduleFileupload-type_locus_sample']",
          verbatimTextOutput(ns("samplelocus_out"))
        ),
        width = 6),
      box(
        title = "Description",
        h5("This application is a graphical user interface (GUI) to the alogrihms implemented in the R-package 'PCRBiasCorrection'."),
        h5("If you use this 'BiasCorrector' or 'PCRBiasCorrection' to correct PCR measurement biases for a publication, please refere to the 'Info'-tab to find out how to cite them."),
        tags$hr(),
        h5("You can test this application with example data by pressing the 'Load Example Data'-button below."),
        div(class="row", style="text-align: center",
            actionButton(ns("load_example_data"), "Load Example Data",
                         style="white-space: normal; text-align:center;
        padding: 9.5px 9.5px 9.5px 9.5px;
        margin: 6px 10px 6px 10px;"))
      )
    ),

    # experimental fileupload box
    fluidRow(
      conditionalPanel(
        condition = "output['moduleFileupload-type_locus_sample']",

        box(
          title = "Data Input: Experimental Data",
          h5("Please upload the CSV files* containing the experimental data."),

          # Input: Select a file
          fileInput(ns("experimentalFile"), "Please choose one CSV file containing the experimental data that is to be corrected.",
                    multiple = FALSE,
                    accept = c(".csv", "text/csv")),
          h6(paste("Max. filesize: ", maxfilesize, " MB")),

          h6("*For the specific CSV file requirements please refere to our", a("FAQ!", href="https://github.com/kapsner/PCRBiasCorrection/blob/master/FAQ.md")),
          width = 6)
      ),

      # calibration fileupload box
      conditionalPanel(
        condition =  "output['moduleFileupload-fileUploaded']",

        box(
          title = "Data Input: Calibration Data",
          h5("Please upload the CSV files* containing the calibration data."),

          uiOutput(ns("fileInputCal")),

          # fileInput("calibrationFile", "Calibration data: choose one CSV file containing the calibration data",
          #             multiple = rv$import_type2,
          #             accept = c(".csv")),

          h6(paste("Max. filesize: ", maxfilesize, " MB")),
          h6("*For the specific CSV file requirements please refere to our", a("FAQ!", href="https://github.com/kapsner/PCRBiasCorrection/blob/master/FAQ.md")),
          width = 6)
      )
    )
  )
}
