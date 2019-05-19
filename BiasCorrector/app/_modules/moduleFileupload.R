moduleFileuploadServer <- function(input, output, session, rv, input_re){
  
  # observe Radiobuttonevents 
  observeEvent(input_re()[["moduleFileupload-type_locus_sample"]], {
    rv$type_locus_sample <- input_re()[["moduleFileupload-type_locus_sample"]]
  })
  
  
  # Experimental file
  observe({
    req(input_re()[["moduleFileupload-experimentalFile"]])
    
    if (isFALSE(rv$expFileReq)){
      writeLog("(app) Entered observation for experimental file.")
      # check file ending
      ending <- strsplit(input_re()[["moduleFileupload-experimentalFile"]]$name, ".", fixed = T)[[1]]
      
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
          rv$sampleLocusName = handleTextInput(input_re()[["moduleFileupload-locusname"]])
          writeLog(paste0("Locus name: ", input_re()[["moduleFileupload-locusname"]], "\n(--> stored as: ", rv$sampleLocusName, ")"))
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
          rv$sampleLocusName = handleTextInput(input_re()[["moduleFileupload-samplename"]])
          writeLog(paste0("Sample name: ", input_re()[["moduleFileupload-samplename"]], "\n(--> stored as: ", rv$sampleLocusName, ")"))
          #shinyjs::disable("moduleFileupload-samplename")
          #removeUI(selector = "#samplename", immediate = T)
        }
      }
    }
    
    if (rv$expFileReq == T && is.null(rv$fileimportExp)){
      #removeUI(selector = "#tag1", immediate = T)
      #shinyjs::disable("moduleFileupload-type_locus_sample")
      
      if (ending[2] %in% c("csv", "CSV")){
        file <- reactiveFileReader(1000, session,
                                   input_re()[["moduleFileupload-experimentalFile"]]$datapath, 
                                   fread, header = T)
        tryCatch({
          rv$fileimportExp <- cleanDT(file(), description = "experimental", type = rv$type_locus_sample, rv=rv)
          
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
        }
        
        # check here, if there have been deleted rows containing missing values
        tryCatch({
          omitnasModal(rv$omitnas, "experimental")
          rv$omitnas <- NULL
        }, error = function(e){
          writeLog(paste0("Errormessage: ", e))
        })
        
        # workaround to tell ui, that experimental file is there
        output$fileUploaded <- reactive({
          return(TRUE)
        })
        outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
        
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
    
    if (is.null(rv$fileimportCal) | is.null(rv$fileimportList)){
      # if calibration file is of data type 1
      if (rv$type_locus_sample == "1"){
        
        # check file ending
        ending <- strsplit(input_re()[["calibrationFile"]]$name, ".", fixed = T)[[1]]
        
        # if ending suggests it might be a csv file
        if (ending[2] %in% c("csv", "CSV")){
          file <- reactiveFileReader(1000, session,
                                     input_re()[["calibrationFile"]]$datapath, 
                                     fread, header = T)
          
          # try to import file
          tryCatch({
            if (is.null(rv$fileimportCal)){
              rv$fileimportCal <- cleanDT(file(), "calibration", type = rv$type_locus_sample, rv=rv)
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
            if (rv$fileimportCal[,min(as.numeric(as.character(true_methylation)))] < 0 || rv$fileimportCal[,max(as.numeric(as.character(true_methylation)))] > 100){
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
        
        # loop through calibration files
        for (i in 1:nrow(input_re()[["calibrationFile"]])){
          # check file ending
          ending <- strsplit(input_re()[["calibrationFile"]]$name[i], ".", fixed = T)[[1]]
          
          file <- reactiveFileReader(1000, session,
                                     input_re()[["calibrationFile"]]$datapath[i], 
                                     fread, header = T)
          
          if (ending[2] %in% c("csv", "CSV")){
            rv$fileimportList[[input_re()[["calibrationFile"]]$name[i]]] <- cleanDT(file(), "calibration", type = rv$type_locus_sample, rv=rv)
          } else {
            # error handling fileimport
            openModal("csv", rv)
          }
        }
        
        # chech type 2 file requirements here
        filecheck <- type2FileReq(rv$fileimportList, rv)
        
        if (is.character(filecheck)){
          openModal(filecheck, rv)
        } else if (isTRUE(filecheck)){
          rv$type2cal_uploaded <- TRUE
        }
      }
    }
  })
}

moduleFileuploadUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # type of data box
      box(
        title = "Type of Data",
        
        # Radiobuttons: Type of data
        radioButtons(inputId = ns("type_locus_sample"), label = h5("Please specify the type of DNA methylation data to be corrected for measurement biases"),
                     choices = list("One locus in many samples (e.g. pyrosequencing data)" = 1,
                                    "Many loci in one sample (e.g. next-generation sequencing data or microarray data)" = 2),
                     selected = character(0)),
        
        tags$hr(),
        
        conditionalPanel(
          condition = "input['moduleFileupload-type_locus_sample'] == 1",
          textInput(ns("locusname"),
                    label = NULL,
                    placeholder = "Locus name")
        ),
        
        conditionalPanel(
          condition = "input['moduleFileupload-type_locus_sample'] == 2",
          textInput(ns("samplename"),
                    label = NULL,
                    placeholder = "Sample-ID")
        ),
        conditionalPanel(
          condition = "input['moduleFileupload-type_locus_sample'] != null",
          verbatimTextOutput(ns("samplelocus_out"))
        ), width = 6)
    ),
    
    # experimental fileupload box
    fluidRow(
      conditionalPanel(
        condition = "input['moduleFileupload-type_locus_sample'] != null",
        
        box(
          title = "Data Input: Experimental Data",
          h5("Please upload the CSV files* containing the experimental data."),
          
          # Input: Select a file
          fileInput(ns("experimentalFile"), "Please choose one CSV file containing the experimental data that is to be corrected.",
                    multiple = FALSE,
                    accept = c(".csv", "text/csv")),
          h6(paste("Max. filesize: ", maxfilesize, " MB")),
          
          h6("*For the specific CSV file requirements please refere to our", a("FAQ!", href="https://github.com/kapsner/BiasCorrector/blob/master/FAQ.md")),
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
          h6("*For the specific CSV file requirements please refere to our", a("FAQ!", href="https://github.com/kapsner/BiasCorrector/blob/master/FAQ.md")),
          width = 6)
      )
    )
  )
}