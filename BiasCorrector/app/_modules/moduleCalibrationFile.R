moduleCalibrationFileServer <- function(input, output, session, rv, input_re){
  # error handling with fileimport
  observeEvent({
    if (isTRUE(rv$type2cal_uploaded) | isTRUE(rv$type1cal_uploaded)) TRUE
    else return()}, {
      writeLog("(app) Entered observeEvent after fileimport of calibration file")
      
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
            formatRound(columns=c(2:ncol(rv$fileimportCal)), digits=3)
        })
        
        output$cal_samples <- reactive({
          len <- unique(rv$fileimportCal[,true_methylation])
          message <- paste0("Unique calibration samples: ", length(len))
          writeLog(message)
          message
        })
        
        output$cal_samples_raw <- reactive({
          len <- unique(rv$fileimportCal[,true_methylation])
          message <- paste0("Unique calibration steps:\n", paste(len, collapse = "\n"))
          writeLog(message)
          message
        })
        
        # if type 2 data
      } else if (rv$type_locus_sample == "2"){
        
        # render assignment of calibration steps
        output$calibration_data <- renderUI({
          select_output_list <- lapply(1:nrow(rv$calibr_steps), function(g) {
            selectname <- paste0("select", g)
            div(class="row",
                div(class="col-sm-6", style="text-align: left",
                    h5(tags$b(paste0(rv$calibr_steps[g, name], ":")))),
                div(class="col-sm-6", style="text-align: center",
                    numericInput(inputId = selectname,
                                 min = 0,
                                 max = 100,
                                 label = NULL,
                                 step = 0.01,
                                 value = rv$calibr_steps[g, step],
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
          writeLog(message)
          message
        })
        
        output$cal_samples_raw <- reactive({
          message <- paste0("Unique calibration steps:\n", paste(levels(factor(rv$calibr_steps[,step])), collapse = "\n"))
          writeLog(message)
          message
        })
      }
    })
  
  # confirm-Button for Type2-Data
  observeEvent(input_re()$confirm_steps, {
    rv$choices_list <- data.table("name" = character(), "step" = numeric())
    lapply(1:nrow(rv$calibr_steps), function(g) {
      selectname <- paste0("select", g)
      rv$choices_list <- rbind(rv$choices_list, cbind("name" = rv$calibr_steps[g,name], "step" = as.numeric(eval(parse(text=paste0("input_re()$", selectname))))))
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
        selectInput(inputId="selectType2", label = NULL, multiple = F, selectize = F, choices = names(rv$fileimportCal))
      })
      
      # create reactive df-selection:
      df <- reactive({
        temp <- rv$fileimportCal[[input_re()$selectType2]]
      })
      
      # render the UI output
      output$calibration_data2 <- renderUI({
        s <- selIn()
        
        output$dt2 <- DT::renderDataTable({
          temp <- df()
          DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
            formatRound(columns=c(2:ncol(temp)), digits=3)
        })
        
        # merge selectInput and dataframe to list
        output_list <- list(s, DT::dataTableOutput("moduleCalibrationFile-dt2"))
        
        # print out list!
        do.call(tagList, output_list)
      })
    }
  })
}

moduleCalibrationFileUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Calibration Data",
        div(class="row", style="margin: 0.5%"),
        verbatimTextOutput(ns("cal_samples")),
        verbatimTextOutput(ns("cal_samples_raw")),
        uiOutput(ns("calibration_data")),
        uiOutput(ns("calibration_data2")),
        tags$hr(),
        width = 12
      ))
  )
}