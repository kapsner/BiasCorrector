server <- function(input, output, session) {
  
  # source functions
  source("Functions.R", echo = F, encoding = "UTF-8")
  source("App_Utilities.R", echo = F, encoding = "UTF-8")
  
  rv <- reactiveValues(
    expFileReq = F,
    fileimportExp = NULL,
    fileimportCal = NULL,
    fileimportList = NULL,
    regStats = NULL,
    modal_closed = T,
    modal_type = NULL,
    finalResults = NULL,
    sampleLocusName = NULL,
    type2cal_uploaded = FALSE,
    plotting_finished = FALSE,
    substitutions = NULL,
    substitutionsCalc = NULL,
    type2CalcRes = NULL,
    choices_list = NULL,
    result_list_type2 = NULL,
    result_list = NULL,
    temp_results = NULL,
    vec_cal = NULL,
    plotdir = NULL,
    csvdir = NULL,
    tempdir = NULL,
    y0 = NULL,
    y1 = NULL,
    b = NULL,
    omitnas = NULL,
    m0 = NULL,
    m1 = NULL,
    j = NULL,
    calibr_steps = NULL,
    logfile = NULL
  )
  
  onStart()
  
  # scientific purpose
  showModal(modalDialog(
    title = "This program is to be used for scientific research purposes only",
    "I hereby confirm to use this program only for scientific research purposes.",
    footer = tagList(actionButton("dismiss_modal",label = "Cancel"),
                     modalButton("Confirm"))
  ))
  
  observeEvent(input$dismiss_modal, {
    writeLog("dismiss modal")
    rv$modal_closed <- T
    rv$modal_type <- NULL
    removeModal()
    js$reset()
  })
  
  observeEvent(input$reset, {
    writeLog("restarting app")
    cleanUp()
    js$reset()
  })
  
  output$samplelocus_out <- reactive({
    paste(rv$sampleLocusName)
  })
  
  openModal <- function(description){
    rv$modal_closed <- F
    rv$modal_type <- description
    requirementsError(description)
  }
  
  # logfileviewer
  observe({
    file <- reactiveFileReader(500, session,
                               logfilename, 
                               readLines)
    rv$logfile <- file()
  })
  output$log_out <- reactive({
    paste(paste0(rv$logfile, collapse = "\n"))
  })
  
  
  
  ###### Experimental data
  observe({
    req(input$experimentalFile)
    writeLog("(app) Entered observation for experimental file.")
    # check file ending
    ending <- strsplit(input$experimentalFile$name, ".", fixed = T)[[1]]
    
    # if type 1 data
    if (input$type_locus_sample == "1"){
      # render fileInput with option "multiple = F"
      output$fileInputCal <- renderUI({
        fileInput("calibrationFile", "Please choose one CSV file containing the calibration DNA samples.",
                  multiple = FALSE,
                  accept = c(".csv", "text/csv"))
      })
      
      # check userinput of locusname
      if (input$locusname == ""){
        openModal("locusname")
      } else {
        rv$expFileReq = T
        rv$sampleLocusName = handleTextInput(input$locusname)
        writeLog(paste0("Locus name: ", input$locusname, "\n(--> stored as: ", rv$sampleLocusName, ")"))
        removeUI(selector = "#locusname", immediate = T)
      }
      
      # if type 2 data
    } else if (input$type_locus_sample == "2"){
      # render fileInput with option "multiple = TRUE"
      output$fileInputCal <- renderUI({
        fileInput("calibrationFile", "Please choose at least 4 different CSV files containing the calibration data (one file per distinct calibration DNA sample; for specific file naming please refer to our FAQ).",
                  multiple = TRUE,
                  accept = c(".csv", "text/csv"))
      })
      
      # check userinput of samplename
      if (input$samplename == ""){
        openModal("samplename")
      } else {
        rv$expFileReq = T
        rv$sampleLocusName = handleTextInput(input$samplename)
        writeLog(paste0("Sample name: ", input$samplename, "\n(--> stored as: ", rv$sampleLocusName, ")"))
        removeUI(selector = "#samplename", immediate = T)
      }
    }
    
    if (rv$expFileReq == T){
      removeUI(selector = "#tag1", immediate = T)
      shinyjs::disable("type_locus_sample")
      
      if (ending[2] %in% c("csv", "CSV")){
        file <- reactiveFileReader(1000, session,
                                   input$experimentalFile$datapath, 
                                   fread)
        tryCatch({
          rv$fileimportExp <- cleanDT(file(), description = "experimental", type = input$type_locus_sample, rv=rv)
          output$menu <- renderMenu({
            sidebarMenu(
              menuItem("Experimental Data", tabName = "panel_1", icon = icon("table"))
            )
          })
          updateTabItems(session, "tabs", "panel_1")
        }, error = function(e){
          print(e)
          # error handling fileimport
          openModal("experimentalFile")
        })
        
        # test, if we imported valid file
        if (is.null(rv$fileimportExp)){
          # error handling fileimport
          openModal("experimentalFile")
        }
        
        # check here, if there have been deleted rows containing missin values
        tryCatch({
          omitnasModal(rv$omitnas, "experimental")
        }, error = function(e){
          writeLog(paste0("Errormessage: ", e))
        })
        
      } else {
        # error handling fileimport
        openModal("experimentalFile")
      }
    }
  })
  
  # error handling with fileimport
  observeEvent({
    if (!is.null(rv$fileimportExp)) TRUE
    else return()}, {
      writeLog("(app) Entered observeEvent after fileimport of experimental file")
      
      # disable upload possibility of experimental file
      shinyjs::disable("experimentalFile")
      
      # if type 1 data
      if (input$type_locus_sample == "1"){
        
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimportExp, options = list(scrollX = TRUE, pageLength = 20)) %>%
            formatRound(columns=c(2:ncol(rv$fileimportExp)), digits=3)
        })
        
        output$exp_samples <- reactive({
          len <- unique(rv$fileimportExp[,sample_id])
          message <- paste0("Unique samples: ", length(len))
          writeLog(message)
          message
        })
        
        output$exp_samples_raw <- reactive({
          len <- sort(unique(rv$fileimportExp[,sample_id]))
          message <- paste0("Unique sample IDs:\n", paste(len, collapse = ", "))
          writeLog(message)
          message
        })
        
        # if type 2 data
      } else if (input$type_locus_sample == "2"){
        
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimportExp, options = list(scrollX = TRUE, pageLength = 20)) %>%
            formatRound(columns=c(2:ncol(rv$fileimportExp)), digits=3)
        })
        
        output$exp_samples <- reactive({
          len <- unique(rv$fileimportExp[,locus_id])
          message <- paste0("Unique loci: ", length(len))
          writeLog(message)
          message
        })
        
        output$exp_samples_raw <- reactive({
          len <- sort(unique(rv$fileimportExp[,locus_id]))
          message <- paste0("Unique locus IDs:\n", paste(len, collapse = ", "))
          writeLog(message)
          message
        })
        
      }
    })
  
  
  
  observeEvent(input$calibrationFile, {
    # error handling, when uploading new data in same session
    output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
        menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
        actionButton("run", "Run Analysis", width = "80%")
      )
    })
    updateTabItems(session, "tabs", "panel_2")
  })
  
  
  ###### Calibration data
  observe({
    req(input$calibrationFile)
    
    # if calibration file is of data type 1
    if (input$type_locus_sample == "1"){
      
      # check file ending
      ending <- strsplit(input$calibrationFile$name, ".", fixed = T)[[1]]
      
      # if ending suggests it might be a csv file
      if (ending[2] %in% c("csv", "CSV")){
        file <- reactiveFileReader(1000, session,
                                   input$calibrationFile$datapath, 
                                   fread)
        
        # try to import file
        tryCatch({
          rv$fileimportCal <- cleanDT(file(), "calibration", type = input$type_locus_sample, rv=rv)
        }, error = function(e){
          print(e)
          # error handling fileimport
          openModal("calibrationFile")
        })
        
        # go on, if we imported valid file
        if (!is.null(rv$fileimportCal)){
          
          # try to check, if colnames of experimental data are same as those of calibration data
          tryCatch({
            # check, if colnames of experimental and calibration data are equal:
            if(!all.equal(colnames(rv$fileimportCal)[-1], colnames(rv$fileimportExp)[-1])){
              # error handling fileimport
              openModal("calibrationFile")
            }
          }, error = function(e){
            print(e)
            # error handling fileimport
            openModal("calibrationFile")
          })
          
          # if we have the value "NULL" in our file-variable; this happens, when cleanDT returns error
        } else {
          # error handling fileimport
          openModal("calibrationFile")
        }
        
        # check here, if there have been deleted rows containing missin values
        tryCatch({
          omitnasModal(rv$omitnas, "calibration")
        }, error = function(e){
          print(e)
        })
        
        # else, if ending is no csv-file
      } else {
        # error handling fileimport
        openModal("calibrationFile")
      }
      
      
      # if calibration file is of data type 2
    } else if (input$type_locus_sample == "2"){
      
      # loop through calibration files
      for (i in 1:nrow(input$calibrationFile)){
        # check file ending
        ending <- strsplit(input$calibrationFile$name[i], ".", fixed = T)[[1]]
        
        file <- reactiveFileReader(1000, session,
                                   input$calibrationFile$datapath[i], 
                                   fread)
        
        if (ending[2] %in% c("csv", "CSV")){
          rv$fileimportList[[input$calibrationFile$name[i]]] <- cleanDT(file(), "calibration", type = input$type_locus_sample, rv=rv)
          
        } else {
          # error handling fileimport
          openModal("csv")
        }
      }
      
      # chech type 2 file requirements here
      filecheck <- type2FileReq(rv$fileimportList, rv)
      
      if (is.character(filecheck)){
        openModal(filecheck)
      } else if (isTRUE(filecheck)){
        rv$type2cal_uploaded <- TRUE
      }
    }
  })
  
  
  # error handling with fileimport
  observeEvent({
    if (isTRUE(rv$type2cal_uploaded) | !is.null(rv$fileimportCal)) TRUE
    else return()}, {
      writeLog("(app) Entered observeEvent after fileimport of calibration file")
      
      # disable upload possibility of calibration file
      shinyjs::disable("calibrationFile")
      
      # if type 1 data
      if (input$type_locus_sample == "1"){
        
        output$dt1 <- DT::renderDataTable({
          DT::datatable(rv$fileimportCal, options = list(scrollX = TRUE, pageLength = 20)) %>%
            formatRound(columns=c(2:ncol(rv$fileimportCal)), digits=3)
        })
        
        output$calibration_data <- renderUI({
          DT::dataTableOutput("dt1")
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
      } else if (input$type_locus_sample == "2"){
        
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
  observeEvent(input$confirm_steps, {
    rv$choices_list <- data.table("name" = character(), "step" = numeric())
    lapply(1:nrow(rv$calibr_steps), function(g) {
      selectname <- paste0("select", g)
      rv$choices_list <- rbind(rv$choices_list, cbind("name" = rv$calibr_steps[g,name], "step" = as.numeric(eval(parse(text=paste0("input$", selectname))))))
    })
    print(rv$choices_list)
    
    # assign rv$fileimportCal
    filecheck <- type2FileConfirm(rv$fileimportList, rv$choices_list, rv)
    if (is.character(filecheck)){
      openModal(filecheck)
    } else {
      
      # store correct formatted calibration data in reactive list
      rv$fileimportCal <- filecheck
      removeUI(selector = "#calibration_data", immediate = T)
      
      # create reactive selectinput:
      selIn <- reactive({
        selectInput(inputId="selectType2", label = NULL, multiple = F, selectize = F, choices = names(rv$fileimportCal))
      })
      
      # create reactive df-selection:
      df <- reactive({
        temp <- rv$fileimportCal[[input$selectType2]]
      })
      
      # render the UI output
      output$calibration_data2 <- renderUI({
        s <- selIn()
        
        output$dt2 <- DT::renderDataTable({
          temp <- df()
          DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 20)) %>%
            formatRound(columns=c(2:ncol(temp)), digits=3)
        })
        
        # merge selectInput and dataframe to list
        output_list <- list(s, DT::dataTableOutput("dt2"))
        
        # print out list!
        do.call(tagList, output_list)
      })
    }
  })
  
  ###### Run Analysis
  observeEvent(input$run, {
    if (!is.null(rv$fileimportCal)){
      
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line"))
        )
      })
      updateTabItems(session, "tabs", "panel_3")
      shinyjs::disable("run")
    }
  })
  
  
  observe({
    # this is needed, to open new tab (Regression plots) before rendering the plots!
    if (input$tabs == "panel_3"){
      
      # type 1 data: 
      if (input$type_locus_sample == "1"){
        if (isFALSE(rv$plotting_finished)){
          
          plottingUtility(rv$fileimportCal, type=1, samplelocusname=rv$sampleLocusName, rv=rv)
          
          # on finished
          rv$plotting_finished <- TRUE
          writeLog("Finished plotting")
          
          # save regression statistics to reactive value
          rv$regStats <- statisticsList(rv$result_list)
        }
        
        # else if type 2 data
      } else if (input$type_locus_sample == "2"){
        
        if (isFALSE(rv$plotting_finished)){
          a <- 1
          rv$result_list_type2 <- list()
          
          for (b in names(rv$fileimportCal)){
            rv$vec_cal <- names(rv$fileimportCal[[a]])[-1]
            #print(paste("Length rv$vec_cal:", length(rv$vec_cal)))
            
            plottingUtility(rv$fileimportCal[[a]], type=2, samplelocusname=rv$sampleLocusName, b=gsub("[[:punct:]]", "", b), rv=rv)
            
            # save regression statistics to reactive value
            rv$regStats[[b]] <- statisticsList(rv$result_list)
            rv$result_list_type2[[b]] <- rv$result_list
            a <- a + 1
          }
          # on finished
          rv$plotting_finished <- TRUE
          writeLog("Finished plotting")
        }
      }
    }
  })
  
  # when plotting has finished
  observe({
    req(rv$plotting_finished)
    
    if (input$type_locus_sample == "1"){
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
          menuItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line")),
          menuItem("Select Regression Model", tabName = "panel_5", icon = icon("chart-line"))
        )
      })
    } else {
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
          menuItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line"))
        )
      })
    }
    updateTabItems(session, "tabs", "panel_3")
  })
  
  observe({
    req(rv$plotting_finished)
    # type 1 data: 
    if (input$type_locus_sample == "1"){
      
      ### Plot tab ###
      # create a list of plotnames to populate selectInput
      plot_output_list <- lapply(1:length(rv$vec_cal), function(g) {
        paste0(gsub("[[:punct:]]", "", rv$vec_cal[g]))
      })
      names(plot_output_list) <- rv$vec_cal 
      
      # create reactive selectinput:
      selIn2 <- reactive({
        selectInput(inputId="selectPlot", label = NULL, multiple = F, selectize = F, choices = plot_output_list, width = "50%")
      })
      
      # create download button for each plot
      output$downloadPlots <- downloadHandler(
        filename = function(){paste0(rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input$selectPlot), ".png")},
        content = function(file){
          file.copy(paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input$selectPlot), ".png"), file)
        },
        contentType = "image/png"
      )
      
      # render head of page with selectInput and downloadbutton
      # TODO align selectinput and button aside of each other
      output$selectPlotInput <- renderUI({
        s <- selIn2()
        b <- downloadButton("downloadPlots", "Download Plot")
        do.call(tagList, list(s, b))
      })
      
      # render plots from local temporary file
      output$plots <- renderImage({
        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input$selectPlot), ".png")
        # Return a list containing the filename
        list(src = filename)
      }, deleteFile = FALSE)
      
      
      ### Regression statistics tab ###
      output$regression_statistics <- renderUI({
        output$dt_reg <- DT::renderDataTable({
          dt <- rv$regStats
          # use formatstyle to highlight lower SSE values
          renderRegressionStatisticTable(dt)
        })
        d <- DT::dataTableOutput("dt_reg")
        do.call(tagList, list(d))
      })
      
      # create download button for regression statistics
      output$downloadRegStat <- downloadHandler(
        filename = function(){
          paste0("BC_regression_stats_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_", 
                 gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
        },
        content = function(file){
          writeCSV(rv$regStats[,-12, with=F], file)
        },
        contentType = "text/csv"
      )
      
      
      ### model selection tab ###
      # render radio buttons for tab 5
      output$reg_radios <- renderUI({ 
        radio_output_list <- lapply(1:length(rv$vec_cal), function(g) {
          radioname <- paste0("radio", g)
          div(class="row",
              div(class="row", style = "margin: 0.5%;",
                  div(class="col-sm-3", style="text-align: left",
                      h5(tags$b(paste0("Regression type for ", rv$vec_cal[g], ":")))),
                  div(class="col-sm-3",
                      div(class = "row", style = "margin: 0.5%;",
                          radioButtons(inputId = radioname, 
                                       label = NULL,
                                       choices = list("hyperbolic" = 0, "cubic" = 1),
                                       selected = as.character(rv$regStats[Name==rv$vec_cal[g], better_model]),
                                       inline = TRUE))
                  ),
                  div(class="col-sm-3",
                      verbatimTextOutput(paste0("text_", radioname)))),
              tags$hr())
        })
        do.call(tagList, list(radio_output_list, 
                              div(class="row", style="text-align: center", actionButton("results", "BiasCorrect your experimental data"))
        )) # needed to display properly.
      })
      
      
    } else if (input$type_locus_sample == "2"){
      # type 2 data: 
      
      ### Plot tab ###
      # create list of loci to populate selectInput "selectPlotLocus"
      list_plot_locus <- list()
      for (i in 1:length(rv$fileimportCal)){
        list_plot_locus[[i]] <- names(rv$fileimportCal)[i]
      }
      
      selectPlotLocus <- reactive({
        selectInput(inputId="selectPlotLocus", label = NULL, multiple = F, selectize = F, choices = list_plot_locus, width = "50%")
      })
      
      
      # create list of cpg-sites for each locus to populate selectInput "selectPlotCpG"
      list_plot_cpg <- list()
      for (i in 1:length(rv$fileimportCal)){
        list_plot_cpg[[names(rv$fileimportCal)[i]]] <- names(rv$fileimportCal[[i]])[-1]
      }
      
      # only return list of CpG-sites for each locus, if there is already a selection of the locus in selectPlotLocus
      cpg_output <- reactive({
        if (!is.null(input$selectPlotLocus)){
          return(list_plot_cpg[input$selectPlotLocus])
        }
      })
      
      # always wrap selectInput into reactive-function
      selectPlotCpG <- reactive({
        selectInput(inputId="selectPlotType2", label = NULL, multiple = F, selectize = F, choices = cpg_output(), width = "50%")
      })
      
      # render second selectInput
      output$s2PlotOutput <- renderUI({
        s3 <- selectPlotCpG()
        s3
      })
      
      # create download button for each plot
      output$downloadPlots <- downloadHandler(
        filename = function(){paste0(gsub("[[:punct:]]", "", input$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input$selectPlotType2), ".png")},
        content = function(file){
          file.copy(paste0(plotdir, gsub("[[:punct:]]", "", input$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input$selectPlotType2), ".png"), file)
        },
        contentType = "image/png"
      )
      
      # render Plot UI
      output$selectPlotInput <- renderUI({
        s1 <- selectPlotLocus()
        s2 <- uiOutput("s2PlotOutput")
        b <- downloadButton("downloadPlots", "Download Plot")
        do.call(tagList, list(s1, s2, b))
      })
      
      # render plot from local temporary file
      output$plots <- renderImage({
        filename <- paste0(plotdir, gsub("[[:punct:]]", "", input$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input$selectPlotType2), ".png")
        # print(filename)
        # Return a list containing the filename
        list(src = filename)
      }, deleteFile = FALSE)
      
      
      ### Regression statistics tab ###
      # create reactive selectinput:
      selInLocus <- reactive({
        selectInput(inputId="selectRegStatsLocus", label = NULL, multiple = F, selectize = F, choices = names(rv$fileimportCal))
      })
      
      # create reactive df-selection:
      df_regs <- reactive({
        dt <- rv$regStats[[input$selectRegStatsLocus]]
      })
      
      output$dt_regs <- DT::renderDataTable({
        dt <- df_regs()
        renderRegressionStatisticTable(dt)
      })
      
      # render head of page with selectInput and downloadbutton
      # TODO align selectinput and button aside of each other
      output$regression_statistics <- renderUI({
        s1 <- selInLocus()
        dt <- DT::dataTableOutput("dt_regs")
        db <- div(class="row", style="text-align: center", actionButton("results", "BiasCorrect your experimental data"))
        do.call(tagList, list(s1, dt, db))
      })
      
      # create download button for regression statistics
      output$downloadRegStat <- downloadHandler(
        filename = function(){
          paste0("BC_regression_stats_", gsub("[[:punct:]]", "", input$selectRegStatsLocus), "_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_", 
                 gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
        },
        content = function(file){
          writeCSV(rv$regStats[[input$selectRegStatsLocus]][,-12, with=F], file)
        },
        contentType = "text/csv"
      )
      
      
      ### model selection tab ###
      
      # TODO work here:
      # TODO think about displaying of model selection in type 2 data... all in one page?
      # modelSelectLocus <- reactive({
      #   selectInput(inputId="modelSelectLocus", label = NULL, multiple = F, selectize = F, choices = names(rv$fileimportCal))
      # })
      # 
      # selRad <- reactive({
      #   if (!is.null(input$modelSelectLocus)){
      #     rv$vec_cal <- names(rv$fileimportCal[[input$modelSelectLocus]])[-1]
      #     
      #     return(lapply(1:length(rv$vec_cal), function(g) {
      #       
      #       radioname <- paste0("radio", g)
      #       div(class="row", style="margin: 0.5%",
      #           div(class="row",
      #               div(class="col-sm-4", style="text-align: center",
      #                   h5(tags$b(paste0(rv$vec_cal[g], ":")))),
      #               div(class="col-sm-4",
      #                   radioButtons(inputId = radioname, 
      #                                label = "Regression type:", 
      #                                choices = list("hyperbolic" = 0, "cubic" = 1),
      #                                selected = as.character(rv$regStats[[input$modelSelectLocus]][Name==rv$vec_cal[g], better_model]),
      #                                inline = T)),
      #               div(class="col-sm-3",
      #                   verbatimTextOutput(paste0("text_", radioname)))),
      #           tags$hr())
      #     }))
      #   }
      # })
      # 
      # output$reg_radios <- renderUI({ 
      #   s <- modelSelectLocus()
      #   radio_output_list <- selRad()
      #   do.call(tagList, list(s, radio_output_list)) # needed to display properly.
      # })
      
      # trigger claculation of results (bypass manual model selection)
      #shinyjs::click("results")
    }
  })
  
  observe({
    if (input$tabs == "panel_5"){
      
      if (input$type_locus_sample == "1"){
        
        lapply(1:length(rv$vec_cal), function(h) {
          radioname <- paste0("text_radio", h)
          output[[radioname]] <- reactive({
            paste("SSE:",
                  as.character(ifelse(input[[paste0("radio", h)]] == 1,
                                      rv$regStats[Name==rv$vec_cal[h], SSE_cubic],
                                      rv$regStats[Name==rv$vec_cal[h], SSE_hyperbolic]))
            )
          })
        })
        
        lapply(1:length(rv$vec_cal), function(k) {
          radioname <- paste0("radio", k)
          if (!is.null(input[[radioname]])){
            if (input[[radioname]] == "1") {
              output[[paste0("text_", radioname)]] <- reactive({
                paste("SSE:",
                      as.character(rv$regStats[Name==rv$vec_cal[k], round(SSE_cubic,3)])
                )
              })
            } else if (input[[radioname]] == "0") {
              output[[paste0("text_", radioname)]] <- reactive({
                paste("SSE:",
                      as.character(rv$regStats[Name==rv$vec_cal[k], round(SSE_hyperbolic, 3)])
                )
              })
            }
          }
        })
      }
    }
  })
  
  
  # Calculate results for experimental data
  observeEvent(input$results, {
    
    if (input$type_locus_sample == "1"){
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
          menuItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line")),
          menuItem("Select Regression Model", tabName = "panel_5", icon = icon("chart-line")),
          menuItem("BiasCorrected Results", tabName = "panel_6", icon = icon("angellist"))
        )
      })
    } else {
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
          menuItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line")),
          menuItem("BiasCorrected Results", tabName = "panel_6", icon = icon("angellist"))
        )
      })
    }
    updateTabItems(session, "tabs", "panel_6")
    
    # reset reactive values
    # TODO why was this implemented here?
    rv$calculate_results <- NULL
    rv$finalResults <- NULL
    
    if (input$type_locus_sample == "1"){
      
      rv$choices_list <- data.table("Name" = character(), "better_model" = numeric())
      lapply(1:length(rv$vec_cal), function(x) {
        radioname <- paste0("radio", x)
        rv$choices_list <- rbind(rv$choices_list, cbind("Name" = rv$vec_cal[x], "better_model" = as.numeric(eval(parse(text=paste0("input$", radioname))))))
      })
      print(rv$choices_list)
      
      
      substitutions_create(rv)
      # calculating final results
      withProgress(message = "BiasCorrecting experimental data", value = 0, {
        incProgress(1/1, detail = "... working on BiasCorrection ...")
        rv$finalResults <- solving_equations(rv$fileimportExp, rv$choices_list, type = 1, rv = rv)
      })
      
      
    } else if (input$type_locus_sample == "2"){
      
      # initialize temp results
      rv$temp_results <- list()
      
      substitutions_create(rv)
      # iterate over unique names in locus_id of experimental file (to correctly display
      # decreasing order of CpG-sites in final results)
      
      # calculating final results
      withProgress(message = "BiasCorrecting experimental data", value = 0, {
        incProgress(1/1, detail = "... working on BiasCorrection ...")
        
        for (b in rv$fileimportExp[,unique(locus_id)]){
          rv$result_list <- rv$result_list_type2[[b]]
          expdata <- rv$fileimportExp[locus_id==b]
          vec <- c("locus_id", colnames(expdata)[2:(expdata[,min(CpG_count)]+1)], "rowmeans")
          rv$temp_results[[b]] <- solving_equations(expdata[,vec,with=F], rv$regStats[[b]][,.(Name, better_model)], type = 2, rv = rv)
        }
        
        for (i in names(rv$temp_results)){
          rv$finalResults <- rbind(rv$finalResults, rv$temp_results[[i]], use.names = T, fill = T)
        }
        
        vec <- colnames(rv$finalResults)[grepl("rowmeans", colnames(rv$finalResults))]
        rv$finalResults <- cbind(rv$finalResults[,-vec, with=F], rv$finalResults[,vec,with=F], CpG_sites = unique(rv$fileimportExp[,CpG_count,by=locus_id])$CpG_count)
      })
    }
    
    output$dtfinal <- DT::renderDataTable({
      # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
      DT::datatable(rv$finalResults, options = list(scrollX = TRUE, pageLength = 20)) %>%
        formatRound(columns=c(2:ncol(rv$finalResults)), digits=3)
    })
    
    # show corrected results for experimental data
    output$corrected_data <- renderUI({
      dt <- dataTableOutput("dtfinal")
      db <- div(class="row", style="text-align: center", downloadButton("downloadFinal", "Download corrected values"))
      dball <- div(class="row", style="text-align: center", downloadButton("downloadAllData", "Download zip archive (tables and plots)"))
      do.call(tagList, list(dt, tags$hr(), db, tags$hr(), dball))
    })
    
    # Download corrected results
    output$downloadFinal <- downloadHandler(
      
      filename = function(){
        paste0("BC_corrected_values_", rv$sampleLocusName, "_", getTimestamp(), ".csv")
      },
      content = function(file){
        writeCSV(rv$finalResults, file)
      },
      contentType = "text/csv"
    )
    
    
    output$downloadAllData <- downloadHandler(
      filename = paste0("BC_all_results_", rv$sampleLocusName, "_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_", 
                        gsub("\\:", "", substr(Sys.time(), 12, 16)), ".zip"),
      content = function(fname) {
        print(getwd())
        
        # temporarily set tempdir as wd
        oldwd <- getwd()
        setwd(tempdir())
        print(getwd())
        
        # create files where is no difference in export between type 1 and 2
        writeCSV(rv$fileimportExp, paste0(csvdir, "raw_experimental_data.csv"))
        writeCSV(rv$finalResults, paste0(csvdir, "BC_corrected_values.csv"))
        writeCSV(rv$substitutions, paste0(csvdir, "BC_substituted_values.csv"))
        
        # create other files
        if (input$type_locus_sample == "1"){
          writeCSV(rv$fileimportCal, paste0(csvdir, "raw_calibration_data.csv"))
          writeCSV(rv$regStats[,-12, with=F], paste0(csvdir, "BC_regression_stats.csv"))
          
        } else if (input$type_locus_sample == "2"){
          # regression stats
          for (key in names(rv$fileimportCal)){
            writeCSV(rv$regStats[[key]][,-12, with=F],
                     paste0(csvdir, "BC_regression_stats_", gsub("[[:punct:]]", "", key), ".csv"))
          }
          
          # raw calibrations data
          for (key in names(rv$fileimportCal)){
            writeCSV(rv$fileimportCal[[key]],
                     paste0(csvdir, "raw_calibration_data_", gsub("[[:punct:]]", "", key), ".csv"))
          }
        }
        
        
        print(list.files(".csv/"))
        print(list.files(".plots/"))
        
        zip(zipfile=fname, files=c(paste0("csv/", list.files(".csv/")), 
                                   paste0("plots/", list.files(".plots/"))
        ))
        
        if(file.exists(paste0(tempdir(), "/", fname, ".zip"))){
          file.rename(paste0(tempdir(), "/", fname, ".zip"), fname)
        }
        
        # return to old wd
        setwd(oldwd)
        print(getwd())
      },
      contentType = "application/zip"
    )
    
    # present substitutions in extra tab (only if there were some)
    if (nrow(rv$substitutions) > 0){
      rv$substitutionsCalc <- TRUE
    }
  })
  
  
  # Presentation of substituted values
  observe({
    req(rv$substitutionsCalc)
    
    # appendTab("tabs", tabPanel(title = "Substituted values", value = "panel_7",
    #                            div(class="row", style="margin: 0.5%"),
    #                            dataTableOutput("substituted_values"),
    #                            downloadButton("downloadSubstituted", "Download substitutions"),
    #                            tags$hr()),
    #           select = F)
    
    # this workaround is related to this issue:
    # TODO issue: https://github.com/rstudio/shiny/issues/2116
    output$substitutedOut <- renderUI({
      h <- div(class="row", style="text-align: center",
               h4("Substituted values"))
      t <- dataTableOutput("substituted_values")
      b <- div(class="row", style="text-align: center", downloadButton("downloadSubstituted", "Download substituted values"))
      do.call(tagList, list(h, tags$hr(), t, b, tags$hr()))
    })
    # change colnames for better display
    colnames(rv$substitutions) <- c("Sample ID", "CpG site", "Corrected value", "Substituted value")
    
    
    output$downloadSubstituted <- downloadHandler(
      
      filename = function(){
        paste0("BC_substituted_values_", rv$sampleLocusName, "_", getTimestamp(), ".csv")
      },
      content = function(file){
        writeCSV(rv$substitutions, file)
      },
      contentType = "text/csv"
    )
    
    output$substituted_values <- DT::renderDataTable({
      DT::datatable(rv$substitutions, options = list(scrollX = TRUE, pageLength = 20)) %>%
        formatRound(columns=c(2:ncol(rv$fileimportExp)), digits=3)
    })
    
    #msg2 <- "Please refer to the tab 'Substituted values' for further information."
    msg2 <- "Please scroll down to the section 'Substituted values' for further information."
    if (nrow(rv$substitutions) == 1){
      msg1 <- "Substituted 1 value. "
    } else{
      msg1 <- paste0("Substituted ", nrow(rv$substitutions), " values.")
    }
    
    # show modal here
    showModal(modalDialog(
      paste(msg1, msg2),
      title = "Substituted values"
    ))
  })
}