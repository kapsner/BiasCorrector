library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(ggplot2)
library(magrittr)
library(polynom)


# TODO: Input-Data-Qualitychecks:
            # missing-value-handling
            # minimum 3 calibration-samples
            # column-values may not excede 100.0
            # columns may not include negative values
            # type1: column-names of calibration- and experimental data must be equal

# maximum filesize in MB:
maxfilesize <- 100

options(shiny.maxRequestSize = maxfilesize*1024^2)


# source functions
source("Functions.R", echo = F)

# define UI
ui <- fluidPage (
  
  useShinyjs(), # Include shinyjs in the UI
  # https://stackoverflow.com/questions/25062422/restart-shiny-session
  extendShinyjs(script = "reset.js", functions = "reset"), # Add the js code to the page
  
  
  # App title --> change for development
  titlePanel("BCv1"),
  # titlePanel("BiasCorrector v0.0.1"),
  # h5("based on Moskalev et al. 2011"),
  
  # Sidebar Layout with input and output definitions
  fluidRow(
    
    # Sidebar Panel
    sidebarPanel(
      
      h4(tags$b("Type of data")),
      
      # Radiobuttons: Type of data
      radioButtons(inputId = "type_locus_sample", label = h5("Please specify the type of DNA methylation data to be corrected for measurement biases"),
                   choices = list("One locus in many samples (e.g., pyrosequencing data)" = 1, 
                                  "Many loci in one sample (e.g., next-gen seq or microarray data)" = 2),
                   selected = character(0)),
      
      tags$hr(),
      
      # h4(tags$b("Mode of experimental bias correction")),
      # 
      # # Radiobuttons: Mode of experimental bias correction
      # radioButtons(inputId = "correction_mode", label = h5("Please specify the mode of correction"),
      #              choices = list("Mean methylation % of the whole locus" = 1, 
      #                             "Methylation % of individuat CpG sites" = 2),
      #              selected = 1),
      # 
      # tags$hr(),
      
      conditionalPanel(
        condition = "input.type_locus_sample == 1",
        textInput("locusname",
                  label = NULL,
                  placeholder = "Locus name")
      ),
      
      conditionalPanel(
        condition = "input.type_locus_sample == 2",
        textInput("samplename",
                  label = NULL,
                  placeholder = "Sample-ID")
      ),
      
      tags$hr(id = "tag1"),
      
      
      conditionalPanel(
        condition = "input.type_locus_sample != null",
        
        verbatimTextOutput("samplelocus_out"),
        
        tags$hr(),
        
        h4(tags$b("Datainput")),
        h5("Please upload the CSV files* containing the experimental data and the calibration data."),
        
        # Input: Select a file
        fileInput("experimentalFile", "Experimental data: choose one CSV file containing the experimental data to be corrected",
                  multiple = FALSE,
                  accept = c(".csv")),
        h6(paste("Max. filesize: ", maxfilesize, " MB")),
        
        h6("*For the specific CSV file requirements please refere to our FAQ!"),
        
        tags$hr(),
        
        conditionalPanel(
          condition =  "output.experimental_data != null",
          
          fileInput("calibrationFile", "Calibration data: choose one CSV file containing the calibration data",
                      multiple = FALSE,
                      accept = c(".csv")),
          
          h6(paste("Max. filesize: ", maxfilesize, " MB")),
          
          tags$hr(id = "tag2"),
      
      
          conditionalPanel(
            condition = "output.calibration_data != null",
            
            # Actionbutton to start analysis
            actionButton("run", "Run Analysis"),
            
            tags$hr(),
            
            # Restart session
            actionButton("reset", "Reset App")
          )
        )
    ),
    # sidepanal width in fluidlayout -> max. 12 units
    width = 3),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                    tabPanel(title = "Experimental data", value = "panel1",
                             verbatimTextOutput("exp_samples"), 
                             verbatimTextOutput("exp_samples_raw"), 
                             dataTableOutput("experimental_data"))
      )
    )
  )
)



server <- function(input, output, session) {
  
  rv <- reactiveValues(
    expFileReq = F,
    fileimportExp = NULL,
    fileimportCal = NULL,
    regStats = NULL,
    modal_closed = T,
    modal_type = NULL,
    finalResults = NULL,
    sampleLocusName = NULL
  )
  
  requirementsError <- function(data_type){
    rv$modal_closed <- F
    rv$modal_type <- data_type
    
    if (data_type %in% c("experimentalFile", "calibrationFile")){
      showModal(modalDialog(
        title = "File requirements error!",
        "The file provided does not meet the file requirements! Please upload a new file! For the specific CSV file requirements please refere to our FAQ.",
        footer = actionButton("dismiss_modal",label = "Dismiss")
      ))
    } else if (data_type == "locusname"){
      showModal(modalDialog(
        title = "No locus specified!",
        "Please specify an appropriate name for the gene or locus of your experiment!",
        footer = actionButton("dismiss_modal",label = "Dismiss")
      ))
    } else if (data_type == "samplename"){
      showModal(modalDialog(
        title = "No sample name specified!",
        "Please specify an appropriate name of the sample of your experiment!",
        footer = actionButton("dismiss_modal",label = "Dismiss")
      ))
    }
  }
  
  observeEvent(input$dismiss_modal, {
    rv$modal_closed <- T
    shinyjs::reset(rv$modal_type)
    removeModal()
    rv$modal_type <- NULL
  })
  
  observeEvent(input$reset, {
    cat("restarting app\n")
    print(input)
    js$reset()
  })
  
  output$samplelocus_out <- reactive({
    paste(rv$sampleLocusName)
  })
  
  
  
  
  ###### Experimental data
  observe({
    req(input$experimentalFile)
    # check file ending
    ending <- strsplit(input$experimentalFile$name, ".", fixed = T)[[1]]
    
    # if type 1 data
    if (input$type_locus_sample == "1"){
      # check userinput of locusname
      if (input$locusname == ""){
        requirementsError("locusname")
      } else {
        rv$expFileReq = T
        rv$sampleLocusName = handleTextInput(input$locusname)
        removeUI(selector = "#locusname", immediate = T)
        removeUI(selector = "#tag1", immediate = T)
      }
    # if type 2 data
    } else if (input$type_locus_sample == "2"){
      
      # check userinput of samplename
      if (input$samplename == ""){
        requirementsError("samplename")
      } else {
        rv$expFileReq = T
        rv$sampleLocusName = handleTextInput(input$samplename)
        removeUI(selector = "#samplename", immediate = T)
        removeUI(selector = "#tag1", immediate = T)
      }
    }
    
    if (rv$expFileReq == T){
      if (ending[2] == "csv"){
        file <- reactiveFileReader(1000, session,
                                   input$experimentalFile$datapath, 
                                   fread)
        rv$fileimportExp <- cleanDT(file(), description = "experimental", type = input$type_locus_sample)
      } else {
        # error handling fileimport
        requirementsError("experimentalFile")
      }
    }
  })
  
  # error handling with fileimport
  observeEvent({
    if (!is.null(rv$fileimportExp)) TRUE
    else return()}, {
      # disable upload possibility of experimental file
      removeUI(selector = "#experimentalFile", immediate = T)
      
      # if type 1 data
      if (input$type_locus_sample == "1"){
        
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimportExp, options = list(scrollX = TRUE, pageLength = 20)) %>%
            formatRound(columns=c(2:ncol(rv$fileimportExp)), digits=3)
        })
        
        output$exp_samples <- reactive({
          len <- unique(rv$fileimportExp[,sample_id])
          paste0("Unique samples: ", length(len))
        })
        
        output$exp_samples_raw <- reactive({
          len <- unique(rv$fileimportExp[,sample_id])
          paste0("Unique sample IDs:\n", paste(len, collapse = ", "))
        })
      
        # if type 2 data
      } else if (input$type_locus_sample == "2"){
        
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimportExp, options = list(scrollX = TRUE, pageLength = 20)) %>%
            formatRound(columns=c(2:ncol(rv$fileimportExp)), digits=3)
        })
        
        output$exp_samples <- reactive({
          len <- unique(rv$fileimportExp[,locus_id])
          paste0("Unique loci: ", length(len))
        })

        output$exp_samples_raw <- reactive({
          len <- unique(rv$fileimportExp[,locus_id])
          paste0("Unique locus IDs:\n", paste(len, collapse = ", "))
        })
        
      }
    })
  
  
  
  observeEvent(input$calibrationFile, {
    # error handling, when uploading new data in same session
    removeTab("tabs", "panel2")
    appendTab("tabs", tabPanel(title = "Calibration data", value = "panel2",
                               verbatimTextOutput("cal_samples"),
                               verbatimTextOutput("cal_samples_raw"),
                               dataTableOutput("calibration_data")),
              select = T)
  }
  )
  
  
  ###### Calibration data
  observe({
    req(input$calibrationFile)
    # check file ending
    ending <- strsplit(input$calibrationFile$name, ".", fixed = T)[[1]]
    if (ending[2] == "csv"){
      file <- reactiveFileReader(1000, session,
                                 input$calibrationFile$datapath, 
                                 fread)
      rv$fileimportCal <- cleanDT(file(), "calibration", type = input$type_locus_sample)
    } else {
      # error handling fileimport
      requirementsError("calibrationFile")
    }
  })
  
  
  
  # error handling with fileimport
  observeEvent({
    if (!is.null(rv$fileimportCal)) TRUE
    else return()}, {
      # disable upload possibility of calibration file
      removeUI(selector = "#calibrationFile", immediate = T)
      
      output$calibration_data <- DT::renderDataTable({
        DT::datatable(rv$fileimportCal, options = list(scrollX = TRUE, pageLength = 20)) %>%
          formatRound(columns=c(2:ncol(rv$fileimportCal)), digits=3)
      })
      
      output$cal_samples <- reactive({
        len <- unique(rv$fileimportCal[,true_methylation])
        paste0("Unique calibration samples: ", length(len))
      })
      
      output$cal_samples_raw <- reactive({
        len <- unique(rv$fileimportCal[,true_methylation])
        paste0("Unique calibration steps:\n", paste(len, collapse = "\n"))
      })
    })
  
  
  ###### Run Analysis
  observeEvent(input$run, {
    removeTab("tabs", "panel3")
    removeTab("tabs", "panel4")
    removeTab("tabs", "panel5")
    removeUI(selector = "#run", immediate = T)
    removeUI(selector = "#tag2", immediate = T)
    appendTab("tabs", tabPanel(title = "Regression plots",  value = "panel3", uiOutput("plots")), select = T)
    
    # updateSelectInput(session, "reg_1", 
    #                   choices = vec_cal,
    #                   selected = "rowmeans")
    
    initializeListJ()
    
    # for plotting: basic idea and some code snippets from:
    # https://gist.github.com/wch/5436415/
    plot.list <- reactive({
      regression_type1(rv$fileimportCal, vec_cal)
    })
    
    for (i in 1:length(vec_cal)) {
      local({
        f <- i
        plotname <- paste("plot", f, sep="")
        output[[plotname]] <- renderPlot({
          
          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          progress$set(message = paste("Making plot", f), value = 0)
          
          # Increment the progress bar, and update the detail text.
          progress$inc(1/1, detail = paste("... working hard on plot", f))
          
          # append new tab based on calculation
          if (f >= length(vec_cal)) {
            appendTab("tabs", tabPanel(title = "Regression statistics",  value = "panel4",
                                       dataTableOutput("regression_statistics"),
                                       tags$hr(),
                                       downloadButton("downloadRegStat", "Download regression statistics")),
                      select = F)
            appendTab("tabs", tabPanel(title = "Select regression model",  value = "panel5",
                                       uiOutput("reg_radios"),
                                       div(class="row",
                                           div(class="col-sm-9", style="display: inline-block",
                                               actionButton("results", "Calculate results for experimental data"),
                                               style="text-align: center"))),
                      select = F)
          }
          
          # add plot and plot statistics here, "j" is necessary to get values for curve in equations
          plot.list()[[f]] +
            stat_function(fun = hyperbolic_equation, geom = "line", aes(colour = "Hyperbolic")) +
            stat_function(fun = cubic_equation, geom = "line", aes(colour = "Cubic")) + 
            scale_colour_manual("Regression:", values = c("red", "green"))
        })
      })
    }
  })
  
  output$plots <- renderUI({ 
    plot_output_list <- lapply(1:length(vec_cal), function(g) {
      plotname <- paste("plot", g, sep="")
      plotOutput(plotname, height = 500, width = 700)
    })
    do.call(tagList, plot_output_list) # needed to display properly.
  })
  
  
  
  ###### Regression statistics
  output$regression_statistics <- DT::renderDataTable({
    rv$regStats <- statisticsList(result_list)
    dt <- rv$regStats
    # use formatstyle to highlight lower SSE values
    # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
    DT::datatable(dt, colnames = c("Name", "SSE (h)", "b", "y0", "y1", "  ", "SSE (c)", "ax³", "bx²", "cx", "d", "better_model"),
                  options = list(scrollX = TRUE, 
                                 pageLength = 20,
                                 columnDefs = list(list(targets = 12, visible = FALSE))
                  )) %>%
      formatRound(columns=c(2:12), digits=3) %>%
      formatStyle(columns = 2,
                  valueColumns = "better_model",
                  fontWeight = styleEqual(0, "bold")) %>%
      formatStyle(columns = 2:5,
                  valueColumns = "better_model",
                  backgroundColor = styleEqual(0, "lawngreen")) %>%
      formatStyle(columns = 7,
                  valueColumns = "better_model",
                  fontWeight = styleEqual(1, "bold")) %>%
      formatStyle(columns = 7:11,
                  valueColumns = "better_model",
                  backgroundColor = styleEqual(1, "lawngreen")) %>%
      formatStyle(columns = c(1:11), fontSize = "80%")
  })
  
  output$reg_radios <- renderUI({ 
    radio_output_list <- lapply(1:length(vec_cal), function(g) {
      radioname <- paste0("radio", g)
      div(class="row",
          div(class="row",
              div(class="col-sm-4", style="text-align: center",
                  h5(tags$b(paste0(vec_cal[g], ":")))),
              div(class="col-sm-4",
                  radioButtons(inputId = radioname, 
                               label = "Regression type:", 
                               choices = list("hyperbolic" = 0, "cubic" = 1),
                               selected = as.character(rv$regStats[Name==vec_cal[g], better_model]),
                               inline = T)),
              div(class="col-sm-3",
                  verbatimTextOutput(paste0("text_", radioname)))),
          tags$hr(style="margin: 0.5%"))
    })
    do.call(tagList, radio_output_list) # needed to display properly.
  })
  
  observe({
    if (input$tabs == "panel5"){
      lapply(1:length(vec_cal), function(h) {
        radioname <- paste0("text_radio", h)
        output[[radioname]] <- reactive({
          paste("SSE:",
                as.character(ifelse(input[[paste0("radio", h)]] == 1,
                                    rv$regStats[Name==vec_cal[h], SSE_cubic],
                                    rv$regStats[Name==vec_cal[h], SSE_hyperbolic]))
          )
        })
      })
    }
  })
  
  observe({
    if (input$tabs == "panel5"){
      lapply(1:length(vec_cal), function(k) {
        radioname <- paste0("radio", k)
        if (!is.null(input[[radioname]])){
          if (input[[radioname]] == "1") {
            output[[paste0("text_", radioname)]] <- reactive({
              paste("SSE:",
                    as.character(rv$regStats[Name==vec_cal[k], round(SSE_cubic,3)])
              )
            })
          } else if (input[[radioname]] == "0") {
            output[[paste0("text_", radioname)]] <- reactive({
              paste("SSE:",
                    as.character(rv$regStats[Name==vec_cal[k], round(SSE_hyperbolic, 3)])
              )
            })
          }
        }
      })
    }
  })
  
  output$downloadRegStat <- downloadHandler(
    filename = function(){
      paste0("BCstats_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_", 
             gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
    },
    content = function(file){
      write.table(rv$regStats[,-12, with=F], file, 
                  row.names = F, 
                  sep = ",", 
                  dec = ".", 
                  fileEncoding = "UTF-8")
    }
  )
  
  
  # Calculate results
  observeEvent(input$results, {
    choices_list <- data.table("Name" = character(), "better_model" = numeric())
    lapply(1:length(vec_cal), function(x) {
      radioname <- paste("radio", x, sep="")
      choices_list <<- rbind(choices_list, cbind("Name" = vec_cal[x], "better_model" = as.numeric(eval(parse(text=paste0("input$", radioname))))))
    })
    print(choices_list)
    
    rv$finalResults <<- solving_equations(rv$fileimportExp, choices_list)
    
    removeTab("tabs", "panel6")
    appendTab("tabs", tabPanel(title = "Corrected values", value = "panel6",
                               dataTableOutput("corrected_data"),
                               tags$hr(),
                               downloadButton("downloadFinal", "Download corrected values")),
              select = T)
  })
  
  output$corrected_data <- DT::renderDataTable({
    # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
    DT::datatable(rv$finalResults,
                  options = list(scrollX = TRUE, pageLength = 20)
                  ) %>%
      formatRound(columns=c(2:ncol(rv$finalResults)), digits=3)
  })
  
  output$downloadFinal <- downloadHandler(
    
    filename = function(){
      paste0("Results_", rv$sampleLocusName, gsub("\\-", "", substr(Sys.time(), 1, 10)), "_", 
             gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
    },
    content = function(file){
      write.table(rv$finalResults, file, 
                  row.names = F, 
                  sep = ",", 
                  dec = ".", 
                  fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
