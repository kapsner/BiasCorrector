moduleStatisticsServer <- function(input, output, session, rv, input_re){
  observe({
    req(rv$plotting_finished)
    
    # type 1 data:
    if (rv$type_locus_sample == "1"){
      output$regression_statistics <- renderUI({
        output$dt_reg <- DT::renderDataTable({
          dt <- rv$regStats
          # use formatstyle to highlight lower SSE values
          renderRegressionStatisticTable(dt)
        })
        d <- DT::dataTableOutput("moduleStatistics-dt_reg")
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
      
      # type 2 data:
    } else if (rv$type_locus_sample == "2"){
      
      # create reactive selectinput:
      selInLocus <- reactive({
        selectInput(inputId="selectRegStatsLocus", label = "Select locus:", multiple = F, selectize = F, choices = names(rv$fileimportCal))
      })
      
      # create reactive df-selection:
      df_regs <- reactive({
        dt <- rv$regStats[[input_re()$selectRegStatsLocus]]
      })
      
      output$dt_regs <- DT::renderDataTable({
        dt <- df_regs()
        renderRegressionStatisticTable(dt)
      })
      
      # render head of page with selectInput and downloadbutton
      
      output$statistics_select <- renderUI({
        s1 <- selInLocus()
        do.call(tagList, list(s1, tags$hr()))
      })
      
      output$biascorrection <- renderUI({
        do.call(tagList, list(div(class="row", style="text-align: center", actionButton("results", "BiasCorrect your experimental data"))))
      })
      
      output$regression_statistics <- renderUI({
        dt <- DT::dataTableOutput("moduleStatistics-dt_regs")
        do.call(tagList, list(dt))
      })
      
      # create download button for regression statistics
      output$downloadRegStat <- downloadHandler(
        filename = function(){
          paste0("BC_regression_stats_", gsub("[[:punct:]]", "", input_re()$selectRegStatsLocus), "_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_",
                 gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
        },
        content = function(file){
          writeCSV(rv$regStats[[input_re()$selectRegStatsLocus]][,-12, with=F], file)
        },
        contentType = "text/csv"
      )
    }
  })
}

moduleStatisticsUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(9,
             box(title = "Regression Statistics",
                 uiOutput(ns("regression_statistics")),
                 width = 12
             )),
      column(3,
             box(title = "Download Regression Statistics",
                 uiOutput(ns("statistics_select")),
                 div(class="row", style="text-align: center", downloadButton(ns("downloadRegStat"), "Download regression statistics")),
                 tags$hr(),
                 width = 12
             ),
             conditionalPanel(
               condition = "input['moduleFileupload-type_locus_sample'] == 2",
               box(title = "BiasCorrect Experimental Data",
                   uiOutput(ns("biascorrection")),
                   tags$hr(),
                   width = 12
               )
             )
      )
    )
  )
}