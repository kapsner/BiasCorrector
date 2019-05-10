moduleStatisticsServer <- function(input, output, session, rv, input_re){
  observe({
    req(rv$plotting_finished)
    
    # type 1 data:
    if (rv$type_locus_sample == "1"){
      output$regression_statistics <- renderUI({
        output$dt_reg <- DT::renderDataTable({
          dt <- rv$regStats
          stats_debug <<- dt
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
        selectInput(inputId="selectRegStatsLocus", label = NULL, multiple = F, selectize = F, choices = names(rv$fileimportCal))
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
      # TODO align selectinput and button aside of each other
      output$regression_statistics <- renderUI({
        s1 <- selInLocus()
        dt <- DT::dataTableOutput("moduleStatistics-dt_regs")
        db <- div(class="row", style="text-align: center", actionButton("results", "BiasCorrect your experimental data"))
        do.call(tagList, list(s1, dt, db))
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
      box(
        title = "Regression Statistics",
        div(class="row", style="margin: 0.5%"),
        uiOutput(ns("regression_statistics")),
        tags$hr(),
        div(class="row", style="text-align: center", downloadButton(ns("downloadRegStat"), "Download regression statistics")),
        tags$hr(),
        width = 12
      ))
  )
}