moduleStatisticsServer <- function(input, output, session, rv, input_re){
  observe({
    req(rv$plotting_finished)
    
    output$description <- renderText({
      str1 <- "The table shows the regression parameters of the hyperbolic regression and the cubic regression.<br/>"
      str2 <- "Column 1 presents the CpG-site's name."
      str3 <- "Column 2 presents the mean of the relative absolute errors for every CpG-site."
      str4 <- "Columns 3-6 present the sum of squared error of the hyperbolic regression ('SSE [h]') and the regression parameters used to calculate the hyperbolic regression curves for the respective CpG-site."
      str5 <- "Columns 7-11 present the sum of squared error of the cubic regression ('SSE [c]') and the regression parameters used to calculate the cubic regression curves."
      str6 <- "The rows highlighted with a green background colour indicate the regression equation, that in comparison of the sum of squared errors better fits the data points for the respecitve CpG-site."
      
      
      HTML(paste(str1, str2, str3, str4, str5, str6, sep = "<br/><br/>"))
    })
    
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
          writeCSV(rv$regStats[,-(which(colnames(rv$regStats)=="better_model")), with=F], file)
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
        do.call(tagList, list(div(class="row", style="text-align: center", actionButton("results", "BiasCorrect experimental data", style="white-space: normal; text-align:center; 
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))))
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
          writeCSV(rv$regStats[[input_re()$selectRegStatsLocus]][,-(which(colnames(rv$regStats[[input_re()$selectRegStatsLocus]])=="better_model")), with=F], file)
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
                 div(class="row", style="text-align: center", downloadButton(ns("downloadRegStat"), "Download regression statistics", style="white-space: normal; text-align:center; 
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
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
             ),
             box(title = "Description",
                 htmlOutput(ns("description")),
                 #tags$head(tags$style("#moduleStatistics-description{overflow-y:visible; overflow-x:visible; width=100vh; word-wrap: break-word; background: ghostwhite;}")),
                 width = 12
             )
      )
    )
  )
}