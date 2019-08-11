# BiasCorrector: A GUI to Correct PCR Bias in DNA Methylation Analyses
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


#' @title moduleCorrectedStatisticsServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleCorrectedStatisticsServer
moduleCorrectedStatisticsServer <- function(input, output, session, rv, input_re){

  # when plotting has finished
  observe({
    req(rv$corrected_finished)
      ## regression statistics
      output$regression_statistics_corrected_h <- renderUI({
        output$dt_reg_corrected_h <- DT::renderDataTable({
          dt <- rv$regStats_corrected_h
          # use formatstyle to highlight lower SSE values
          renderRegressionStatisticTable(dt, mode = "corrected", minmax = rv$minmax)
        })
        d <- DT::dataTableOutput("moduleCorrectedStatistics-dt_reg_corrected_h")
        do.call(tagList, list(d))
      })

      output$regression_statistics_corrected_c <- renderUI({
        output$dt_reg_corrected_c <- DT::renderDataTable({
          dt <- rv$regStats_corrected_c
          # use formatstyle to highlight lower SSE values
          renderRegressionStatisticTable(dt, mode = "corrected", minmax = rv$minmax)
        })
        d <- DT::dataTableOutput("moduleCorrectedStatistics-dt_reg_corrected_c")
        do.call(tagList, list(d))
      })

      # create download button for regression statistics
      output$downloadRegStat_corrected_h <- downloadHandler(
        filename = function(){
          paste0("BC_regression_stats_corrected_h_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_",
                 gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
        },
        content = function(file){
          PCRBiasCorrection::writeCSV_(rv$regStats_corrected_h[,-(which(colnames(rv$regStats_corrected_h)=="better_model")), with=F], file)
        },
        contentType = "text/csv"
      )
      output$downloadRegStat_corrected_c <- downloadHandler(
        filename = function(){
          paste0("BC_regression_stats_corrected_c_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_",
                 gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
        },
        content = function(file){
          PCRBiasCorrection::writeCSV_(rv$regStats_corrected_c[,-(which(colnames(rv$regStats_corrected_c)=="better_model")), with=F], file)
        },
        contentType = "text/csv"
      )
      
      
      # substitutions
      output$substitutions_corrected_h <- DT::renderDataTable({
        DT::datatable(rv$substitutions_corrected_h, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
          DT::formatRound(columns=c(3:4), digits=3)
      })
      
      output$substitutions_corrected_c <- DT::renderDataTable({
        DT::datatable(rv$substitutions_corrected_c, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
          DT::formatRound(columns=c(3:4), digits=3)
      })
      
      output$downloadSubstitutions_corrected_h <- downloadHandler(
        
        filename = function(){
          paste0("BC_substituted_values_corrected_h_", rv$sampleLocusName, "_", PCRBiasCorrection::getTimestamp_(), ".csv")
        },
        content = function(file){
          PCRBiasCorrection::writeCSV_(rv$substitutions_corrected_h, file)
        },
        contentType = "text/csv"
      )
      
      output$downloadSubstitutions_corrected_c <- downloadHandler(
        
        filename = function(){
          paste0("BC_substituted_values_corrected_c_", rv$sampleLocusName, "_", PCRBiasCorrection::getTimestamp_(), ".csv")
        },
        content = function(file){
          PCRBiasCorrection::writeCSV_(rv$substitutions_corrected_c, file)
        },
        contentType = "text/csv"
      )
  })
}

#' @title moduleCorrectedStatisticsUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleCorrectedStatisticsUI
moduleCorrectedStatisticsUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(9,
             box(title = "Regression Statistics (corrected)",
                 tabsetPanel(
                   tabPanel("Hyperbolic Correction",
                            uiOutput(ns("regression_statistics_corrected_h"))
                   ),
                   tabPanel("Cubic Correction",
                            uiOutput(ns("regression_statistics_corrected_c"))
                   )
                 ),
                 width = 12
             )),
      column(3,
             box(title = "Download Regression Statistics (corrected)",
                 uiOutput(ns("statistics_select")),
                 div(class="row", style="text-align: center", downloadButton(ns("downloadRegStat_corrected_h"), "Download regression statistics (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
                 div(class="row", style="text-align: center", downloadButton(ns("downloadRegStat_corrected_c"), "Download regression statistics (cubic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
                 tags$hr(),
                 width = 12
             )
      )
    ),
    fluidRow(
      column(9,
             box(title = "Substitutions (corrected)",
                 tabsetPanel(
                   tabPanel("Hyperbolic Correction",
                            DT::dataTableOutput(ns("substitutions_corrected_h"))
                   ),
                   tabPanel("Cubic Correction",
                            DT::dataTableOutput(ns("substitutions_corrected_c"))
                   )
                 ),
                 width = 12
             )),
      column(3,
             box(title = "Download substitutions (corrected)",
                 div(class="row", style="text-align: center", downloadButton(ns("downloadSubstitutions_corrected_h"), "Download substitutions (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
                 div(class="row", style="text-align: center", downloadButton(ns("downloadSubstitutions_corrected_c"), "Download substitutions (cubic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
                 tags$hr(),
                 width = 12
             )
      )
    )
  )
}
