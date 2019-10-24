# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
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


#' @title module_statistics_server
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive
#'   expression: input_re = reactive({input})
#'
#' @export
#'
# module_statistics_server
module_statistics_server <- function(input,
                                   output,
                                   session,
                                   rv,
                                   input_re) {
  observe({
    req(rv$reg_stats_corrected_c)
    
    output$description <- renderText({
      str1 <- paste0("The table shows the regression parameters ",
                     "of the hyperbolic regression and the cubic ",
                     "regression.<br/>")
      str2 <- paste0("Column 1 presents the CpG site's name.")
      str3 <- paste0("Column 2 presents the mean of the relative errors ",
                     "for every CpG site. It is calculated for every ",
                     "CpG site as a mean of relative errors between the ",
                     "true and the observed methylation degrees of each ",
                     "methylation step across all available calibrator ",
                     "DNAs for the respective CpG site. ",
                     "<br/>Formula:<br/> <i>abs(methylation_true - ",
                     "methylation_observed) / methylation_true </i>")
      str4 <- paste0("Columns 3-7 present the sum of squared error of the ",
                     "hyperbolic regression ('SSE [h]'), the coefficient ",
                     "of determination ('R\u00B2 [h]') and the regression ",
                     "parameters used to calculate the hyperbolic ",
                     "regression curves for the respective CpG site.")
      str5 <- paste0("Columns 8-13 present the sum of squared error of the ",
                     "cubic regression ('SSE [c]'), the coefficient of ",
                     "determination ('R\u00B2 [c]') and the regression ",
                     "parameters used to calculate the cubic regression ",
                     "curves.")
      str6 <- paste0("The rows highlighted with a green background colour ",
                     "indicate the regression equation that is automatically ",
                     "selected for the correction of measurement biases ",
                     "based on the method, selected in the 'Settings'-tab.")
      str7 <- paste0("The bold marked sum of squared errors indicate, that ",
                     "in comparison of the SSE this regression equation ",
                     "better fits the data points for the respecitve CpG ",
                     "site.")
      
      
      HTML(
        paste(
          str1,
          str2,
          str3,
          str4,
          str5,
          str6,
          str7,
          sep = "<br/><br/>"
        )
      )
    })
    
    # type 1 data:
    if (rv$type_locus_sample == "1") {
      rv$better_model_stats <- rBiasCorrection::better_model(
        statstable_pre = rv$reg_stats,
        statstable_post_hyperbolic = rv$reg_stats_corrected_h,
        statstable_post_cubic = rv$reg_stats_corrected_c,
        selection_method = rv$selection_method
      )
      
      
      output$regression_statistics <- renderUI({
        output$dt_reg <- DT::renderDataTable({
          # use formatstyle to highlight lower SSE values
          render_regressionstatistics(
            dt = rv$reg_stats[
              , ("better_model") := rv$better_model_stats[
                , get("better_model")
                ]
              ],
            minmax = rv$minmax)
        })
        d <- DT::dataTableOutput("moduleStatistics-dt_reg")
        do.call(tagList, list(d))
      })
      
      # create download button for regression statistics
      output$download_regstat <- downloadHandler(
        filename = function() {
          paste0(
            rv$sample_locus_name,
            "_regression_stats_",
            gsub("\\-",
                 "",
                 substr(Sys.time(), 1, 10)),
            "_",
            gsub("\\:",
                 "",
                 substr(Sys.time(), 12, 16)),
            ".csv"
          )
        },
        content = function(file) {
          rBiasCorrection::write_csv(
            table = rv$reg_stats[
              , -which(colnames(rv$reg_stats) == "better_model"), with = F
              ], 
            filename = file)
        },
        contentType = "text/csv"
      )
      
      # type 2 data:
    } else if (rv$type_locus_sample == "2") {
      
      # create reactive selectinput:
      sel_in_locus <- reactive({
        selectInput(
          inputId = "selectRegStatsLocus",
          label = "Select locus:",
          multiple = F,
          selectize = F, 
          choices = names(rv$fileimport_calibration))
      })
      
      # create reactive df-selection:
      df_regs <- reactive({
        dt <- rv$reg_stats[[input_re()$selectRegStatsLocus]]
      })
      
      output$dt_regs <- DT::renderDataTable({
        dt <- df_regs()
        render_regressionstatistics(dt = dt,
                                    minmax = rv$minmax)
      })
      
      # render head of page with selectInput and downloadbutton
      
      output$statistics_select <- renderUI({
        s1 <- sel_in_locus()
        do.call(tagList, list(s1, tags$hr()))
      })
      
      output$biascorrection <- renderUI({
        do.call(tagList,
                list(
                  div(
                    class = "row",
                    style = "text-align: center",
                    actionButton(
                      "results", "BiasCorrect experimental data",
                      style = paste0(
                        "white-space: normal; ",
                        "text-align:center; ",
                        "padding: 9.5px 9.5px 9.5px 9.5px; ",
                        "margin: 6px 10px 6px 10px;")
                    )
                  )
                )
        )
      })
      
      output$regression_statistics <- renderUI({
        dt <- DT::dataTableOutput("moduleStatistics-dt_regs")
        do.call(tagList, list(dt))
      })
      
      # create download button for regression statistics
      output$download_regstat <- downloadHandler(
        filename = function() {
          paste0(
            rv$sample_locus_name,
            "_regression_stats_",
            gsub("[[:punct:]]",
                 "",
                 input_re()$selectRegStatsLocus),
            "_",
            gsub("\\-",
                 "", 
                 substr(Sys.time(), 1, 10)),
            "_",
            gsub("\\:",
                 "",
                 substr(Sys.time(), 12, 16)),
            ".csv"
          )
        },
        content = function(file) {
          rBiasCorrection::write_csv(
            table = rv$reg_stats[[input_re()$selectRegStatsLocus]][
              , -which(
                colnames(
                  rv$reg_stats[[input_re()$selectRegStatsLocus]]
                ) == "better_model"
              ), with = F
              ],
            filename = file)
        },
        contentType = "text/csv"
      )
    }
  })
}


#' @title module_statistics_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_statistics_ui
module_statistics_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        9,
        box(
          title = "Regression Statistics",
          uiOutput(ns("regression_statistics")),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Download Regression Statistics",
          uiOutput(ns("statistics_select")),
          div(class =
                "row",
              style = "text-align: center",
              downloadButton(
                ns("download_regstat"),
                "Download regression statistics",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;"
                )
              )
          ),
          tags$hr(),
          width = 12
        ),
        conditionalPanel(
          condition = "input['moduleFileupload-type_locus_sample'] == 2",
          box(
            title = "BiasCorrect Experimental Data",
            uiOutput(ns("biascorrection")),
            tags$hr(),
            width = 12
          )
        ),
        box(
          title = "Description",
          htmlOutput(ns("description")),
          width = 12
        )
      )
    )
  )
}
