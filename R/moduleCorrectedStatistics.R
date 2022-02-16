# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
# Copyright (C) 2019-2022 Lorenz Kapsner
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


#' @title module_correctedstats_server
#'
#' @inheritParams module_calibrationfile_server
#'
#' @return The function returns a shiny server module.
#'
#' @seealso \url{https://shiny.rstudio.com/articles/modules.html}
#'
#' @examples
#' if (interactive()) {
#' rv <- list()
#' logfilename <- paste0(tempdir(), "/log.txt")
#' shiny::callModule(
#'   module_correctedstats_server,
#'   "moduleCorrectedStats",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_correctedstats_server
module_correctedstats_server <- function(input,
                                         output,
                                         session,
                                         rv,
                                         input_re) {
  # when plotting has finished
  observe({
    req(rv$corrected_finished)
    ## regression statistics
    output$regression_stats_corrected_h <- renderUI({
      output$dt_reg_corrected_h <- DT::renderDataTable({
        dt <- rv$reg_stats_corrected_h
        # use formatstyle to highlight lower SSE values
        render_regressionstatistics(dt,
                                    mode = "corrected",
                                    minmax = rv$minmax)
      })
      d <- DT::dataTableOutput("moduleCorrectedStatistics-dt_reg_corrected_h")
      do.call(tagList, list(d))
    })
    output$regression_stats_corrected_c <- renderUI({
      output$dt_reg_corrected_c <- DT::renderDataTable({
        dt <- rv$reg_stats_corrected_c
        # use formatstyle to highlight lower SSE values
        render_regressionstatistics(dt,
                                    mode = "corrected",
                                    minmax = rv$minmax)
      })
      d <- DT::dataTableOutput("moduleCorrectedStatistics-dt_reg_corrected_c")
      do.call(tagList, list(d))
    })
    # create download button for regression statistics
    output$download_regstat_corrected_h <- downloadHandler(
      filename = function() {
        paste0(
          rv$sample_locus_name,
          "_corrected_regression_stats_h_",
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
          table = rv$reg_stats_corrected_h[, -which(
            colnames(rv$reg_stats_corrected_h) == "better_model")
            , with = FALSE],
          filename = file)
      },
      contentType = "text/csv"
    )
    output$download_regstat_corrected_c <- downloadHandler(
      filename = function() {
        paste0(
          rv$sample_locus_name,
          "_corrected_regression_stats_c_",
          gsub("\\-", "", substr(Sys.time(), 1, 10)), "_",
          gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv"
        )
      },
      content = function(file) {
        rBiasCorrection::write_csv(
          table = rv$reg_stats_corrected_c[, -which(
            colnames(rv$reg_stats_corrected_c) == "better_model")
            , with = FALSE],
          filename = file)
      },
      contentType = "text/csv"
    )

    # substitutions
    output$substitutions_corrected_h <- DT::renderDataTable({
      DT::datatable(rv$substitutions_corrected_h,
                    options = list(scrollX = TRUE,
                                   pageLength = 20,
                                   dom = "ltip"),
                    rownames = FALSE) %>%
        DT::formatRound(columns = c(3:4), digits = 3)
    })
    output$substitutions_corrected_c <- DT::renderDataTable({
      DT::datatable(rv$substitutions_corrected_c,
                    options = list(scrollX = TRUE,
                                   pageLength = 20,
                                   dom = "ltip"),
                    rownames = FALSE) %>%
        DT::formatRound(columns = c(3:4), digits = 3)
    })
    output$download_subs_corrected_h <- downloadHandler(
      filename = function() {
        paste0(rv$sample_locus_name,
               "_substituted_corrected_h_",
               rBiasCorrection::get_timestamp(), ".csv")
      },
      content = function(file) {
        rBiasCorrection::write_csv(
          table = rv$substitutions_corrected_h,
          filename = file)
      },
      contentType = "text/csv"
    )
    output$download_subs_corrected_c <- downloadHandler(
      filename = function() {
        paste0(rv$sample_locus_name,
               "_substituted_corrected_c_",
               rBiasCorrection::get_timestamp(),
               ".csv")
      },
      content = function(file) {
        rBiasCorrection::write_csv(
          table = rv$substitutions_corrected_c,
          filename = file)
      },
      contentType = "text/csv"
    )
  })
}

#' @title module_correctedstatistics_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @return The function returns a shiny ui module.
#'
#' @seealso \url{https://shiny.rstudio.com/articles/modules.html}
#'
#' @examples
#' if (interactive()) {
#' shinydashboard::tabItems(
#'   shinydashboard::tabItem(
#'     tabName = "correctedstats",
#'     module_correctedstatistics_ui(
#'       "moduleCorrectedStats"
#'     )
#'   )
#' )
#' }
#'
#' @export
#'
# module_correctedstatistics_ui
module_correctedstatistics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        9,
        box(
          title = "Regression Statistics (corrected)",
          tabsetPanel(
            tabPanel(
              "Hyperbolic Correction",
              uiOutput(ns("regression_stats_corrected_h"))
            ),
            tabPanel(
              "Cubic Correction",
              uiOutput(ns("regression_stats_corrected_c"))
            )
          ),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Download Regression Statistics (corrected)",
          uiOutput(ns("statistics_select")),
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                ns("download_regstat_corrected_h"),
                "Download regression statistics (hyperbolic correction)",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;"))),
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                ns("download_regstat_corrected_c"),
                "Download regression statistics (cubic correction)",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;"))),
          tags$hr(),
          width = 12
        )
      )
    ),
    fluidRow(
      column(
        9,
        box(
          title = "Substitutions (corrected)",
          tabsetPanel(
            tabPanel(
              "Hyperbolic Correction",
              DT::dataTableOutput(ns("substitutions_corrected_h"))
            ),
            tabPanel(
              "Cubic Correction",
              DT::dataTableOutput(ns("substitutions_corrected_c"))
            )
          ),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Download substitutions (corrected)",
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                ns("download_subs_corrected_h"),
                "Download substitutions (hyperbolic correction)",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;"))),
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                ns("download_subs_corrected_c"),
                "Download substitutions (cubic correction)",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;"))),
          tags$hr(),
          width = 12
        )
      )
    )
  )
}
