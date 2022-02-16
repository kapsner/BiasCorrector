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


#' @title module_modelselection_server
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
#'   module_modelselection_server,
#'   "moduleModelSelection",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_modelselection_server
module_modelselection_server <- function(input,
                                         output,
                                         session,
                                         rv,
                                         input_re) {
  observe({
    req(rv$better_model_stats)
    # model selection only implemented for type 1 data
    if (rv$type_locus_sample == "1") {

      # select all at once
      observeEvent(
        eventExpr = input_re()[["moduleModelSelection-reg_all"]],
        handlerExpr = {
          if (input_re()[["moduleModelSelection-reg_all"]] %in% c("0", "1")) {
            rv$radioselection <- rep(
              input_re()[["moduleModelSelection-reg_all"]],
              times = length(rv$vec_cal)
            )
          } else {
            rv$radioselection <- as.character(
              rv$better_model_stats[, get("better_model")]
            )
          }
        })
      if (is.null(rv$radioselection)) {
        rv$radioselection <- as.character(
          rv$better_model_stats[, get("better_model")]
        )
      }

      # render radio buttons for tab 5
      output$reg_radios <- renderUI({
        radio_output_list <- lapply(
          seq_len(length(rv$vec_cal)),
          function(g) {
            radioname <- paste0("radio", g)
            div(
              class = "row",
              style = "margin: 0.5%; text-align: center;",
              div(
                class = "col-sm-4",
                style = "text-align: left;",
                h5(tags$b(paste0("Regression type for ",
                                 rv$vec_cal[g], ":")))
              ),
              div(
                class = "col-sm-4",
                style = "text-align: left;",
                div(
                  class = "row",
                  style = "text-align: center;",
                  radioButtons(
                    inputId = paste0("moduleModelSelection-",
                                     radioname),
                    label = NULL,
                    choices = list(
                      "hyperbolic" = "0",
                      "cubic" = "1"
                    ),
                    selected = as.character(rv$radioselection[g]),
                    inline = TRUE
                  )
                )
              ),
              div(
                class = "col-sm-4",
                verbatimTextOutput(
                  paste0("moduleModelSelection-text_",
                         radioname))
              )
            )
          })
        do.call(tagList,
                list(radio_output_list)) # needed to display properly.
      })
    } else if (rv$type_locus_sample == "2") {
      # type 2 data:
      # trigger claculation of results (bypass manual model selection)
      #% shinyjs::click("results")
    }
  })


  observe({
    req(rv$better_model_stats)

    if (rv$type_locus_sample == "1") {
      lapply(seq_len(length(rv$vec_cal)), function(k) {
        radioname <- paste0("radio", k)

        if (!is.null(input_re()[[paste0("moduleModelSelection-",
                                        radioname)]])) {
          if (rv$selection_method == "SSE") {
            output[[paste0("text_", radioname)]] <- reactive({
              paste(
                "SSE:",
                as.character(
                  ifelse(
                    input_re()[[paste0("moduleModelSelection-",
                                       radioname)]] == "1",
                    rv$better_model_stats[
                      get("Name") == rv$vec_cal[k], round(
                        get("SSE_cubic"),
                        3)
                      ],
                    rv$better_model_stats[
                      get("Name") == rv$vec_cal[k], round(
                        get("SSE_hyperbolic"),
                        3)
                      ]
                  ))
              )
            })
          } else if (rv$selection_method == "RelError") {
            output[[paste0("text_", radioname)]] <- reactive({
              paste(
                "Rel.Error:",
                as.character(
                  ifelse(input_re()[[paste0("moduleModelSelection-",
                                            radioname)]] == "1",
                         rv$better_model_stats[
                           get("Name") == rv$vec_cal[k], round(
                             get("relative_error_c"),
                             3)
                           ],
                         rv$better_model_stats[
                           get("Name") == rv$vec_cal[k], round(
                             get("relative_error_h"),
                             3)
                           ]
                  ))
              )
            })
          }
        }
      })
    }
  })
}


#' @title module_modelselection_ui
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
#'     tabName = "modelselection",
#'     module_modelselection_ui(
#'       "moduleModelSelection"
#'     )
#'   )
#' )
#' }
#'
#' @export
#'
# module_modelselection_ui
module_modelselection_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        title = "Select Regression Model",
        radioButtons(
          ns("reg_all"),
          label = "Select algorithm for all CpG sites",
          choices = list(
            "best" = "2",
            "hyperbolic" = "0",
            "cubic polynomial" = "1"
          ),
          selected = character(0), inline = TRUE
        ),
        uiOutput(ns("reg_radios")),
        width = 12
      )
    )
  )
}
