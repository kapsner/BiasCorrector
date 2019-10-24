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


#' @title module_settings_server
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
# module_settings_server
module_settings_server <- function(input,
                                 output,
                                 session,
                                 rv,
                                 input_re) {
  
  # observe Radiobuttonevents
  observeEvent(
    eventExpr = input_re()[["moduleSettings-settings_minmax"]],
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0(
          "Settings: minmax = ",
          input_re()[["moduleSettings-settings_minmax"]]), 
        logfilename = logfilename
      )
      rv$minmax <- input_re()[["moduleSettings-settings_minmax"]]
    })
  
  # observe Radiobuttonevents
  observeEvent(
    eventExpr = input_re()[["moduleSettings-settings_selection_method"]],
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0(
          "Settings: selection_method = ",
          input_re()[["moduleSettings-settings_selection_method"]]),
        logfilename = logfilename
      )
      waround12 <- input_re()[["moduleSettings-settings_selection_method"]]
      rv$selection_method <- waround12
    })
}


#' @title module_settings_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_settings_ui
module_settings_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # type of data box
      box(
        title = "Settings",
        radioButtons(ns("settings_selection_method"),
                     label = paste0(
                       "Method to automatically (pre-) select ",
                       "the regression method for correction"),
                     choices = list(
                       "Sum of squared errors (SSE)" = "SSE",
                       "Relative Error" = "RelError"
                     ),
                     selected = "SSE"
        ),
        tags$hr(),
        checkboxInput(ns("settings_minmax"),
                      label = "Use 'min-max'-correction (default: off)",
                      value = FALSE
        ),
        helpText(
          paste0(
            "[CAUTION: This is an experimental feature ",
            "and has neither been tested nor validated!]")
        ),
        width = 9
      ),
      box(
        title = "Description",
        width = 3
      )
    )
  )
}
