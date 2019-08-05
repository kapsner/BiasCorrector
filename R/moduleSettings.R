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


#' @title moduleSettingsServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleSettingsServer
moduleSettingsServer <- function(input, output, session, rv, input_re){

  # observe Radiobuttonevents
  observeEvent(input_re()[["moduleSettings-settings_minmax"]], {
    PCRBiasCorrection::writeLog_(paste0("Settings: minmax = ", input_re()[["moduleSettings-settings_minmax"]]), logfilename)
    rv$minmax <- input_re()[["moduleSettings-settings_minmax"]]
  })

  # observe Radiobuttonevents
  observeEvent(input_re()[["moduleSettings-settings_selection_method"]], {
    PCRBiasCorrection::writeLog_(paste0("Settings: selection_method = ", input_re()[["moduleSettings-settings_selection_method"]]), logfilename)
    rv$selection_method <- input_re()[["moduleSettings-settings_selection_method"]]
  })
}


#' @title moduleSettingsUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleSettingsUI
moduleSettingsUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      # type of data box
      box(
        title = "Settings",
        checkboxInput(ns("settings_minmax"),
                      label = HTML("Use 'min-max'-correction (default: off) <b>[CAUTION: this feature is very experimental and neither tested nor validated!]</b>"),
                      value = FALSE),
        tags$hr(),
        radioButtons(ns("settings_selection_method"),
                     label = "Method to automatically (pre-) select the regression method for correction",
                     choices = list("SSE" = "SSE", "Relative Error" = "RelError"),
                     selected = "SSE"),
        width = 9
      ),
      box(
        title = "Description",
        width = 3
      )
    )
  )
}