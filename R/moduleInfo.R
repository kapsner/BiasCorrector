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


#' @title module_info_server
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
# module_info_server
module_info_server <- function(input,
                               output,
                               session,
                               rv,
                               input_re) {
  
  output$citation_correction <- renderPrint({
    utils::citation("rBiasCorrection")
  })
  output$citation_corrector <- renderPrint({
    utils::citation("BiasCorrector")
  })
}


#' @title module_info_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_info_ui
module_info_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Info",
        tabsetPanel(
          tabPanel(
            title = "Citation",
            h5(paste0("If you use these 'BiasCorrector' or ",
                      "'rBiasCorrection' packages to correct ",
                      "DNA methylation data for a publication, ",
                      "please cite them as follows:")),
            h5(tags$b("rBiasCorrection:")),
            verbatimTextOutput(ns("citation_correction")),
            h5(tags$b("BiasCorrector:")),
            verbatimTextOutput(ns("citation_corrector"))
          )
        ),
        width = 12
      )
    )
  )
}
