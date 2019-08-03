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


#' @title moduleModelSelectionServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleModelSelectionServer
moduleModelSelectionServer <- function(input, output, session, rv, input_re){

  observe({
    req(rv$better_model_stats)
    # model selection only implemented for type 1 data
    if (rv$type_locus_sample == "1"){
      # render radio buttons for tab 5
      output$reg_radios <- renderUI({
        radio_output_list <- lapply(1:length(rv$vec_cal), function(g) {
          radioname <- paste0("radio", g)
          div(class="row", style = "margin: 0.5%; text-align: center;",
              div(class="col-sm-4", style="text-align: left;",
                  h5(tags$b(paste0("Regression type for ", rv$vec_cal[g], ":")))),
              div(class="col-sm-4", style = "text-align: left;",
                  div(class = "row", style = "text-align: center;",
                      radioButtons(inputId = radioname,
                                   label = NULL,
                                   choices = list("hyperbolic" = 0, "cubic" = 1),
                                   selected = as.character(rv$better_model_stats[get("Name")==rv$vec_cal[g], get("better_model")]),
                                   inline = TRUE))
              ),
              div(class="col-sm-4",
                  verbatimTextOutput(paste0("moduleModelSelection-text_", radioname)))
              )
        })
        do.call(tagList, list(radio_output_list)) # needed to display properly.
      })

      output$biascorrection <- renderUI({
        do.call(tagList, list(div(class="row", style="text-align: center", actionButton("results", "BiasCorrect experimental data", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))))
      })

    } else if (rv$type_locus_sample == "2"){
      # type 2 data:
      # trigger claculation of results (bypass manual model selection)
      #shinyjs::click("results")
    }
  })


  observe({
    req(rv$better_model_stats)

      if (rv$type_locus_sample == "1"){

        lapply(1:length(rv$vec_cal), function(k) {
          radioname <- paste0("radio", k)

          if (!is.null(input_re()[[radioname]])){

            if (rv$selection_method == "SSE"){
              output[[paste0("text_", radioname)]] <- reactive({
                paste("SSE:",
                      as.character(ifelse(input_re()[[radioname]] == "1",
                                          rv$better_model_stats[get("Name")==rv$vec_cal[k], round(get("SSE_cubic"),3)],
                                          rv$better_model_stats[get("Name")==rv$vec_cal[k], round(get("SSE_hyperbolic"), 3)]))
                )
              })

            } else if (rv$selection_method == "RelError"){
              output[[paste0("text_", radioname)]] <- reactive({
                paste("Rel.Error:",
                      as.character(ifelse(input_re()[[radioname]] == "1",
                                          rv$better_model_stats[get("Name")==rv$vec_cal[k], round(get("relative_error_c"),3)],
                                          rv$better_model_stats[get("Name")==rv$vec_cal[k], round(get("relative_error_h"), 3)]))
                )
              })
            }
          }
        })
      }
  })
}


#' @title moduleModelSelectionUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleModelSelectionUI
moduleModelSelectionUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(9,
             box(title = "Select Regression Model",
                 uiOutput(ns("reg_radios")),
                 width=12
             )
      ),
      column(3,
             box(title = "BiasCorrect Experimental Data",
                 uiOutput(ns("biascorrection")),
                 tags$hr(),
                 width = 12)
      )
    )
  )
}
