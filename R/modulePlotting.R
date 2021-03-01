# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
# Copyright (C) 2019-2021 Lorenz Kapsner
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


#' @title module_plotting_server
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
#'   module_plotting_server,
#'   "modulePlotting",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_plotting_server
module_plotting_server <- function(input,
                                   output,
                                   session,
                                   rv,
                                   input_re,
                                   ...) {

  arguments <- list(...)

  observe({
    # this is needed, to open new tab (Regression plots)
    # before rendering the plots!
    # if (input_re()$tabs == "panel_3") {

    req(rv$run)

    # type 1 data:
    if (rv$type_locus_sample == "1") {
      if (isFALSE(rv$plotting_finished)) {
        withProgress(
          expr  = {
            regression_results <- rBiasCorrection::regression_utility(
              data = rv$fileimport_calibration,
              samplelocusname = rv$sample_locus_name,
              rv = rv,
              logfilename = arguments$logfilename,
              minmax = rv$minmax,
              seed = rv$seed
            )

            plotlist_reg <- regression_results[["plot_list"]]
            rv$result_list <- regression_results[["result_list"]]
          },
          value = 1 / 2,
          message = "Calculating calibration curves"
        )

        withProgress(
          expr  = {
            rBiasCorrection::plotting_utility(
              data = rv$fileimport_calibration,
              plotlist_reg = plotlist_reg,
              type = 1,
              samplelocusname = rv$sample_locus_name,
              rv = rv,
              plotdir = arguments$plotdir,
              logfilename = arguments$logfilename,
              minmax = rv$minmax,
              plot_height = rv$plot_height,
              plot_width = rv$plot_width,
              plot_textsize = rv$plot_textsize
            )

            # save regression statistics to reactive value
            rv$reg_stats <- rBiasCorrection::statistics_list(
              resultlist = rv$result_list,
              minmax = rv$minmax)

            # on finished
            rv$plotting_finished <- TRUE
            rBiasCorrection::write_log(message = "Finished plotting",
                                       logfilename = arguments$logfilename)
          },
          value = 1 / 2,
          message = "Plotting calibration curves"
        )

      }

      # else if type 2 data
    } else if (rv$type_locus_sample == "2") {
      if (isFALSE(rv$plotting_finished)) {
        a <- 1
        rv$result_list_type2 <- list()

        for (b in names(rv$fileimport_calibration)) {

          withProgress(
            expr  = {
              rv$vec_cal <- names(rv$fileimport_calibration[[a]])[-1]
              #% print(paste("Length rv$vec_cal:", length(rv$vec_cal)))

              regression_results <- rBiasCorrection::regression_utility(
                data = rv$fileimport_calibration[[a]],
                samplelocusname = rv$sample_locus_name,
                locus_id = gsub("[[:punct:]]", "", b),
                rv = rv,
                logfilename = arguments$logfilename,
                minmax = rv$minmax,
                seed = rv$seed
              )

              plotlist_reg <- regression_results[["plot_list"]]
              rv$result_list <- regression_results[["result_list"]]
            },
            value = 1 / 2,
            message = "Calculating calibration curves",
            detail = b
          )

          withProgress(
            expr  = {
              rBiasCorrection::plotting_utility(
                data = rv$fileimport_calibration[[a]],
                plotlist_reg = plotlist_reg,
                type = 2,
                samplelocusname = rv$sample_locus_name,
                locus_id = gsub("[[:punct:]]", "", b),
                rv = rv,
                plotdir = arguments$plotdir,
                logfilename = arguments$logfilename,
                minmax = rv$minmax,
                plot_height = rv$plot_height,
                plot_width = rv$plot_width,
                plot_textsize = rv$plot_textsize
              )

              # save regression statistics to reactive value
              rv$reg_stats[[b]] <- rBiasCorrection::statistics_list(
                resultlist = rv$result_list,
                minmax = rv$minmax)
              rv$result_list_type2[[b]] <- rv$result_list
              a <- a + 1
            },
            value = 1 / 2,
            message = "Plotting calibration curves",
            detail = b
          )
        }
        # on finished
        rv$plotting_finished <- TRUE
        rBiasCorrection::write_log(
          message = "Finished plotting",
          logfilename = arguments$logfilename)
      }
    }
  })

  # when plotting has finished
  observe({
    req(rv$plotting_finished)

    # type 1 data:
    if (rv$type_locus_sample == "1") {

      ### Plot tab ###
      # create a list of plotnames to populate selectInput
      plot_output_list <- lapply(
        seq_len(length(rv$vec_cal)),
        function(g) {
          paste0(gsub("[[:punct:]]", "", rv$vec_cal[g]))
        })
      names(plot_output_list) <- rv$vec_cal

      # create reactive selectinput:
      sel_in2 <- reactive({
        selectInput(inputId = "selectPlot",
                    label = "Select CpG site",
                    multiple = FALSE,
                    selectize = FALSE,
                    choices = plot_output_list)
      })

      # create download button for each plot
      output$download_plots <- downloadHandler(
        filename = function() {
          paste0(rv$sample_locus_name,
                 "_",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$selectPlot),
                 ".png")
        },
        content = function(file) {
          file.copy(paste0(arguments$plotdir,
                           rv$sample_locus_name,
                           "_",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$selectPlot),
                           ".png"),
                    file)
        },
        contentType = "image/png"
      )

      # render head of page with selectInput and downloadbutton
      # TODO align selectinput and button aside of each other
      output$select_plotinput <- renderUI({
        s <- sel_in2()
        b <- div(class = "row",
                 style = "text-align: center",
                 downloadButton(
                   "modulePlotting-download_plots",
                   "Download Plot",
                   style = paste0(
                     "white-space: normal; ",
                     "text-align:center; ",
                     "padding: 9.5px 9.5px 9.5px 9.5px; ",
                     "margin: 6px 10px 6px 10px;")))
        do.call(tagList, list(s,
                              tags$hr(),
                              b,
                              tags$hr()))
      })

      # for debugging
      observeEvent(
        eventExpr = input_re()$selectPlot,
        handlerExpr = {
          message(input_re()$selectPlot)
        })

      # render plots from local temporary file
      output$plots <- renderImage(
        expr = {
          filename <- paste0(arguments$plotdir,
                             rv$sample_locus_name,
                             "_",
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$selectPlot),
                             ".png")
          # Return a list containing the filename
          list(src = filename)
        },
        deleteFile = FALSE
      )

      # type 2 data:
    } else if (rv$type_locus_sample == "2") {

      # create list of loci to populate selectInput
      # "select_plotlocus"
      list_plot_locus <- list()
      for (i in seq_len(length(rv$fileimport_calibration))) {
        list_plot_locus[[i]] <- names(rv$fileimport_calibration)[i]
      }

      select_plotlocus <- reactive({
        selectInput(inputId = "select_plotlocus",
                    label = "Select locus:",
                    multiple = FALSE,
                    selectize = FALSE,
                    choices = list_plot_locus)
      })


      # create list of cpg-sites for each locus to populate selectInput
      # "select_plot_cpg"
      list_plot_cpg <- list()
      for (i in seq_len(length(rv$fileimport_calibration))) {
        list_plot_cpg[[names(rv$fileimport_calibration)[i]]] <- names(
          rv$fileimport_calibration[[i]]
        )[-1]
      }

      # only return list of CpG-sites for each locus, if there is
      # already a selection of the locus in select_plotlocus
      cpg_output <- reactive({
        if (!is.null(input_re()$select_plotlocus)) {
          return(list_plot_cpg[input_re()$select_plotlocus])
        }
      })

      # always wrap selectInput into reactive-function
      select_plot_cpg <- reactive({
        selectInput(inputId = "selectPlotType2",
                    label = "Select CpG site:",
                    multiple = FALSE,
                    selectize = FALSE,
                    choices = cpg_output())
      })

      # render second selectInput
      output$s2_plotoutput <- renderUI({
        select_plot_cpg()
      })

      # create download button for each plot
      output$download_plots <- downloadHandler(
        filename = function() {
          paste0(gsub("[[:punct:]]",
                      "",
                      input_re()$select_plotlocus),
                 "-",
                 rv$sample_locus_name,
                 "_",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$selectPlotType2),
                 ".png")
        },
        content = function(file) {
          file.copy(paste0(arguments$plotdir,
                           gsub("[[:punct:]]",
                                "",
                                input_re()$select_plotlocus),
                           "-",
                           rv$sample_locus_name,
                           "_",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$selectPlotType2),
                           ".png"),
                    file)
        },
        contentType = "image/png"
      )

      # render Plot UI
      output$select_plotinput <- renderUI({
        s1 <- select_plotlocus()
        s2 <- uiOutput("modulePlotting-s2_plotoutput")
        b <- div(class = "row",
                 style = "text-align: center",
                 downloadButton(
                   "modulePlotting-download_plots",
                   "Download Plot",
                   style = paste0(
                     "white-space: normal; ",
                     "text-align:center; ",
                     "padding: 9.5px 9.5px 9.5px 9.5px; ",
                     "margin: 6px 10px 6px 10px;")))
        do.call(tagList, list(s1,
                              s2,
                              tags$hr(),
                              b,
                              tags$hr()))
      })

      # render plot from local temporary file
      output$plots <- renderImage(
        expr = {
          filename <- paste0(arguments$plotdir,
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$select_plotlocus),
                             "-",
                             rv$sample_locus_name,
                             "_",
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$selectPlotType2),
                             ".png")
          #% print(filename)
          # Return a list containing the filename
          list(src = filename)
        },
        deleteFile = FALSE
      )
    }
  })
}


#' @title module_plotting_ui
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
#'     tabName = "plotting",
#'     module_plotting_ui(
#'       "modulePlotting"
#'     )
#'   )
#' )
#' }
#'
#' @export
#'
# module_plotting_ui
module_plotting_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        9,
        box(
          title = "Regression Plot",
          imageOutput(ns("plots")),
          tags$head(
            tags$style(
              type = "text/css",
              paste0(
                "#modulePlotting-plots img ",
                "{max-height: 100%; max-width: 100%; width: auto; ",
                "display: block; margin-left: auto; margin-right: auto;}")
            )
          ),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Plot Selection",
          uiOutput(ns("select_plotinput")),
          width = 12
        )
      )
    )
  )
}
