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


#' @title modulePlottingServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# modulePlottingServer
modulePlottingServer <- function(input, output, session, rv, input_re){
  observe({
    # this is needed, to open new tab (Regression plots) before rendering the plots!
    if (input_re()$tabs == "panel_3"){

      # type 1 data:
      if (rv$type_locus_sample == "1"){
        if (isFALSE(rv$plotting_finished)){

          regression_results <- PCRBiasCorrection::regressionUtility_(rv$fileimportCal, samplelocusname=rv$sampleLocusName, rv=rv, logfilename = logfilename, minmax = rv$minmax)
          plotlistR <- regression_results[["plot_list"]]
          rv$result_list <- regression_results[["result_list"]]
          PCRBiasCorrection::plottingUtility_(rv$fileimportCal, plotlistR, type=1, samplelocusname=rv$sampleLocusName, rv=rv, plotdir = plotdir, logfilename = logfilename, minmax = rv$minmax)

          # save regression statistics to reactive value
          rv$regStats <- PCRBiasCorrection::statisticsList_(rv$result_list, minmax = rv$minmax)

          # on finished
          rv$plotting_finished <- TRUE
          PCRBiasCorrection::writeLog_("Finished plotting", logfilename = logfilename)

        }

        # else if type 2 data
      } else if (rv$type_locus_sample == "2"){

        if (isFALSE(rv$plotting_finished)){
          a <- 1
          rv$result_list_type2 <- list()

          for (b in names(rv$fileimportCal)){
            rv$vec_cal <- names(rv$fileimportCal[[a]])[-1]
            #print(paste("Length rv$vec_cal:", length(rv$vec_cal)))

            regression_results <- PCRBiasCorrection::regressionUtility_(rv$fileimportCal[[a]], samplelocusname=rv$sampleLocusName, locus_id = gsub("[[:punct:]]", "", b), rv=rv, logfilename = logfilename, minmax = rv$minmax)
            plotlistR <- regression_results[["plot_list"]]
            rv$result_list <- regression_results[["result_list"]]
            PCRBiasCorrection::plottingUtility_(rv$fileimportCal[[a]], plotlistR, type=2, samplelocusname=rv$sampleLocusName, locus_id = gsub("[[:punct:]]", "", b), rv=rv, plotdir = plotdir, logfilename = logfilename, minmax = rv$minmax)

            # save regression statistics to reactive value
            rv$regStats[[b]] <- PCRBiasCorrection::statisticsList_(rv$result_list, minmax = rv$minmax)
            rv$result_list_type2[[b]] <- rv$result_list
            a <- a + 1
          }
          # on finished
          rv$plotting_finished <- TRUE
          PCRBiasCorrection::writeLog_("Finished plotting", logfilename = logfilename)
        }
      }
    }
  })

  # when plotting has finished
  observe({
    req(rv$plotting_finished)

    # type 1 data:
    if (rv$type_locus_sample == "1"){

      ### Plot tab ###
      # create a list of plotnames to populate selectInput
      plot_output_list <- lapply(1:length(rv$vec_cal), function(g) {
        paste0(gsub("[[:punct:]]", "", rv$vec_cal[g]))
      })
      names(plot_output_list) <- rv$vec_cal

      # create reactive selectinput:
      selIn2 <- reactive({
        selectInput(inputId="selectPlot", label = "Select CpG-site", multiple = F, selectize = F, choices = plot_output_list)
      })

      # create download button for each plot
      output$downloadPlots <- downloadHandler(
        filename = function(){paste0(rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot), ".png")},
        content = function(file){
          file.copy(paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot), ".png"), file)
        },
        contentType = "image/png"
      )

      # render head of page with selectInput and downloadbutton
      # TODO align selectinput and button aside of each other
      output$selectPlotInput <- renderUI({
        s <- selIn2()
        b <- div(class="row", style="text-align: center", downloadButton("modulePlotting-downloadPlots", "Download Plot", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
        do.call(tagList, list(s, tags$hr(), b, tags$hr()))
      })

      # for debugging
      observeEvent(input_re()$selectPlot, {
        print(input_re()$selectPlot)
      })

      # render plots from local temporary file
      output$plots <- renderImage({
        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot), ".png")
        # Return a list containing the filename
        list(src = filename)
      }, deleteFile = FALSE)

      # type 2 data:
    } else if (rv$type_locus_sample == "2"){

      #create list of loci to populate selectInput "selectPlotLocus"
          list_plot_locus <- list()
          for (i in 1:length(rv$fileimportCal)){
            list_plot_locus[[i]] <- names(rv$fileimportCal)[i]
          }

          selectPlotLocus <- reactive({
            selectInput(inputId="selectPlotLocus", label = "Select locus:", multiple = F, selectize = F, choices = list_plot_locus)
          })


          # create list of cpg-sites for each locus to populate selectInput "selectPlotCpG"
          list_plot_cpg <- list()
          for (i in 1:length(rv$fileimportCal)){
            list_plot_cpg[[names(rv$fileimportCal)[i]]] <- names(rv$fileimportCal[[i]])[-1]
          }

          # only return list of CpG-sites for each locus, if there is already a selection of the locus in selectPlotLocus
          cpg_output <- reactive({
            if (!is.null(input_re()$selectPlotLocus)){
              return(list_plot_cpg[input_re()$selectPlotLocus])
            }
          })

          # always wrap selectInput into reactive-function
          selectPlotCpG <- reactive({
            selectInput(inputId="selectPlotType2", label = "Select CpG-site:", multiple = F, selectize = F, choices = cpg_output())
          })

          # render second selectInput
          output$s2PlotOutput <- renderUI({
            selectPlotCpG()
          })

          # create download button for each plot
          output$downloadPlots <- downloadHandler(
            filename = function(){paste0(gsub("[[:punct:]]", "", input_re()$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2), ".png")},
            content = function(file){
              file.copy(paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2), ".png"), file)
            },
            contentType = "image/png"
          )

          # render Plot UI
          output$selectPlotInput <- renderUI({
            s1 <- selectPlotLocus()
            s2 <- uiOutput("modulePlotting-s2PlotOutput")
            b <- div(class="row", style="text-align: center", downloadButton("modulePlotting-downloadPlots", "Download Plot", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
            do.call(tagList, list(s1, s2, tags$hr(), b, tags$hr()))
          })

          # render plot from local temporary file
          output$plots <- renderImage({
            filename <- paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2), ".png")
            # print(filename)
            # Return a list containing the filename
            list(src = filename)
          }, deleteFile = FALSE)
    }
  })
}


#' @title modulePlottingUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# modulePlottingUI
modulePlottingUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(9,
             box(
               title = "Regression Plot",
               imageOutput(ns("plots")),
               tags$head(tags$style(type="text/css", "#modulePlotting-plots img {height: auto; max-width: 100%; width: auto}")),
               width=12
             )
      ),
      column(3,
             box(
               title = "Plot Selection",
               uiOutput(ns("selectPlotInput")),
               width = 12)
      )
    )
  )
}
