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


#' @title moduleCorrectedPlotsServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleCorrectedPlotsServer
moduleCorrectedPlotsServer <- function(input, output, session, rv, input_re){

  # correct all hyperbolic
  observe({
    req(rv$plotting_finished)
    if (is.null(rv$fileimportCal_corrected_h)){
      if (rv$type_locus_sample == "1"){

        withProgress(message = "BiasCorrecting calibration data", value = 0, {
          incProgress(1/1, detail = "... using hyperbolic regression parameters ...")
          # hyperbolic correction
          rv$choices_list <- rv$regStats[,c("Name"), with=F][,("better_model"):=0]

          # correct calibration data (to show corrected calibration curves)
          solved_eq_h <- PCRBiasCorrection::solvingEquations_(rv$fileimportCal, rv$choices_list, type = 1, rv = rv, mode = "corrected", logfilename = logfilename, minmax = rv$minmax)
          rv$fileimportCal_corrected_h <- solved_eq_h[["results"]]
          colnames(rv$fileimportCal_corrected_h) <- colnames(rv$fileimportCal)
          
          rv$substitutions_corrected_h <- solved_eq_h[["substitutions"]]
          colnames(rv$substitutions_corrected_h) <- c("Sample ID", "CpG site", "BiasCorrected value", "Substituted value", "Regression")
        })
      } else if (rv$type_locus_sample == "2"){
        cat("fileimportCal_corrected_h: Not implemented yet.\n")

        # TODO
        # # Calibration Data (to show corrected calibration curves)
        # # initialize calibration results list
        # rv$fileimportCal_corrected <- list()
        # # iterate over fileimportCal (in type 2 data, this is a list with one calibrationdata data.table for each locus)
        # for (a in names(rv$fileimportCal)){
        #   # get unique elements of true_methylation for one specific locus (we are treating them here as if they were sample ids)
        #   for (b in rv$fileimportCal[[a]][,unique(true_methylation)]){
        #     # get the regression parameters of that locus (locusname is saved in "a")
        #     rv$result_list <- rv$result_list_type2[[a]]
        #     # get subset of the calibration data of that methylation step
        #     caldata <- rv$fileimportCal[[a]][true_methylation==b,]
        #     nc <- ncol(caldata)
        #     vec <- c("true_methylation", colnames(caldata)[2:(nc-1)], "row_means")
        #     # solve equation for that calibrationstep
        #     # save result of each calibrationstep in tmp object
        #     tmp <- PCRBiasCorrection::solvingEquations_(caldata[,vec,with=F], rv$regStats[[a]][,.(Name, better_model)], type = 2, rv = rv, mode = "corrected", logfilename = logfilename)[["results"]]
        #     # imediatelly rename columnames
        #     colnames(tmp) <- vec
        #     # if new calibration step is saved for the first time
        #     if (is.null(rv$fileimportCal_corrected[[a]])){
        #       rv$fileimportCal_corrected[[a]] <- tmp
        #     } else {
        #       # we should not need fill, since there should be no differences in colnames for one file
        #       rv$fileimportCal_corrected[[a]] <- rbind(rv$fileimportCal_corrected[[a]], tmp, use.names=T, fill=F)
        #     }
        #   }
        # }
      }
    }
  })

  # correct all cubic
  observe({
    req(rv$plotting_finished)
    if (is.null(rv$fileimportCal_corrected_c)){
      if (rv$type_locus_sample == "1"){

        withProgress(message = "BiasCorrecting calibration data", value = 0, {
          incProgress(1/1, detail = "... using cubic regression parameters ...")
          # cubic correction
          rv$choices_list <- rv$regStats[,c("Name"), with=F][,("better_model"):=1]

          # correct calibration data (to show corrected calibration curves)
          solved_eq_c <- PCRBiasCorrection::solvingEquations_(rv$fileimportCal, rv$choices_list, type = 1, rv = rv, mode = "corrected", logfilename = logfilename, minmax = rv$minmax)
          rv$fileimportCal_corrected_c <- solved_eq_c[["results"]]
          colnames(rv$fileimportCal_corrected_c) <- colnames(rv$fileimportCal)
          
          rv$substitutions_corrected_c <- solved_eq_c[["substitutions"]]
          colnames(rv$substitutions_corrected_c) <- c("Sample ID", "CpG site", "BiasCorrected value", "Substituted value", "Regression")
        })
      } else if (rv$type_locus_sample == "2"){
        cat("fileimportCal_corrected_c: Not implemented yet.\n")
      }

    }
  })

  observeEvent({
    # this is needed, to start plotting, when we have the bias corrected calibration values!
    if (!is.null(rv$fileimportCal_corrected_h) & !is.null(rv$fileimportCal_corrected_c)) TRUE
    else return()}, {

      # type 1 data:
      if (rv$type_locus_sample == "1"){
        if (isFALSE(rv$corrected_finished)){

          # plot hyperbolic
          withProgress(message = "Plotting BiasCorrected calibration plot", value = 0, {
            incProgress(1/1, detail = "... working hard on hyperbolic correction ...")

            # calculate new calibration curves from corrected calibration data
            regression_results <- PCRBiasCorrection::regressionUtility_(rv$fileimportCal_corrected_h, samplelocusname=rv$sampleLocusName, rv=rv, mode="corrected", logfilename = logfilename, minmax = rv$minmax)
            plotlistR <- regression_results[["plot_list"]]
            rv$result_list_hyperbolic <- regression_results[["result_list"]]

            PCRBiasCorrection::plottingUtility_(rv$fileimportCal_corrected_h, plotlistR, type=1, samplelocusname=rv$sampleLocusName, locus_id = NULL, rv=rv, mode="corrected_h", plotdir = plotdir, logfilename = logfilename, minmax = rv$minmax)

            # save regression statistics to reactive value
            rv$regStats_corrected_h <- PCRBiasCorrection::statisticsList_(rv$result_list_hyperbolic, minmax = rv$minmax)

            for (i in rv$choices_list[,get("Name")]){
              rv$regStats_corrected_h[get("Name")==i,("better_model"):=rv$choices_list[get("Name")==i,as.integer(as.character(get("better_model")))]]
            }
            PCRBiasCorrection::createBarErrorPlots_(rv$regStats, rv$regStats_corrected_h, rv, type=1, plotdir = plotdir, logfilename = logfilename, mode = "corrected_h")
          })

          # plot cubic
          withProgress(message = "Plotting BiasCorrected calibration plot", value = 0, {
            incProgress(1/1, detail = "... working hard on cubic correction ...")

            # calculate new calibration curves from corrected calibration data
            regression_results <- PCRBiasCorrection::regressionUtility_(rv$fileimportCal_corrected_c, samplelocusname=rv$sampleLocusName, rv=rv, mode="corrected", logfilename = logfilename, minmax = rv$minmax)
            plotlistR <- regression_results[["plot_list"]]
            rv$result_list_cubic<- regression_results[["result_list"]]

            PCRBiasCorrection::plottingUtility_(rv$fileimportCal_corrected_c, plotlistR, type=1, samplelocusname=rv$sampleLocusName, locus_id = NULL, rv=rv, mode="corrected_c", plotdir = plotdir, logfilename = logfilename, minmax = rv$minmax)

            # save regression statistics to reactive value
            rv$regStats_corrected_c <- PCRBiasCorrection::statisticsList_(rv$result_list_cubic, minmax = rv$minmax)

            for (i in rv$choices_list[,get("Name")]){
              rv$regStats_corrected_c[get("Name")==i,("better_model"):=rv$choices_list[get("Name")==i,as.integer(as.character(get("better_model")))]]
            }
            PCRBiasCorrection::createBarErrorPlots_(rv$regStats, rv$regStats_corrected_c, rv, type=1, plotdir = plotdir, logfilename = logfilename, mode = "corrected_c")
          })

          # when finished
          rv$corrected_finished <- TRUE
          PCRBiasCorrection::writeLog_("Finished plotting corrected", logfilename = logfilename)
        }

        # else if type 2 data
      } else if (rv$type_locus_sample == "2"){

        if (isFALSE(rv$corrected_finished)){
          a <- 1
          rv$result_list_type2_corrected <- list()

          withProgress(message = "Plotting BiasCorrected results", value = 0, {
            incProgress(1/1, detail = "... working hard ...")

            for (locus in names(rv$fileimportCal_corrected)){
              rv$vec_cal <- names(rv$fileimportCal_corrected[[a]])[-1]
              #print(paste("Length rv$vec_cal:", length(rv$vec_cal)))

              regression_results <- PCRBiasCorrection::regressionUtility_(rv$fileimportCal_corrected[[a]], samplelocusname=rv$sampleLocusName, locus_id = gsub("[[:punct:]]", "", locus), rv=rv, mode="corrected", logfilename = logfilename, minmax = rv$minmax)
              plotlistR <- regression_results[["plot_list"]]
              rv$result_list <- regression_results[["result_list"]]

              PCRBiasCorrection::plottingUtility_(rv$fileimportCal_corrected[[a]], plotlistR, type=2, samplelocusname=rv$sampleLocusName, locus_id=gsub("[[:punct:]]", "", locus), rv=rv, mode="corrected", plotdir = plotdir, logfilename = logfilename, minmax = rv$minmax)

              # save regression statistics to reactive value
              rv$regStats_corrected[[locus]] <- PCRBiasCorrection::statisticsList_(rv$result_list, minmax = rv$minmax)
              rv$result_list_type2_corrected[[locus]] <- rv$result_list

              # create barplots
              PCRBiasCorrection::createBarErrorPlots_(rv$regStats[[locus]], rv$regStats_corrected[[locus]], rv, type=2, locus_id=locus, plotdir=plotdir, logfilename = logfilename)

              a <- a + 1
            }
          })
          # on finished
          rv$corrected_finished <- TRUE
          PCRBiasCorrection::writeLog_("Finished plotting corrected", logfilename = logfilename)
        }
      }
    })

  # when plotting has finished
  observe({
    req(rv$corrected_finished)

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
        selectInput(inputId="selectPlot_corrected", label = "Select CpG-site:", multiple = F, selectize = F, choices = plot_output_list)
      })

      # create download button for each plot
      output$downloadPlots_corrected_h <- downloadHandler(
        filename = function(){paste0(rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_h.png")},
        content = function(file){
          file.copy(paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_h.png"), file)
        },
        contentType = "image/png"
      )

      output$downloadPlotsSSE_corrected_h <- downloadHandler(
        filename = function(){paste0("Errorplot_", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_h.png")},
        content = function(file){
          file.copy(paste0(plotdir, "Errorplot_", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_h.png"), file)
        },
        contentType = "image/png"
      )

      output$downloadPlots_corrected_c <- downloadHandler(
        filename = function(){paste0(rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_c.png")},
        content = function(file){
          file.copy(paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_c.png"), file)
        },
        contentType = "image/png"
      )

      output$downloadPlotsSSE_corrected_c <- downloadHandler(
        filename = function(){paste0("Errorplot_", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_c.png")},
        content = function(file){
          file.copy(paste0(plotdir, "Errorplot_", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_c.png"), file)
        },
        contentType = "image/png"
      )

      # render head of page with selectInput and downloadbutton
      output$selectPlotInput_corrected <- renderUI({
        s <- selIn2()
        do.call(tagList, list(s, tags$hr()))
      })

      # for debugging
      observeEvent(input_re()$selectPlot_corrected, {
        print(input_re()$selectPlot_corrected)
      })

      # render plots from local temporary file
      output$plots_corrected_h <- renderImage({
        #width  <- session$clientData[["output_moduleCorrectedPlots-plots_corrected_width"]]

        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_h.png")
        # Return a list containing the filename
        # list(src = filename,
        #      width = width)
        list(src = filename)
      }, deleteFile = FALSE)

      output$plots_corrected_c <- renderImage({
        #width  <- session$clientData[["output_moduleCorrectedPlots-plots_corrected_width"]]

        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_c.png")
        # Return a list containing the filename
        # list(src = filename,
        #      width = width)
        list(src = filename)
      }, deleteFile = FALSE)

      # render plots from local temporary file
      output$plotsSSE_corrected_h <- renderImage({
        #width  <- session$clientData[["output_moduleCorrectedPlots-plotsSSE_corrected_width"]]

        filename <- paste0(plotdir, "Errorplot_", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_h.png")
        # Return a list containing the filename
        # list(src = filename,
        #      width = width)
        list(src = filename)
      }, deleteFile = FALSE)

      output$plotsSSE_corrected_c <- renderImage({
        #width  <- session$clientData[["output_moduleCorrectedPlots-plotsSSE_corrected_width"]]

        filename <- paste0(plotdir, "Errorplot_", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected_c.png")
        # Return a list containing the filename
        # list(src = filename,
        #      width = width)
        list(src = filename)
      }, deleteFile = FALSE)


      # type 2 data:
    } else if (rv$type_locus_sample == "2"){

      #create list of loci to populate selectInput "selectPlotLocus"
      list_plot_locus <- list()
      for (i in 1:length(rv$fileimportCal_corrected)){
        list_plot_locus[[i]] <- names(rv$fileimportCal_corrected)[i]
      }

      selectPlotLocus <- reactive({
        selectInput(inputId="selectPlotLocus_corrected", label = "Select locus:", multiple = F, selectize = F, choices = list_plot_locus)
      })


      # create list of cpg-sites for each locus to populate selectInput "selectPlotCpG"
      list_plot_cpg <- list()
      for (i in 1:length(rv$fileimportCal_corrected)){
        list_plot_cpg[[names(rv$fileimportCal_corrected)[i]]] <- names(rv$fileimportCal_corrected[[i]])[-1]
      }

      # only return list of CpG-sites for each locus, if there is already a selection of the locus in selectPlotLocus
      cpg_output <- reactive({
        if (!is.null(input_re()$selectPlotLocus_corrected)){
          return(list_plot_cpg[input_re()$selectPlotLocus_corrected])
        }
      })

      # always wrap selectInput into reactive-function
      selectPlotCpG <- reactive({
        selectInput(inputId="selectPlotType2_corrected", label = "Select CpG-site:", multiple = F, selectize = F, choices = cpg_output())
      })

      # render second selectInput
      output$s2PlotOutput_corrected <- renderUI({
        s3 <- selectPlotCpG()
        s3
      })

      # create download button for each plot
      output$downloadPlots_corrected <- downloadHandler(
        filename = function(){paste0(gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected.png")},
        content = function(file){
          file.copy(paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected.png"), file)
        },
        contentType = "image/png"
      )

      output$downloadPlotsSSE_corrected_h <- downloadHandler(
        filename = function(){paste0("Errorplot_", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected_h.png")},
        content = function(file){
          file.copy(paste0(plotdir, "Errorplot_", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected_h.png"), file)
        },
        contentType = "image/png"
      )

      # render Plot UI
      output$selectPlotInput_corrected <- renderUI({
        s1 <- selectPlotLocus()
        s2 <- uiOutput("moduleCorrectedPlots-s2PlotOutput_corrected")
        b1 <-  div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlots_corrected_h", "Download Corrected Plot (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
        c1 <-  div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlotsSSE_corrected_h", "Download Error Plot (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
        b2 <-  div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlots_corrected_c", "Download Corrected Plot (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
        c2 <-  div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlotsSSE_corrected_c", "Download Error Plot (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
        do.call(tagList, list(s1, s2, tags$hr(), b1, b2, tags$hr(), c1, c2, tags$hr()))
      })

      # render plot from local temporary file
      output$plots_corrected <- renderImage({
        #width  <- session$clientData[["output_moduleCorrectedPlots-plots_corrected_width"]]

        filename <- paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected.png")
        # print(filename)
        # Return a list containing the filename
        # list(src = filename,
        #      width = width)
        list(src = filename)
      }, deleteFile = FALSE)

      # render plots from local temporary file
      output$plotsSSE_corrected <- renderImage({
        #width  <- session$clientData[["output_moduleCorrectedPlots-plotsSSE_corrected_width"]]

        filename <- paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_errorplot.png")
        # Return a list containing the filename
        # list(src = filename,
        #      width = width)
        list(src = filename)
      }, deleteFile = FALSE)

    }
  })
}

#' @title moduleCorrectedPlotsUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleCorrectedPlotsUI
moduleCorrectedPlotsUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(9,
             box(title = "BiasCorrected Regression Plots",
                 column(6,
                        h5(tags$b("Calibration data corrected with hyperbolic regression:")),
                        imageOutput(ns("plots_corrected_h")),
                        tags$head(tags$style(type="text/css", "#moduleCorrectedPlots-plots_corrected_h img {max-height: 100%; max-width: 100%; width: auto}")),
                        div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlots_corrected_h", "Download Corrected Plot (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
                 ),
                 column(6,
                        h5(tags$b("Calibration data corrected with cubic regression:")),
                        imageOutput(ns("plots_corrected_c")),
                        tags$head(tags$style(type="text/css", "#moduleCorrectedPlots-plots_corrected_c img {max-height: 100%; max-width: 100%; width: auto}")),
                        div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlots_corrected_c", "Download Corrected Plot (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
                 ),
                 width=12
             ),
             box(title = "Efficiency of BiasCorrection",
                 column(6,
                        imageOutput(ns("plotsSSE_corrected_h")),
                        tags$head(tags$style(type="text/css", "#moduleCorrectedPlots-plotsSSE_corrected_h img {max-height: 100%; max-width: 100%; width: auto}")),
                        div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlotsSSE_corrected_h", "Download Error Plot (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
                 ),
                 column(6,
                        imageOutput(ns("plotsSSE_corrected_c")),
                        tags$head(tags$style(type="text/css", "#moduleCorrectedPlots-plotsSSE_corrected_c img {max-height: 100%; max-width: 100%; width: auto}")),
                        div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlotsSSE_corrected_c", "Download Error Plot (hyperbolic correction)", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))
                 ),
                 width=12
             )

      ),
      column(3,
             box(title = "Plot Selection",
                 uiOutput(ns("selectPlotInput_corrected")),
                 width = 12
             )
      )
    )
  )
}
