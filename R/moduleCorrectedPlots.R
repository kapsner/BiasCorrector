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


#' @title module_correctedplots_server
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
#'   module_correctedplots_server,
#'   "moduleCorrectedPlots",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_correctedplots_server
module_correctedplots_server <- function(input,
                                         output,
                                         session,
                                         rv,
                                         input_re,
                                         ...) {
  arguments <- list(...)
  # correct all hyperbolic
  observe({
    req(rv$plotting_finished)
    if (is.null(rv$fileimport_cal_corrected_h)) {
      if (rv$type_locus_sample == "1") {
        withProgress(message = "BiasCorrecting calibration data", value = 0, {
          incProgress(
            1 / 2,
            detail = "... using hyperbolic regression parameters ...")
          # hyperbolic correction
          rv$choices_list <- rv$reg_stats[, c("Name"), with = FALSE
          ][
            , ("better_model") := 0
          ]
          # correct calibration data (to show corrected calibration curves)
          solved_eq_h <- rBiasCorrection::solving_equations(
            datatable = rv$fileimport_calibration,
            regmethod = rv$choices_list,
            type = 1,
            rv = rv,
            mode = "corrected",
            logfilename = arguments$logfilename,
            minmax = rv$minmax
          )
          rv$fileimport_cal_corrected_h <- solved_eq_h[["results"]]
          colnames(rv$fileimport_cal_corrected_h) <- colnames(
            rv$fileimport_calibration
          )
          rv$substitutions_corrected_h <- solved_eq_h[["substitutions"]]
          colnames(rv$substitutions_corrected_h) <- c(
            "Sample ID",
            "CpG site",
            "BiasCorrected value",
            "Substituted value",
            "Regression"
          )
        })
      } else if (rv$type_locus_sample == "2") {
        message("fileimport_cal_corrected_h: Not implemented yet.\n")
        # TODO
        # Calibration Data (to show corrected calibration curves)
        # initialize calibration results list
        #% rv$fileimport_cal_corrected <- list()
        #% # iterate over fileimport_cal (in type 2 data, this is a list
        #% # with one calibrationdata data.table for each locus)
        #% for (a in names(rv$fileimport_cal)) {
        #%   # get unique elements of true_methylation for one specific locus
        #%   # (we are treating them here as if they were sample ids)
        #%   for (b in rv$fileimport_cal[[a]][,unique(true_methylation)]) {
        #%     # get the regression parameters of that locus (locusname is
        #%     # saved in "a")
        #%     rv$result_list <- rv$result_list_type2[[a]]
        #%     # get subset of the calibration data of that methylation step
        #%     caldata <- rv$fileimport_cal[[a]][true_methylation==b,]
        #%     nc <- ncol(caldata)
        #%     vec <- c("true_methylation",
        #%              colnames(caldata)[2:(nc-1)], "row_means")
        #%     # solve equation for that calibrationstep
        #%     # save result of each calibrationstep in tmp object
        #%     tmp <- rBiasCorrection::solving_equations(
        #%       datatable = caldata[,vec,with=F],
        #%       regmethod = rv$reg_stats[[a]][,.(Name, better_model)],
        #%       type = 2,
        #%       rv = rv,
        #%       mode = "corrected",
        #%       logfilename = arguments$logfilename)[["results"]]
        #%     # imediatelly rename columnames
        #%     colnames(tmp) <- vec
        #%     # if new calibration step is saved for the first time
        #%     if (is.null(rv$fileimport_cal_corrected[[a]])) {
        #%       rv$fileimport_cal_corrected[[a]] <- tmp
        #%     } else {
        #%       # we should not need fill, since there should be no
        #%       # differences in colnames for one file
        #%       rv$fileimport_cal_corrected[[a]] <- rbind(
        #%         rv$fileimport_cal_corrected[[a]],
        #%         tmp,
        #%         use.names=T, fill=F)
        #%     }
        #%   }
        #% }
      }
    }
  })

  # correct all cubic
  observe({
    req(rv$plotting_finished)
    if (is.null(rv$fileimport_cal_corrected_c)) {
      if (rv$type_locus_sample == "1") {
        withProgress(message = "BiasCorrecting calibration data", value = 0, {
          incProgress(
            1 / 2,
            detail = "... using cubic regression parameters ...")
          # cubic correction
          rv$choices_list <- rv$reg_stats[, c("Name"), with = FALSE
          ][
            , ("better_model") := 1
          ]
          # correct calibration data (to show corrected calibration curves)
          solved_eq_c <- rBiasCorrection::solving_equations(
            datatable = rv$fileimport_calibration,
            regmethod = rv$choices_list,
            type = 1,
            rv = rv,
            mode = "corrected",
            logfilename = arguments$logfilename,
            minmax = rv$minmax
          )
          rv$fileimport_cal_corrected_c <- solved_eq_c[["results"]]
          colnames(rv$fileimport_cal_corrected_c) <- colnames(
            rv$fileimport_calibration
          )
          rv$substitutions_corrected_c <- solved_eq_c[["substitutions"]]
          colnames(rv$substitutions_corrected_c) <- c(
            "Sample ID",
            "CpG site",
            "BiasCorrected value",
            "Substituted value",
            "Regression"
          )
        })
      } else if (rv$type_locus_sample == "2") {
        message("fileimport_cal_corrected_c: Not implemented yet.\n")
      }
    }
  })

  observeEvent(
    eventExpr = {
      # this is needed, to start plotting, when we have the bias
      # corrected calibration values!
      req(!is.null(rv$fileimport_cal_corrected_h) &
          !is.null(rv$fileimport_cal_corrected_c))
    },
    handlerExpr = {
      # type 1 data:
      if (rv$type_locus_sample == "1") {
        if (isFALSE(rv$corrected_finished)) {
          # plot hyperbolic
          withProgress(
            message = "Plotting BiasCorrected calibration plots", value = 0, {
              incProgress(
                1 / 2,
                detail = "... working hard on hyperbolic correction ...")
              # calculate new calibration curves from corrected calibration data
              regression_results <- rBiasCorrection::regression_utility(
                data = rv$fileimport_cal_corrected_h,
                samplelocusname = rv$sample_locus_name,
                rv = rv,
                mode = "corrected",
                logfilename = arguments$logfilename,
                minmax = rv$minmax,
                seed = rv$seed
              )

              plotlist_reg <- regression_results[["plot_list"]]
              rv$result_list_hyperbolic <- regression_results[["result_list"]]

              rBiasCorrection::plotting_utility(
                data = rv$fileimport_cal_corrected_h,
                plotlist_reg = plotlist_reg,
                type = 1,
                samplelocusname = rv$sample_locus_name,
                locus_id = NULL,
                rv = rv,
                mode = "corrected_h",
                plotdir = arguments$plotdir,
                logfilename = arguments$logfilename,
                minmax = rv$minmax,
                plot_height = rv$plot_height,
                plot_width = rv$plot_width,
                plot_textsize = rv$plot_textsize
              )

              # save regression statistics to reactive value
              rv$reg_stats_corrected_h <- rBiasCorrection::statistics_list(
                resultlist = rv$result_list_hyperbolic,
                minmax = rv$minmax
              )

              for (i in rv$choices_list[, get("Name")]) {
                rv$reg_stats_corrected_h[
                  get("Name") == i, ("better_model") := rv$choices_list[
                    get("Name") == i, as.integer(
                      as.character(get("better_model"))
                    )]
                ]
              }

              rBiasCorrection::createbarerrorplots(
                statstable_pre = rv$reg_stats,
                statstable_post = rv$reg_stats_corrected_h,
                rv = rv,
                type = 1,
                plotdir = arguments$plotdir,
                logfilename = arguments$logfilename,
                mode = "corrected_h",
                plot_height = rv$plot_height,
                plot_width = rv$plot_width,
                plot_textsize = rv$plot_textsize
              )
            })

          # plot cubic
          withProgress(
            message = "Plotting BiasCorrected calibration plots",
            value = 0, {
              incProgress(
                1 / 2,
                detail = "... working hard on cubic correction ...")
              # calculate new calibration curves from corrected calibration data
              regression_results <- rBiasCorrection::regression_utility(
                data = rv$fileimport_cal_corrected_c,
                samplelocusname = rv$sample_locus_name,
                rv = rv,
                mode = "corrected",
                logfilename = arguments$logfilename,
                minmax = rv$minmax,
                seed = rv$seed
              )

              plotlist_reg <- regression_results[["plot_list"]]
              rv$result_list_cubic <- regression_results[["result_list"]]

              rBiasCorrection::plotting_utility(
                data = rv$fileimport_cal_corrected_c,
                plotlist_reg = plotlist_reg,
                type = 1,
                samplelocusname = rv$sample_locus_name,
                locus_id = NULL,
                rv = rv,
                mode = "corrected_c",
                plotdir = arguments$plotdir,
                logfilename = arguments$logfilename,
                minmax = rv$minmax,
                plot_height = rv$plot_height,
                plot_width = rv$plot_width,
                plot_textsize = rv$plot_textsize
              )

              # save regression statistics to reactive value
              rv$reg_stats_corrected_c <- rBiasCorrection::statistics_list(
                resultlist = rv$result_list_cubic,
                minmax = rv$minmax
              )

              for (i in rv$choices_list[, get("Name")]) {
                rv$reg_stats_corrected_c[
                  get("Name") == i, ("better_model") := rv$choices_list[
                    get("Name") == i, as.integer(
                      as.character(get("better_model"))
                    )]
                ]
              }

              rBiasCorrection::createbarerrorplots(
                statstable_pre = rv$reg_stats,
                statstable_post = rv$reg_stats_corrected_c,
                rv = rv,
                type = 1,
                plotdir = arguments$plotdir,
                logfilename = arguments$logfilename,
                mode = "corrected_c",
                plot_height = rv$plot_height,
                plot_width = rv$plot_width,
                plot_textsize = rv$plot_textsize
              )
            })

          # when finished
          rv$corrected_finished <- TRUE
          rBiasCorrection::write_log(
            message = "Finished plotting corrected",
            logfilename = arguments$logfilename)
        }
        # else if type 2 data
      } else if (rv$type_locus_sample == "2") {
        if (isFALSE(rv$corrected_finished)) {
          a <- 1
          rv$result_list_type2_corrected <- list()
          withProgress(
            message = "Plotting BiasCorrected results",
            value = 0, {
              incProgress(
                1 / 2,
                detail = "... working hard ...")
              for (locus in names(rv$fileimport_cal_corrected)) {
                rv$vec_cal <- names(rv$fileimport_cal_corrected[[a]])[-1]
                #% print(paste("Length rv$vec_cal:", length(rv$vec_cal)))
                regression_results <- rBiasCorrection::regression_utility(
                  data = rv$fileimport_cal_corrected[[a]],
                  samplelocusname = rv$sample_locus_name,
                  locus_id = gsub("[[:punct:]]", "", locus),
                  rv = rv,
                  mode = "corrected",
                  logfilename = arguments$logfilename,
                  minmax = rv$minmax,
                  seed = rv$seed
                )

                plotlist_reg <- regression_results[["plot_list"]]
                rv$result_list <- regression_results[["result_list"]]

                rBiasCorrection::plotting_utility(
                  data = rv$fileimport_cal_corrected[[a]],
                  plotlist_reg = plotlist_reg,
                  type = 2,
                  samplelocusname = rv$sample_locus_name,
                  locus_id = gsub("[[:punct:]]", "", locus),
                  rv = rv,
                  mode = "corrected",
                  plotdir = arguments$plotdir,
                  logfilename = arguments$logfilename,
                  minmax = rv$minmax,
                  plot_height = rv$plot_height,
                  plot_width = rv$plot_width,
                  plot_textsize = rv$plot_textsize
                )

                # save regression statistics to reactive value
                waround <- rBiasCorrection::statistics_list(
                  resultlist = rv$result_list,
                  minmax = rv$minmax
                )

                rv$reg_stats_corrected[[locus]] <- waround
                rv$result_list_type2_corrected[[locus]] <- rv$result_list

                # create barplots
                rBiasCorrection::createbarerrorplots(
                  statstable_pre = rv$reg_stats[[locus]],
                  statstable_post = rv$reg_stats_corrected[[locus]],
                  rv = rv,
                  type = 2,
                  locus_id = locus,
                  plotdir = arguments$plotdir,
                  logfilename = arguments$logfilename,
                  plot_height = rv$plot_height,
                  plot_width = rv$plot_width,
                  plot_textsize = rv$plot_textsize
                )

                a <- a + 1
              }
            })
          # on finished
          rv$corrected_finished <- TRUE
          rBiasCorrection::write_log(
            message = "Finished plotting corrected",
            logfilename = arguments$logfilename)
        }
      }
    }
  )

  # when plotting has finished
  observe({
    req(rv$corrected_finished)
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
        selectInput(inputId = "selectPlot_corrected",
                    label = "Select CpG site:",
                    multiple = FALSE,
                    selectize = FALSE,
                    choices = plot_output_list)
      })
      # create download button for each plot
      output$download_plots_corrected_h <- downloadHandler(
        filename = function() {
          paste0(rv$sample_locus_name,
                 "_",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$selectPlot_corrected),
                 "_corrected_h.png")
        },
        content = function(file) {
          file.copy(paste0(arguments$plotdir,
                           rv$sample_locus_name,
                           "_",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$selectPlot_corrected),
                           "_corrected_h.png"),
                    file)
        },
        contentType = "image/png"
      )
      output$download_plot_sse_corrected_h <- downloadHandler(
        filename = function() {
          paste0(rv$sample_locus_name,
                 "_error_",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$selectPlot_corrected),
                 "_corrected_h.png")
        },
        content = function(file) {
          file.copy(paste0(arguments$plotdir,
                           rv$sample_locus_name,
                           "_error_",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$selectPlot_corrected),
                           "_corrected_h.png"),
                    file)
        },
        contentType = "image/png"
      )
      output$download_plots_corrected_c <- downloadHandler(
        filename = function() {
          paste0(rv$sample_locus_name,
                 "_",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$selectPlot_corrected),
                 "_corrected_c.png")
        },
        content = function(file) {
          file.copy(paste0(arguments$plotdir,
                           rv$sample_locus_name,
                           "_",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$selectPlot_corrected),
                           "_corrected_c.png"),
                    file)
        },
        contentType = "image/png"
      )
      output$download_plot_sse_corrected_c <- downloadHandler(
        filename = function() {
          paste0(rv$sample_locus_name,
                 "_error_",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$selectPlot_corrected),
                 "_corrected_c.png")
        },
        content = function(file) {
          file.copy(paste0(arguments$plotdir,
                           rv$sample_locus_name,
                           "_error_",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$selectPlot_corrected),
                           "_corrected_c.png"),
                    file)
        },
        contentType = "image/png"
      )
      # render head of page with selectInput and downloadbutton
      output$select_plotinput_corrected <- renderUI({
        s <- sel_in2()
        do.call(tagList, list(s, tags$hr()))
      })
      # for debugging
      observeEvent(input_re()$selectPlot_corrected, {
        message(input_re()$selectPlot_corrected)
      })
      # render plots from local temporary file
      output$plots_corrected_h <- renderImage(
        expr = {
          #% width  <- session$clientData
          ##% [["output_moduleCorrectedPlots-plots_corrected_width"]]
          filename <- paste0(arguments$plotdir,
                             rv$sample_locus_name,
                             "_", gsub("[[:punct:]]",
                                       "",
                                       input_re()$selectPlot_corrected),
                             "_corrected_h.png")
          # Return a list containing the filename
          #% list(src = filename,
          #%      width = width)
          list(src = filename)
        },
        deleteFile = FALSE
      )
      output$plots_corrected_c <- renderImage(
        expr = {
          #% width  <- session$clientData
          ##% [["output_moduleCorrectedPlots-plots_corrected_width"]]
          filename <- paste0(arguments$plotdir,
                             rv$sample_locus_name,
                             "_", gsub("[[:punct:]]",
                                       "",
                                       input_re()$selectPlot_corrected),
                             "_corrected_c.png")
          # Return a list containing the filename
          #% list(src = filename,
          #%      width = width)
          list(src = filename)
        },
        deleteFile = FALSE
      )
      # render plots from local temporary file
      output$plots_sse_corrected_h <- renderImage(
        expr = {
          #% width  <- session$clientData
          ##% [["output_moduleCorrectedPlots-plots_sse_corrected_width"]]
          filename <- paste0(arguments$plotdir,
                             rv$sample_locus_name,
                             "_error_",
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$selectPlot_corrected),
                             "_corrected_h.png")
          # Return a list containing the filename
          #% list(src = filename,
          #%      width = width)
          list(src = filename)
        },
        deleteFile = FALSE
      )
      output$plots_sse_corrected_c <- renderImage(
        expr = {
          #% width  <- session$clientData
          ##% [["output_moduleCorrectedPlots-plots_sse_corrected_width"]]
          filename <- paste0(arguments$plotdir,
                             rv$sample_locus_name,
                             "_error_",
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$selectPlot_corrected),
                             "_corrected_c.png")
          # Return a list containing the filename
          #% list(src = filename,
          #%      width = width)
          list(src = filename)
        },
        deleteFile = FALSE
      )
      # type 2 data:
    } else if (rv$type_locus_sample == "2") {
      # create list of loci to populate selectInput "select_plotlocus"
      list_plot_locus <- list()
      for (i in seq_len(length(rv$fileimport_cal_corrected))) {
        list_plot_locus[[i]] <- names(rv$fileimport_cal_corrected)[i]
      }
      select_plotlocus <- reactive({
        selectInput(inputId = "select_plotlocus_corrected",
                    label = "Select locus:",
                    multiple = FALSE,
                    selectize = FALSE,
                    choices = list_plot_locus
        )
      })
      # create list of cpg-sites for each locus to populate selectInput
      # "select_plot_cpg"
      list_plot_cpg <- list()
      for (i in seq_len(length(rv$fileimport_cal_corrected))) {
        list_plot_cpg[[names(rv$fileimport_cal_corrected)[i]]] <- names(
          rv$fileimport_cal_corrected[[i]])[-1]
      }
      # only return list of CpG-sites for each locus, if there is already
      # a selection of the locus in select_plotlocus
      cpg_output <- reactive({
        if (!is.null(input_re()$select_plotlocus_corrected)) {
          return(list_plot_cpg[input_re()$select_plotlocus_corrected])
        }
      })
      # always wrap selectInput into reactive-function
      select_plot_cpg <- reactive({
        selectInput(inputId = "select_plot_type2_corrected",
                    label = "Select CpG site:",
                    multiple = FALSE,
                    selectize = FALSE,
                    choices = cpg_output())
      })
      # render second selectInput
      output$s2_plotoutput_corrected <- renderUI({
        s3 <- select_plot_cpg()
        s3
      })
      # create download button for each plot
      output$download_plots_corrected <- downloadHandler(
        filename = function() {
          paste0(gsub("[[:punct:]]",
                      "",
                      input_re()$select_plotlocus_corrected),
                 "-",
                 rv$sample_locus_name,
                 "_",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$select_plot_type2_corrected),
                 "_corrected.png")
        },
        content = function(file) {
          file.copy(paste0(arguments$plotdir,
                           gsub("[[:punct:]]",
                                "",
                                input_re()$select_plotlocus_corrected),
                           "-",
                           rv$sample_locus_name,
                           "_",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$select_plot_type2_corrected),
                           "_corrected.png"),
                    file)
        },
        contentType = "image/png"
      )
      output$download_plotssse_corrected_h <- downloadHandler(
        filename = function() {
          paste0("Errorplot_",
                 rv$sample_locus_name,
                 "_",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$select_plotlocus_corrected),
                 "-",
                 gsub("[[:punct:]]",
                      "",
                      input_re()$select_plot_type2_corrected),
                 "_corrected_h.png")
        },
        content = function(file) {
          file.copy(paste0(arguments$plotdir,
                           "Errorplot_",
                           rv$sample_locus_name,
                           "_",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$select_plotlocus_corrected),
                           "-",
                           gsub("[[:punct:]]",
                                "",
                                input_re()$select_plot_type2_corrected),
                           "_corrected_h.png"),
                    file)
        },
        contentType = "image/png"
      )
      # render Plot UI
      output$select_plotinput_corrected <- renderUI({
        s1 <- select_plotlocus()
        s2 <- uiOutput("moduleCorrectedPlots-s2_plotoutput_corrected")
        b1 <- div(class = "row",
                  style = "text-align: center",
                  downloadButton(
                    "moduleCorrectedPlots-download_plots_corrected_h",
                    "Download Corrected Plot (hyperbolic correction)",
                    style = paste0(
                      "white-space: normal; ",
                      "text-align:center; ",
                      "padding: 9.5px 9.5px 9.5px 9.5px; ",
                      "margin: 6px 10px 6px 10px;")
                  ))
        c1 <- div(class = "row",
                  style = "text-align: center",
                  downloadButton(
                    "moduleCorrectedPlots-download_plotssse_corrected_h",
                    "Download Error Plot (hyperbolic correction)",
                    style = paste0(
                      "white-space: normal; ",
                      "text-align:center; ",
                      "padding: 9.5px 9.5px 9.5px 9.5px; ",
                      "margin: 6px 10px 6px 10px;")))
        b2 <- div(class = "row",
                  style = "text-align: center",
                  downloadButton(
                    "moduleCorrectedPlots-download_plots_corrected_c",
                    "Download Corrected Plot (hyperbolic correction)",
                    style = paste0(
                      "white-space: normal; ",
                      "text-align:center; ",
                      "padding: 9.5px 9.5px 9.5px 9.5px; ",
                      "margin: 6px 10px 6px 10px;")))
        c2 <- div(class = "row",
                  style = "text-align: center",
                  downloadButton(
                    "moduleCorrectedPlots-download_plotssse_corrected_c",
                    "Download Error Plot (hyperbolic correction)",
                    style = paste0(
                      "white-space: normal; ",
                      "text-align:center; ",
                      "padding: 9.5px 9.5px 9.5px 9.5px; ",
                      "margin: 6px 10px 6px 10px;")))
        do.call(tagList, list(s1,
                              s2,
                              tags$hr(),
                              b1,
                              b2,
                              tags$hr(),
                              c1,
                              c2,
                              tags$hr())
        )
      })
      # render plot from local temporary file
      output$plots_corrected <- renderImage(
        expr = {
          #% width  <- session$clientData
          ##% [["output_moduleCorrectedPlots-plots_corrected_width"]]
          filename <- paste0(arguments$plotdir,
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$select_plotlocus_corrected),
                             "-",
                             rv$sample_locus_name,
                             "_",
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$select_plot_type2_corrected),
                             "_corrected.png")
          #% print(filename)
          # Return a list containing the filename
          #% list(src = filename,
          #%      width = width)
          list(src = filename)
        },
        deleteFile = FALSE
      )
      # render plots from local temporary file
      output$plots_sse_corrected <- renderImage(
        expr = {
          #% width  <- session$clientData
          ##% [["output_moduleCorrectedPlots-plots_sse_corrected_width"]]
          filename <- paste0(arguments$plotdir,
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$select_plotlocus_corrected),
                             "-",
                             rv$sample_locus_name,
                             "_",
                             gsub("[[:punct:]]",
                                  "",
                                  input_re()$select_plot_type2_corrected),
                             "_errorplot.png")
          # Return a list containing the filename
          # list(src = filename,
          #      width = width)
          list(src = filename)
        },
        deleteFile = FALSE
      )
    }
  })
}

#' @title module_correctedplots_ui
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
#'     tabName = "correctedplots",
#'     module_correctedplots_ui(
#'       "moduleCorrectedPlots"
#'     )
#'   )
#' )
#' }
#'
#' @export
#'
# module_correctedplots_ui
module_correctedplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        9,
        box(
          title = "BiasCorrected Regression Plots",
          fluidRow(
            column(
              6,
              div(class = "row",
                  style = "text-align: center",
                  h5(tags$b(
                    "Calibration data corrected with hyperbolic regression:")
                  )),
              imageOutput(ns("plots_corrected_h")),
              tags$head(
                tags$style(
                  type = "text/css",
                  paste0(
                    "#moduleCorrectedPlots-plots_corrected_h img ",
                    "{max-height: 100%; max-width: 100%; width: auto; ",
                    "display: block; margin-left: auto; margin-right: auto;}")
                )
              )
            ),
            column(
              6,
              div(class = "row",
                  style = "text-align: center",
                  h5(tags$b(
                    paste0("Calibration data corrected with cubic ",
                           "polynomial regression:")))
              ),
              imageOutput(ns("plots_corrected_c")),
              tags$head(
                tags$style(
                  type = "text/css",
                  paste0(
                    "#moduleCorrectedPlots-plots_corrected_c img ",
                    "{max-height: 100%; max-width: 100%; width: auto; ",
                    "display: block; margin-left: auto; margin-right: auto;}")
                )
              )
            )
          ),
          fluidRow(
            column(
              6,
              div(class = "row",
                  style = "text-align: center",
                  downloadButton(
                    "moduleCorrectedPlots-download_plots_corrected_h",
                    "Download Corrected Plot (hyperbolic correction)",
                    style = paste0(
                      "white-space: normal; ",
                      "text-align:center; ",
                      "padding: 9.5px 9.5px 9.5px 9.5px; ",
                      "margin: 6px 10px 6px 10px;")))
            ),
            column(
              6,
              div(class = "row",
                  style = "text-align: center",
                  downloadButton(
                    "moduleCorrectedPlots-download_plots_corrected_c",
                    "Download Corrected Plot (cubic correction)",
                    style = paste0(
                      "white-space: normal; ",
                      "text-align:center; ",
                      "padding: 9.5px 9.5px 9.5px 9.5px; ",
                      "margin: 6px 10px 6px 10px;")))
            )
          ),
          width = 12
        ),
        box(
          title = "Efficiency of BiasCorrection",
          fluidRow(
            column(
              6,
              div(class = "row",
                  style = "text-align: center",
                  h5(tags$b(
                    paste0("Theoretical efficiency of BiasCorrection ",
                           "with hyperbolic regression:")))
              ),
              imageOutput(ns("plots_sse_corrected_h")),
              tags$head(
                tags$style(
                  type = "text/css",
                  paste0(
                    "#moduleCorrectedPlots-plots_sse_corrected_h img ",
                    "{max-height: 100%; max-width: 100%; width: auto; ",
                    "display: block; margin-left: auto; margin-right: auto;}")
                )
              )
            ),
            column(
              6,
              div(class = "row",
                  style = "text-align: center",
                  h5(tags$b(
                    paste0("Theoretical efficiency of BiasCorrection ",
                           "with cubic polynomial regression:")))
              ),
              imageOutput(ns("plots_sse_corrected_c")),
              tags$head(
                tags$style(
                  type = "text/css",
                  paste0(
                    "#moduleCorrectedPlots-plots_sse_corrected_c img ",
                    "{max-height: 100%; max-width: 100%; width: auto; ",
                    "display: block; margin-left: auto; margin-right: auto;}")
                )
              )
            )
          ),
          fluidRow(
            column(
              6,
              div(class = "row",
                  style = "text-align: center",
                  downloadButton(
                    "moduleCorrectedPlots-download_plot_sse_corrected_h",
                    "Download Error Plot (hyperbolic correction)",
                    style = paste0(
                      "white-space: normal; ",
                      "text-align:center; ",
                      "padding: 9.5px 9.5px 9.5px 9.5px; ",
                      "margin: 6px 10px 6px 10px;")))
            ),
            column(
              6,
              div(class = "row",
                  style = "text-align: center",
                  downloadButton(
                    "moduleCorrectedPlots-download_plot_sse_corrected_c",
                    "Download Error Plot (cubic correction)",
                    style = paste0(
                      "white-space: normal; ",
                      "text-align:center; ",
                      "padding: 9.5px 9.5px 9.5px 9.5px; ",
                      "margin: 6px 10px 6px 10px;")))
            )
          ),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Plot Selection",
          uiOutput(ns("select_plotinput_corrected")),
          width = 12
        )
      )
    )
  )
}
