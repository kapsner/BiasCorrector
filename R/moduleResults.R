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


#' @title module_results_server
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
#'   module_results_server,
#'   "moduleResults",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_results_server
module_results_server <- function(input,
                                  output,
                                  session,
                                  rv,
                                  input_re,
                                  ...) {

  arguments <- list(...)

  observe({
    req(rv$calculate_results)

    if (rv$calculate_results) {
      message("\nCalculate results\n")

      if (rv$type_locus_sample == "1") {
        rv$choices_list <- tryCatch(
          expr = {
            o <- data.table::data.table(
              "Name" = character(),
              "better_model" = numeric()
            )
            for (l in seq_len(length(rv$vec_cal))) {
              radioname <- paste0("radio", l)
              o <- rbind(o,
                         cbind(
                           "Name" = rv$vec_cal[l],
                           "better_model" = as.numeric(
                             eval(
                               parse(
                                 text = paste0(
                                   "input_re()[[\"moduleModelSelection-",
                                   radioname, "\"]]"))
                             )
                           )
                         )
              )
            }
            o
          },
          error = function(e) {
            e
            o <- rv$better_model_stats[, c("Name",
                                           "better_model"
            ), with = FALSE]
            o
          },
          finally = function(f) {
            return(o)
          }
        )
        message(rv$choices_list)

        # calculating final results
        withProgress(
          message = "BiasCorrecting experimental data",
          value = 0, {
            incProgress(
              1 / 1,
              detail = "... working on BiasCorrection ...")

            # Experimental data
            solved_eq <- rBiasCorrection::solving_equations(
              rv$fileimport_experimental,
              rv$choices_list,
              type = 1,
              rv = rv,
              logfilename = arguments$logfilename,
              minmax = rv$minmax
            )
            rv$final_results <- solved_eq[["results"]]
            rv$substitutions <- solved_eq[["substitutions"]]
          })
      } else if (rv$type_locus_sample == "2") {

        # initialize temp results
        rv$temp_results <- list()

        rv$substitutions <- rBiasCorrection::substitutions_create()

        # iterate over unique names in locus_id of experimental file
        # (to correctly display decreasing order of CpG-sites in final
        # results)
        # calculating final results
        withProgress(
          message = "BiasCorrecting experimental data",
          value = 0, {
            incProgress(
              1 / 1,
              detail = "... working on BiasCorrection ...")

            # Experimental data
            # iterate over unique locus ids in experimental file
            for (locus in rv$fileimport_experimental[, unique(
              get("locus_id")
            )]) {
              # get regression results
              rv$result_list <- rv$result_list_type2[[locus]]
              # get copy of experimental data for that specific locus
              expdata <- rv$fileimport_experimental[get("locus_id") == locus, ]
              # get colnames of that specific locus (different loci can have
              # different numbers of CpG-sites)
              vec <- c("locus_id", colnames(expdata)[2:(expdata[, min(
                get("CpG_count")
              )] + 1)], "row_means")
              # solve equations for that locus and append temp_results
              solved_eq <- rBiasCorrection::solving_equations(
                expdata[, vec, with = FALSE],
                rv$reg_stats[[locus]][
                  ,
                  c("Name", "better_model"),
                  with = FALSE
                ],
                type = 2,
                rv = rv,
                logfilename = arguments$logfilename,
                minmax = rv$minmax
              )
              rv$temp_results[[locus]] <- solved_eq[["results"]]
              rv$substitutions <- rbind(rv$substitutions,
                                        solved_eq[["substitutions"]])
            }

            # iterate over temp_results (key = locus-name) and iteratively
            # append final results
            for (i in names(rv$temp_results)) {
              if (is.null(rv$final_results)) {
                rv$final_results <- rv$temp_results[[i]]
              } else {
                # set use.names = T and fill = T because, as pointed out
                # before, different loci can have different numbers of CpG
                # sites and!!
                # the best fitting algorithm can be cubic or hyperbolic for
                # the same CpG site-number of different loci
                rv$final_results <- rbind(
                  rv$final_results,
                  rv$temp_results[[i]],
                  use.names = TRUE,
                  fill = TRUE
                )
              }
            }
            vec <- colnames(rv$final_results)[grepl("row_means",
                                                    colnames(
                                                      rv$final_results
                                                    )
            )]
            # reorder the columns so that the rownames are at the end!
            rv$final_results <- cbind(rv$final_results[, -vec, with = FALSE],
                                      rv$final_results[, vec, with = FALSE],
                                      CpG_sites = unique(
                                        rv$fileimport_experimental[, get(
                                          "CpG_count"
                                        ), by = get("locus_id")])$CpG_count
            )
          })
      }

      output$dtfinal <- DT::renderDataTable({

        # https://stackoverflow.com/questions/49636423/how-to-change-the-
        # cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
        DT::datatable(rv$final_results,
                      options = list(scrollX = TRUE,
                                     pageLength = 20,
                                     dom = "ltip",
                                     rowCallback = DT::JS(rv$row_callback)),
                      rownames = FALSE) %>%
          DT::formatRound(columns = c(2:ncol(rv$final_results)),
                          digits = 3)
      })

      # show corrected results for experimental data
      output$corrected_data <- renderUI({
        dt <- DT::dataTableOutput("moduleResults-dtfinal")

        do.call(tagList, list(dt))
      })

      # Download corrected results
      output$download_final <- downloadHandler(
        filename = function() {
          paste0(rv$sample_locus_name,
                 "_corrected_values_",
                 rBiasCorrection::get_timestamp(), ".csv")
        },
        content = function(file) {
          rBiasCorrection::write_csv(
            table = rv$final_results,
            filename = file)
        },
        contentType = "text/csv"
      )


      output$download_all_data <- downloadHandler(
        filename = paste0(
          rv$sample_locus_name,
          "_all-results_",
          gsub("\\-",
               "",
               substr(Sys.time(), 1, 10)),
          "_",
          gsub("\\:",
               "",
               substr(Sys.time(), 12, 16)),
          ".zip"
        ),
        content = function(fname) {
          message(paste0("getwd(): ", getwd()))

          # temporarily set tempdir as wd
          oldwd <- getwd()
          # fix for CRAN-submission!!
          on.exit(setwd(oldwd))

          setwd(tempdir())
          message(paste0("getwd(): ", getwd()))

          # create files where is no difference in export
          # between type 1 and 2
          rBiasCorrection::write_csv(
            rv$fileimport_experimental,
            paste0(
              arguments$csvdir,
              "raw_experimental_data.csv")
          )
          rBiasCorrection::write_csv(
            rv$aggregated_experimental,
            paste0(
              arguments$csvdir,
              "aggregated_experimental_data.csv")
          )
          rBiasCorrection::write_csv(
            rv$final_results,
            paste0(
              arguments$csvdir,
              rv$sample_locus_name,
              "_corrected_values.csv")
          )
          rBiasCorrection::write_csv(
            rv$substitutions,
            paste0(arguments$csvdir,
                   rv$sample_locus_name,
                   "_substituted_values.csv")
          )
          rBiasCorrection::write_csv(
            rv$substitutions_corrected_h,
            paste0(
              arguments$csvdir,
              rv$sample_locus_name,
              "_substituted_corrected_h.csv")
          )
          rBiasCorrection::write_csv(
            rv$substitutions_corrected_c,
            paste0(
              arguments$csvdir,
              rv$sample_locus_name,
              "_substituted_corrected_c.csv")
          )
          write(rv$logfile,
                paste0(
                  arguments$csvdir,
                  "BC_logfile.txt")
          )

          # create other files
          if (rv$type_locus_sample == "1") {
            rBiasCorrection::write_csv(
              rv$fileimport_calibration,
              paste0(
                arguments$csvdir,
                "raw_calibration_data.csv")
            )
            rBiasCorrection::write_csv(
              rv$aggregated_calibration,
              paste0(
                arguments$csvdir,
                "aggregated_calibration_data.csv")
            )
            rBiasCorrection::write_csv(
              rv$reg_stats,
              paste0(arguments$csvdir,
                     rv$sample_locus_name,
                     "_regression_stats.csv")
            )
            rBiasCorrection::write_csv(
              rv$reg_stats_corrected_h,
              paste0(
                arguments$csvdir,
                rv$sample_locus_name,
                "_corrected_regression_stats_h.csv")
            )
            rBiasCorrection::write_csv(
              rv$reg_stats_corrected_c,
              paste0(
                arguments$csvdir,
                rv$sample_locus_name,
                "_corrected_regression_stats_c.csv")
            )

          } else if (rv$type_locus_sample == "2") {
            # regression stats
            for (key in names(rv$fileimport_calibration)) {
              rBiasCorrection::write_csv(
                rv$reg_stats[[key]],
                paste0(
                  arguments$csvdir,
                  rv$sample_locus_name,
                  "_regression_stats_",
                  gsub("[[:punct:]]",
                       "",
                       key),
                  ".csv")
              )
            }

            for (key in names(rv$fileimport_cal_corrected)) {
              rBiasCorrection::write_csv(
                rv$reg_stats_corrected[[key]],
                paste0(
                  arguments$csvdir,
                  "BC_regression_stats_corrected_",
                  gsub("[[:punct:]]",
                       "",
                       key),
                  ".csv")
              )
            }

            # raw calibrations data
            for (key in names(rv$fileimport_calibration)) {
              rBiasCorrection::write_csv(
                rv$fileimport_calibration[[key]],
                paste0(
                  arguments$csvdir,
                  "raw_calibration_data_",
                  gsub("[[:punct:]]",
                       "",
                       key),
                  ".csv")
              )
            }
          }

          utils::zip(
            zipfile = fname,
            files = c(
              paste0("csv/",
                     list.files(arguments$csvdir)),
              paste0("plots/",
                     list.files(arguments$plotdir))
            ))

          if (file.exists(paste0(tempdir(), "/", fname, ".zip"))) {
            file.rename(paste0(tempdir(), "/", fname, ".zip"), fname)
          }

          # return to old wd
          setwd(oldwd)
          message(paste0("getwd(): ", getwd()))
        },
        contentType = "application/zip"
      )

      # present substitutions in extra tab (only if there were some)
      if (nrow(rv$substitutions) > 0) {
        rv$substitutions_calc <- TRUE
        # workaround to tell ui, that experimental file is there
        output$got_substitutions <- reactive({
          return(TRUE)
        })
        outputOptions(output,
                      "got_substitutions",
                      suspendWhenHidden = FALSE)
      }

      output$description <- renderText({
        str1 <- paste0("The results table shows the ",
                       "BiasCorrected experimental data.")
        str2 <- paste0("Column 1 shows the sample ID (type 1 data) ",
                       "or the locus ID (type 2 data).")
        str3 <- paste0("All other columns represent the BiasCorrected ",
                       "experimental data for the CpG sites and the ",
                       "row-means of all CpG sites respectively.")
        str4 <- paste0("The suffixes '_h' and '_c' in the column names ",
                       "indicate the regression algorithm used for ",
                       "BiasCorrection of the respective CpG site ",
                       "('_h': hyperbolic regression; '_c': cubic ",
                       "polynomial regression).")
        HTML(
          paste(
            str1,
            str2,
            str3,
            str4,
            sep = "<br/><br/>"
          )
        )
      })

      rv$calculate_results <- FALSE
    }
  })

  # Presentation of substituted values
  observe({
    req(rv$substitutions_calc)

    output$description_sub <- renderText({
      str1 <- paste0("Substitutions occur, when no result is found in ",
                     "the range of plausible values between 0 and 100 ",
                     "during the BiasCorrection.")
      str2 <- paste0("A 'border zone' is implemented in the ranges ",
                     "0 - 10% and 100 + 10%.")
      str3 <- paste0("If a result is in the range -10 < x < 0 ",
                     "percentage or 100  < x < 110 percentage, the ",
                     "value is substituted in the final results with ",
                     "0% or 100% respectively.")
      str4 <- paste0("Values beyond these border zones will be ",
                     "substituted with a blank value in the final ",
                     "output, as they seem implausible and could ",
                     "indicate substantial errors in the underlying ",
                     "data.")
      str5 <- paste0("For a detailed feedback, the substitutions ",
                     "table shows the results of the algorithm ",
                     "'BiasCorrected value' and the corresponding ",
                     "substitution 'Substituted value' for the ",
                     "respective CpG site.")

      HTML(
        paste(
          str1,
          str2,
          str3,
          str4,
          str5,
          sep = "<br/><br/>"
        )
      )
    })

    # this workaround is related to this issue:
    # TODO issue: https://github.com/rstudio/shiny/issues/2116
    output$substituted_out <- renderUI({
      t <- DT::dataTableOutput("moduleResults-substituted_values")
      do.call(tagList, list(t))
    })
    # change colnames for better display
    colnames(rv$substitutions) <- c("Sample ID",
                                    "CpG site",
                                    "BiasCorrected value",
                                    "Substituted value",
                                    "Regression")


    output$download_substituted <- downloadHandler(
      filename = function() {
        paste0(rv$sample_locus_name,
               "_substituted_values_",
               rBiasCorrection::get_timestamp(),
               ".csv"
        )
      },
      content = function(file) {
        rBiasCorrection::write_csv(
          table = rv$substitutions,
          filename = file)
      },
      contentType = "text/csv"
    )

    output$substituted_values <- DT::renderDataTable({
      DT::datatable(rv$substitutions,
                    options = list(scrollX = TRUE,
                                   pageLength = 20,
                                   dom = "ltip"),
                    rownames = FALSE) %>%
        DT::formatRound(columns = c(3:4), digits = 3)
    })

    # msg2 <- "Please refer to the tab 'Substituted values'
    # for further information."
    msg2 <- paste0("Please scroll down to the section ",
                   "'Substituted values' for further information.")
    if (nrow(rv$substitutions) == 1) {
      msg1 <- "Substituted 1 value. "
    } else {
      msg1 <- paste0("Substituted ",
                     nrow(rv$substitutions),
                     " values.")
    }

    # show modal here
    showModal(modalDialog(
      paste(msg1, msg2),
      title = "Substituted values"
    ))
  })
}



#' @title module_results_ui
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
#'     tabName = "results",
#'     module_results_ui(
#'       "moduleResults"
#'     )
#'   )
#' )
#' }
#'
#' @export
#'
# module_results_ui
module_results_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        9,
        box(
          title = "BiasCorrected Results",
          uiOutput(ns("corrected_data")),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Download BiasCorrected Results",
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                "moduleResults-download_final",
                "Download corrected values",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;"))
          ),
          tags$hr(),
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                "moduleResults-download_all_data",
                "Download zip archive (tables and plots)",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;"))
          ),
          tags$hr(),
          width = 12
        ),
        box(
          title = "Description",
          htmlOutput(ns("description")),
          width = 12
        )
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = "output['moduleResults-got_substitutions']",
        column(
          9,
          box(
            title = "Substituted values",
            uiOutput(ns("substituted_out")),
            width = 12
          )
        ),
        column(
          3,
          box(
            title = "Download Substitutions",
            div(class = "row",
                style = "text-align: center",
                downloadButton(
                  "moduleResults-download_substituted",
                  "Download substituted values",
                  style = paste0(
                    "white-space: normal; ",
                    "text-align:center; ",
                    "padding: 9.5px 9.5px 9.5px 9.5px; ",
                    "margin: 6px 10px 6px 10px;"))
            ),
            tags$hr(),
            width = 12
          ),
          box(
            title = "What are 'substitutions'?",
            htmlOutput(ns("description_sub")),
            width = 12
          )
        )
      )
    )
  )
}
