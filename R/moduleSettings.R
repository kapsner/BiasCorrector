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


#' @title module_settings_server
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
#'   module_settings_server,
#'   "moduleSettings",
#'   rv = rv,
#'   logfilename = logfilename
#' )
#' }
#'
#' @export
#'
# module_settings_server
module_settings_server <- function(input,
                                   output,
                                   session,
                                   rv,
                                   input_re,
                                   ...) {

  arguments <- list(...)

  # observe Radiobuttonevents
  observeEvent(
    eventExpr = input_re()[["moduleSettings-settings_minmax"]],
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0(
          "Settings: minmax = ",
          input_re()[["moduleSettings-settings_minmax"]]),
        logfilename = arguments$logfilename
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
        logfilename = arguments$logfilename
      )
      waround12 <- input_re()[["moduleSettings-settings_selection_method"]]
      rv$selection_method <- waround12
    })

  # seed
  observeEvent(
    eventExpr = input_re()[["moduleSettings-settings_seed"]],
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0(
          "Settings: seed = ",
          input_re()[["moduleSettings-settings_seed"]]),
        logfilename = arguments$logfilename
      )
      rv$seed <- input_re()[["moduleSettings-settings_seed"]]
    }
  )

  # plot height
  observeEvent(
    eventExpr = input_re()[["moduleSettings-settings_plot_height"]],
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0(
          "Settings: plot_height = ",
          input_re()[["moduleSettings-settings_plot_height"]]),
        logfilename = arguments$logfilename
      )
      rv$plot_height <- input_re()[["moduleSettings-settings_plot_height"]]
    }
  )

  # plot width
  observeEvent(
    eventExpr = input_re()[["moduleSettings-settings_plot_width"]],
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0(
          "Settings: plot_width = ",
          input_re()[["moduleSettings-settings_plot_width"]]),
        logfilename = arguments$logfilename
      )
      rv$plot_width <- input_re()[["moduleSettings-settings_plot_width"]]
    }
  )

  # plot text size
  observeEvent(
    eventExpr = input_re()[["moduleSettings-settings_plot_textsize"]],
    handlerExpr = {
      rBiasCorrection::write_log(
        message = paste0(
          "Settings: plot_textsize = ",
          input_re()[["moduleSettings-settings_plot_textsize"]]),
        logfilename = arguments$logfilename
      )
      rv$plot_textsize <- input_re()[["moduleSettings-settings_plot_textsize"]]
    }
  )

  observe({

    req(rv$plot_textsize)

    # load exampledata 1
    gdat <- rBiasCorrection::example._plot.df_agg

    coef_h <- rBiasCorrection::example._plot_coef_h
    coef_c <- rBiasCorrection::example._plot_coef_c

    rBiasCorrection::create_exampleplot(
      data = gdat,
      coef_hyper = coef_h,
      coef_cubic = coef_c,
      plot_height = rv$plot_height,
      plot_width = rv$plot_width,
      plot_textsize = rv$plot_textsize,
      filename = paste0(
        arguments$tempdir,
        "/exampleplot.png"
      )
    )

    # render plots from local temporary file
    output$settings_exampleplot <- renderImage(
      expr = {
        list(src = paste0(
          arguments$tempdir,
          "/exampleplot.png"
        ))
      },
      deleteFile = FALSE
    )

    output$settings_download_exampleplot <- downloadHandler(
      filename = function() {
        "Exampleplot.png"
      },
      content = function(file) {
        file.copy(paste0(
          arguments$tempdir,
          "/exampleplot.png"
        ),
        file)
      },
      contentType = "image/png"
    )
  })
}


#' @title module_settings_ui
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
#'     tabName = "settings",
#'     module_settings_ui(
#'       "moduleSettings"
#'     )
#'   )
#' )
#' }
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
        radioButtons(
          ns("settings_selection_method"),
          label = paste0(
            "Criterion to automatically (pre-) select ",
            "the regression method for correction"),
          choices = list(
            "Sum of squared errors (SSE)" = "SSE",
            "Relative Error" = "RelError"
          ),
          selected = "SSE"
        ),
        tags$hr(),
        checkboxInput(
          ns("settings_minmax"),
          label = "Use 'min-max'-correction (default: off)",
          value = FALSE
        ),
        helpText(
          paste0(
            "[CAUTION: This is an experimental feature ",
            "and has neither been tested nor validated!]")
        ),
        width = 12
      ),
      box(
        title = "Expert Settings",
        h5(
          tags$b("It is recommended to not change these ",
                 "settings unless you know exactly, what ",
                 "you are doing!")
        ),
        tags$hr(),
        numericInput(
          ns("settings_seed"),
          label = "Seed",
          value = 1234,
          min = 0,
          max = Inf,
          step = 1,
          width = "30%"
        ),
        helpText(
          paste0("The seed makes the calculation of the ",
                 "unknowns of both, the hyperbolic and the ",
                 "cubic regression equation reproducible.")
        ),
        tags$hr(),
        column(
          4,
          numericInput(
            ns("settings_plot_height"),
            label = "Plot height (unit: inch)",
            value = 5.3,
            min = 1,
            max = 50,
            step = 0.01
          ),
          helpText(
            paste0("If you need a different resolution of ",
                   "the resulting plots, you can set the ",
                   "plot height (in inches) manually here.")
          ),
          tags$hr(),
          numericInput(
            ns("settings_plot_width"),
            label = "Plot width (unit: inch)",
            value = 6.2,
            min = 1,
            max = 50,
            step = 0.01
          ),
          helpText(
            paste0("If you need a different resolution of ",
                   "the resulting plots, you can set the ",
                   "plot width (in inches) manually here.")
          ),
          tags$hr(),
          numericInput(
            ns("settings_plot_textsize"),
            label = "Plot font size",
            value = 15.15,
            min = 1,
            max = 50,
            step = 0.01
          ),
          helpText(
            paste0(
              "The font size of the plots. ",
              "It is passed further to the 'size'-argument ",
              "of ggplot2's 'element_text' function."
            )
          )
        ),
        column(
          8,
          imageOutput(ns("settings_exampleplot")),
          tags$head(
            tags$style(
              type = "text/css",
              paste0(
                "#moduleSettings-settings_exampleplot img ",
                "{max-height: 100%; max-width: 100%; width: auto; ",
                "display: block; margin-left: auto; margin-right: auto;}"))
          ),
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                "moduleSettings-settings_download_exampleplot",
                "Download Example Plot",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;")))
        ),
        column(12,
               tags$hr(),
               helpText(
                 paste0("Please note, that the decimal separator ",
                        "of the numeric input fields is depending ",
                        "on your operating system's and/or your browser's ",
                        "language settings. It can be a comma (',') ",
                        "or a period ('.').")
               )
        ),
        width = 12
      )
    )
  )
}
