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

shiny::shinyServer(function(input, output, session) {

  # Generate session-specific temporary directories
  session_id <- session$token
  session_tempdir <- file.path(tempdir(), session_id)
  dir.create(session_tempdir, showWarnings = FALSE)

  # Get defaults from options set in launch_app
  opt_plotdir <- getOption("biascorrector.plotdir", default = "plots")
  opt_csvdir <- getOption("biascorrector.csvdir", default = "csv")
  opt_logfilename <- getOption("biascorrector.logfilename",
                               default = "biascorrector.log")
  opt_parallel <- getOption("biascorrector.parallel", default = TRUE)

  # Create session-specific subdirectories
  session_plotdir <- file.path(session_tempdir, opt_plotdir)
  session_csvdir <- file.path(session_tempdir, opt_csvdir)
  session_logfile <- file.path(session_tempdir, opt_logfilename)

  dir.create(session_plotdir, showWarnings = FALSE)
  dir.create(session_csvdir, showWarnings = FALSE)

  rv <- shiny::reactiveValues(
    ending = NULL,
    exp_filereq = FALSE,
    type_locus_sampe = NULL,
    # 1
    # currently there is only type 1 correction implemented
    fileimport_experimental = NULL,
    fileimport_calibration = NULL,
    fileimport_list = NULL,
    reg_stats = NULL,
    modal_closed = TRUE,
    modal_type = NULL,
    calculate_results = FALSE,
    final_results = NULL,
    sample_locus_name = NULL,
    type2cal_uploaded = FALSE,
    type1cal_uploaded = FALSE,
    plotting_finished = FALSE,
    substitutions = NULL,
    substitutions_calc = NULL,
    type2_calcres = NULL,
    choices_list = NULL,
    result_list_type2 = NULL,
    result_list = NULL,
    temp_results = NULL,
    vec_cal = NULL,
    plotdir = session_plotdir,
    csvdir = session_csvdir,
    tempdir = session_tempdir,
    logfilename = session_logfile,
    b = NULL,
    omitnas = NULL,
    calibr_steps = NULL,
    logfile = NULL,
    corrected_finished = FALSE,
    fileimport_cal_corrected = NULL,
    reg_stats_corrected = NULL,
    result_list_type2_corrected = NULL,
    minmax = FALSE, # initial minmax-value
    row_callback = c(
      "function(row, data) {",
      "  for(var i=0; i<data.length; i++) {",
      "    if(data[i] === null) {",
      "      $('td:eq('+i+')', row).html('NA')",
      "        .css({",
      "'color': 'rgb(151,151,151)',",
      "'font-style': 'italic'});",
      "    }",
      "  }",
      "}"
    )
  )

  # run start function
  rBiasCorrection::on_start(
    plotdir = session_plotdir,
    csvdir = session_csvdir,
    logfilename = session_logfile,
    parallel = opt_parallel
  )

  message(paste0("session plotdir: ", session_plotdir))
  message(paste0("session csvdir: ", session_csvdir))
  message(paste0("session tempdir: ", session_tempdir))
  message(paste0("session logfile: ", session_logfile))

  # TODO original selection of data type (hard coded to type 1 data)
  rv$type_locus_sample <- 1

  # scientific purpose
  shiny::showModal(shiny::modalDialog(
    title = paste0(
      "This program is to be used for scientific ",
      "research purposes only"
    ),
    paste0(
      "I hereby confirm to use this program only for ",
      "scientific research purposes."
    ),
    footer = shiny::tagList(
      shiny::actionButton("dismiss_modal", label = "Cancel"),
      shiny::modalButton("Confirm")
    )
  ))

  shiny::observeEvent(input$dismiss_modal, {
    rBiasCorrection::write_log(
      message = "dismiss modal",
      logfilename = session_logfile
    )
    rv$modal_closed <- TRUE
    rv$modal_type <- NULL
    shiny::removeModal()
    session$reload()
  })

  shiny::observeEvent(input$reset, {
    rBiasCorrection::write_log(
      message = "restarting app",
      logfilename = session_logfile
    )
    rBiasCorrection::clean_up(session_plotdir, session_csvdir)
    session$reload()
  })

  output$samplelocus_out <- shiny::reactive({
    paste(rv$sample_locus_name)
  })

  input_reactive <- reactive({
    input
  })

  ###### Experimental data
  # Fileupload module
  shiny::callModule(
    BiasCorrector::module_fileupload_server,
    "moduleFileupload",
    rv = rv,
    input_re = input_reactive,
    logfilename = session_logfile
  )

  # table rendering module
  shiny::callModule(
    BiasCorrector::module_experimentalfile_server,
    "moduleExperimentalFile",
    rv = rv,
    logfilename = session_logfile
  )

  # some ui stuff
  shiny::observe({
    shiny::req(rv$fileimport_experimental)
    cat(
      paste0(
        "\nSome UI Stuff: disable Radiobuttons, ",
        "experimentalFile and textInput\n"
      )
    )
    # disable exampledata-button
    shinyjs::disable("moduleFileupload-load_example_data")
    # enable logfile-download
    shinyjs::enable("moduleLog-download_logfile")
    # disable radiobuttons
    shinyjs::disable("moduleFileupload-type_locus_sample")
    # disable upload possibility of experimental file
    shinyjs::disable("moduleFileupload-experimentalFile")
    # disable textinput
    if (rv$type_locus_sample == "1") {
      shinyjs::disable("moduleFileupload-locusname")
    } else if (rv$type_locus_sample == "1") {
      shinyjs::disable("moduleFileupload-samplename")
    }
    # render menu with experimental file
    output$menu <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "Experimental Data",
          tabName = "panel_1",
          icon = icon("table")
        )
      )
    })
  })

  shiny::observeEvent(
    eventExpr = {
      req(isTRUE(rv$type2cal_uploaded) ||
          isTRUE(rv$type1cal_uploaded))
    },
    handlerExpr = {
        # error handling, when uploading new data in same session
        output$menu <- shinydashboard::renderMenu({
          shinydashboard::sidebarMenu(
            shinydashboard::menuItem(
              "Experimental Data",
              tabName = "panel_1",
              icon = icon("table")
            ),
            shinydashboard::menuItem(
              "Calibration Data",
              tabName = "panel_2",
              icon = icon("table")
            ),
            shiny::actionButton(
              "run",
              "Run Analysis",
              style = paste0(
                "white-space: normal; ",
                "text-align:center; ",
                "padding: 9.5px 9.5px 9.5px 9.5px;",
                "margin: 6px 10px 6px 10px;"
              )
            )
          )
        })
        shinydashboard::updateTabItems(session, "tabs", "panel_2")

        cat("\nSome UI Stuff: disable calibrationFile\n")
        # disable upload possibility of calibration file
        shinyjs::disable("calibrationFile")
      })

  # ###### Calibration data
  # table rendering module
  shiny::callModule(
    BiasCorrector::module_calibrationfile_server,
    "moduleCalibrationFile",
    rv = rv,
    input_re = input_reactive,
    logfilename = session_logfile
  )

  shiny::observe({
    shiny::req(rv$fileimport_calibration)
    # enable calibrationfile download button
    shinyjs::enable("moduleCalibrationFile-download_calibration")
  })

  ###### Run Analysis
  shiny::observeEvent(input$run, {
    if (!is.null(rv$fileimport_calibration)) {
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Experimental Data",
            tabName = "panel_1",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Calibration Data",
            tabName = "panel_2",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Regression Results",
            icon = shiny::icon("chart-line"),
            startExpanded = TRUE,
            shinydashboard::menuSubItem(
              "Regression Plots",
              tabName = "panel_3",
              icon = shiny::icon("chart-line"),
              selected = TRUE
            )
          )
        )
      })
      shinydashboard::updateTabItems(session, "tabs", "panel_3")

      # disable run-analysis button
      shinyjs::disable("run")

      # disable some settings here
      shinyjs::disable("moduleSettings-settings_minmax")

    } else if (rv$type_locus_sample == "2") {
      shiny::showModal(
        shiny::modalDialog(
          "Please confirm the assignment of the calibration steps.",
          title = "Confirmation needed",
          footer = shiny::modalButton("OK")
        )
      )
    }
  })

  ###### Plotting
  observe({
    # this is needed, to open new tab (Regression plots)
    # before rendering the plots!
    if (input$tabs == "panel_3") {
      rv$run <- TRUE
    }
  })
  shiny::callModule(
    BiasCorrector::module_plotting_server,
    "modulePlotting",
    rv = rv,
    input_re = input_reactive,
    logfilename = session_logfile,
    plotdir = session_plotdir
  )

  # when plotting has finished
  shiny::observe({
    shiny::req(rv$plotting_finished)

    if (rv$type_locus_sample == "1") {
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Experimental Data",
            tabName = "panel_1",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Calibration Data",
            tabName = "panel_2",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Regression Results",
            icon = shiny::icon("chart-line"),
            startExpanded = FALSE,
            shinydashboard::menuSubItem(
              "Regression Plots",
              tabName = "panel_3",
              icon = shiny::icon("chart-line"),
              selected = TRUE
            ),
            shinydashboard::menuSubItem(
              "Regression Statistics",
              tabName = "panel_4",
              icon = shiny::icon("angellist")
            ),
            shinydashboard::menuSubItem(
              "Corrected Regression Plots",
              tabName = "panel_7",
              icon = shiny::icon("chart-line")
            ),
            shinydashboard::menuSubItem(
              "Corrected Regression Statistics",
              tabName = "panel_8",
              icon = shiny::icon("angellist")
            ),
            shinydashboard::menuSubItem(
              "Select Regression Model",
              tabName = "panel_5",
              icon = shiny::icon("chart-line")
            ),
            shiny::actionButton(
              "results",
              "BiasCorrect experimental data",
              style = paste0(
                "white-space: normal; ",
                "text-align:center; ",
                "padding: 9.5px 9.5px 9.5px 9.5px; ",
                "margin: 6px 10px 6px 10px;"
              )
            )
          )
        )
      })
    } else {
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Experimental Data",
            tabName = "panel_1",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Calibration Data",
            tabName = "panel_2",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Regression Results",
            icon = shiny::icon("chart-line"),
            startExpanded = FALSE,
            shinydashboard::menuSubItem(
              "Regression Plots",
              tabName = "panel_3",
              icon = shiny::icon("chart-line"),
              selected = TRUE
            ),
            shinydashboard::menuSubItem(
              "Regression Statistics",
              tabName = "panel_4",
              icon = shiny::icon("chart-line")
            ),
            shinydashboard::menuSubItem(
              "Corrected Regression Plots",
              tabName = "panel_7",
              icon = shiny::icon("angellist")
            )
          )
        )
      })
    }
    shinydashboard::updateTabItems(session, "tabs", "panel_3")
  })

  ###### Regression Statistics
  shiny::callModule(
    BiasCorrector::module_statistics_server,
    "moduleStatistics",
    rv = rv,
    input_re = input_reactive
  )

  ###### Plot Corrected Results
  shiny::callModule(
    BiasCorrector::module_correctedplots_server,
    "moduleCorrectedPlots",
    rv = rv,
    input_re = input_reactive,
    logfilename = session_logfile,
    plotdir = session_plotdir
  )

  ###### Statistics Corrected Results
  shiny::callModule(
    BiasCorrector::module_correctedstats_server,
    "moduleCorrectedStatistics",
    rv = rv,
    input_re = input_reactive
  )

  ###### Model Selection
  shiny::callModule(
    BiasCorrector::module_modelselection_server,
    "moduleModelSelection",
    rv = rv,
    input_re = input_reactive
  )

  # Calculate results for experimental data
  shiny::observeEvent(input$results, {
    # disable Biascorrection-Button
    shinyjs::disable("results")

    if (rv$type_locus_sample == "1") {
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Experimental Data",
            tabName = "panel_1",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Calibration Data",
            tabName = "panel_2",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Regression Results",
            icon = shiny::icon("chart-line"),
            startExpanded = FALSE,
            shinydashboard::menuSubItem(
              "Regression Plots",
              tabName = "panel_3",
              icon = shiny::icon("chart-line")
            ),
            shinydashboard::menuSubItem(
              "Regression Statistics",
              tabName = "panel_4",
              icon = shiny::icon("angellist")
            ),
            shinydashboard::menuSubItem(
              "Corrected Regression Plots",
              tabName = "panel_7",
              icon = shiny::icon("chart-line")
            ),
            shinydashboard::menuSubItem(
              "Corrected Regression Statistics",
              tabName = "panel_8",
              icon = shiny::icon("angellist")
            ),
            shinydashboard::menuSubItem(
              "Select Regression Model",
              tabName = "panel_5",
              icon = shiny::icon("chart-line")
            )
          ),
          shinydashboard::menuItem(
            "BiasCorrected Results",
            tabName = "panel_6",
            icon = shiny::icon("angellist")
          )
        )
      })

    } else if (rv$type_locus_sample == "2") {
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Experimental Data",
            tabName = "panel_1",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Calibration Data",
            tabName = "panel_2",
            icon = shiny::icon("table")
          ),
          shinydashboard::menuItem(
            "Regression Results",
            icon = shiny::icon("chart-line"),
            startExpanded = FALSE,
            shinydashboard::menuSubItem(
              "Regression Plots",
              tabName = "panel_3",
              icon = shiny::icon("chart-line")
            ),
            shinydashboard::menuSubItem(
              "Regression Statistics",
              tabName = "panel_4",
              icon = shiny::icon("chart-line")
            ),
            shinydashboard::menuSubItem(
              "Corrected Regression Plots",
              tabName = "panel_7",
              icon = shiny::icon("angellist")
            )
          ),
          shinydashboard::menuItem(
            "BiasCorrected Results",
            tabName = "panel_6",
            icon = shiny::icon("angellist")
          )
        )
      })
    }

    # reset reactive values
    rv$calculate_results <- TRUE
    rv$final_results <- NULL

    shinydashboard::updateTabItems(session, "tabs", "panel_6")
  })

  ###### Calcluate Results
  shiny::callModule(
    BiasCorrector::module_results_server,
    "moduleResults",
    rv = rv,
    input_re = input_reactive,
    logfilename = session_logfile,
    csvdir = session_csvdir,
    plotdir = session_plotdir
  )

  ###### Logs
  shiny::callModule(
    BiasCorrector::module_log_server,
    "moduleLog",
    rv = rv,
    input_re = input_reactive,
    logfilename = session_logfile
  )

  ###### Settings
  shiny::callModule(
    BiasCorrector::module_settings_server,
    "moduleSettings",
    rv = rv,
    input_re = input_reactive,
    logfilename = session_logfile,
    tempdir = session_tempdir
  )

  ###### Info
  shiny::callModule(
    BiasCorrector::module_info_server,
    "moduleInfo",
    rv = rv,
    input_re = input_reactive
  )
})
