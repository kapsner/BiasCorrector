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

shiny::shinyServer(function(input, output, session) {

  rv <- shiny::reactiveValues(
    ending = NULL,
    expFileReq = F,
    type_locus_sampe = NULL, # 1, # currently there is only type 1 correction implemented
    fileimportExp = NULL,
    fileimportCal = NULL,
    fileimportList = NULL,
    regStats = NULL,
    modal_closed = T,
    modal_type = NULL,
    calculate_results = FALSE,
    finalResults = NULL,
    sampleLocusName = NULL,
    type2cal_uploaded = FALSE,
    type1cal_uploaded = FALSE,
    plotting_finished = FALSE,
    substitutions = NULL,
    substitutionsCalc = NULL,
    type2CalcRes = NULL,
    choices_list = NULL,
    result_list_type2 = NULL,
    result_list = NULL,
    temp_results = NULL,
    vec_cal = NULL,
    plotdir = NULL,
    csvdir = NULL,
    tempdir = NULL,
    b = NULL,
    omitnas = NULL,
    calibr_steps = NULL,
    logfile = NULL,
    corrected_finished = FALSE,
    fileimportCal_corrected = NULL,
    regStats_corrected = NULL,
    result_list_type2_corrected = NULL,
    minmax = FALSE # initial minmax-value
  )

  # run start function
  PCRBiasCorrection::onStart_(plotdir, csvdir, logfilename)

  # TODO original selection of data type (hard coded to type 1 data)
  rv$type_locus_sample <- 1

  # scientific purpose
  shiny::showModal(shiny::modalDialog(
    title = "This program is to be used for scientific research purposes only",
    "I hereby confirm to use this program only for scientific research purposes.",
    footer = shiny::tagList(shiny::actionButton("dismiss_modal",label = "Cancel"),
                            shiny::modalButton("Confirm"))
  ))

  shiny::observeEvent(input$dismiss_modal, {
    PCRBiasCorrection::writeLog_("dismiss modal", logfilename = logfilename)
    rv$modal_closed <- T
    rv$modal_type <- NULL
    shiny::removeModal()
    shinyjs::js$reset()
  })

  shiny::observeEvent(input$reset, {
    PCRBiasCorrection::writeLog_("restarting app", logfilename = logfilename)
    PCRBiasCorrection::cleanUp_(plotdir, csvdir)
    shinyjs::js$reset()
  })

  output$samplelocus_out <- shiny::reactive({
    paste(rv$sampleLocusName)
  })


  ###### Experimental data
  # Fileupload module
  shiny::callModule(moduleFileuploadServer, "moduleFileupload", rv=rv, input_re=reactive({input}))

  # table rendering module
  shiny::callModule(moduleExperimentalFileServer, "moduleExperimentalFile", rv=rv)

  # some ui stuff
  shiny::observe({
    shiny::req(rv$fileimportExp)
    cat("\nSome UI Stuff: disable Radiobuttons, experimentalFile and textInput\n")
    # disable exampledata-button
    shinyjs::disable("moduleFileupload-load_example_data")
    # enable logfile-download
    shinyjs::enable("moduleLog-downloadLogfile")
    # disable radiobuttons
    shinyjs::disable("moduleFileupload-type_locus_sample")
    # disable upload possibility of experimental file
    shinyjs::disable("moduleFileupload-experimentalFile")
    # disable textinput
    if (rv$type_locus_sample == "1"){
      shinyjs::disable("moduleFileupload-locusname")
    } else if (rv$type_locus_sample == "1"){
      shinyjs::disable("moduleFileupload-samplename")
    }
    # render menu with experimental file
    output$menu <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Experimental Data", tabName = "panel_1", icon = icon("table"))
      )
    })
  })

  shiny::observeEvent(
    if (isTRUE(rv$type2cal_uploaded) | isTRUE(rv$type1cal_uploaded)) TRUE
    else return(), {
      # error handling, when uploading new data in same session
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          shinydashboard::menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          shiny::actionButton("run", "Run Analysis", style="white-space: normal; text-align:center;
                              padding: 9.5px 9.5px 9.5px 9.5px;
                              margin: 6px 10px 6px 10px;")
        )
      })
      shinydashboard::updateTabItems(session, "tabs", "panel_2")

      cat("\nSome UI Stuff: disable calibrationFile\n")
      # disable upload possibility of calibration file
      shinyjs::disable("calibrationFile")
    })


  # ###### Calibration data
  # table rendering module
  shiny::callModule(moduleCalibrationFileServer, "moduleCalibrationFile", rv=rv, input_re=reactive({input}))

  shiny::observe({
    shiny::req(rv$fileimportCal)
    # enable calibrationfile download button
    shinyjs::enable("moduleCalibrationFile-downloadCalibration")
  })


  ###### Run Analysis
  shiny::observeEvent(input$run, {
    if (!is.null(rv$fileimportCal)){

      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          shinydashboard::menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          shinydashboard::menuItem("Regression Results", tabName = "panel_0", icon = icon("chart-line"),
                                   shinydashboard::menuSubItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line"))
          )
        )
      })
      shinydashboard::updateTabItems(session, "tabs", "panel_3")

      # disable run-analysis button
      shinyjs::disable("run")

      # disable some settings here
      shinyjs::disable("moduleSettings-settings_minmax")

    } else if (rv$type_locus_sample == "2"){
      shiny::showModal(shiny::modalDialog(
        "Please confirm the assignment of the calibration steps.",
        title = "Confirmation needed",
        footer = shiny::modalButton("OK")
      ))
    }
  })


  ###### Plotting
  shiny::callModule(modulePlottingServer, "modulePlotting", rv=rv, input_re=reactive({input}))

  # when plotting has finished
  shiny::observe({
    shiny::req(rv$plotting_finished)

    if (rv$type_locus_sample == "1"){
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          shinydashboard::menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          shinydashboard::menuItem("Regression Results", icon = icon("chart-line"), startExpanded = T,
                                   shinydashboard::menuSubItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line"), selected = T),
                                   shinydashboard::menuSubItem("Regression Statistics", tabName = "panel_4", icon = icon("angellist")),
                                   shinydashboard::menuSubItem("Corrected Regression Plots", tabName = "panel_7", icon = icon("chart-line")),
                                   shinydashboard::menuSubItem("Corrected Regression Statistics", tabName = "panel_8", icon = icon("angellist")),
                                   shinydashboard::menuSubItem("Select Regression Model", tabName = "panel_5", icon = icon("chart-line")),
                                   shiny::actionButton("results", "BiasCorrect experimental data", style="white-space: normal; text-align:center;
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")
          )
        )
      })
    } else {
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          shinydashboard::menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          shinydashboard::menuItem("Regression Results", icon = icon("chart-line"), startExpanded = T,
                                   shinydashboard::menuSubItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line"), selected = T),
                                   shinydashboard::menuSubItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line")),
                                   shinydashboard::menuSubItem("Corrected Regression Plots", tabName = "panel_7", icon = icon("angellist"))
          )
        )
      })
    }
  })

  ###### Regression Statistics
  shiny::callModule(moduleStatisticsServer, "moduleStatistics", rv=rv, input_re=reactive({input}))

  ###### Plot Corrected Results
  shiny::callModule(moduleCorrectedPlotsServer, "moduleCorrectedPlots", rv=rv, input_re=reactive({input}))
  
  ###### Statistics Corrected Results
  shiny::callModule(moduleCorrectedStatisticsServer, "moduleCorrectedStatistics", rv=rv, input_re=reactive({input}))

  ###### Model Selection
  shiny::callModule(moduleModelSelectionServer, "moduleModelSelection", rv=rv, input_re=reactive({input}))


  # Calculate results for experimental data
  shiny::observeEvent(input$results, {

    # disable Biascorrection-Button
    shinyjs::disable("results")

    if (rv$type_locus_sample == "1"){
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          shinydashboard::menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          shinydashboard::menuItem("Regression Results", icon = icon("chart-line"), startExpanded = F,
                                   shinydashboard::menuSubItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
                                   shinydashboard::menuSubItem("Regression Statistics", tabName = "panel_4", icon = icon("angellist")),
                                   shinydashboard::menuSubItem("Corrected Regression Plots", tabName = "panel_7", icon = icon("chart-line")),
                                   shinydashboard::menuSubItem("Corrected Regression Statistics", tabName = "panel_8", icon = icon("angellist")),
                                   shinydashboard::menuSubItem("Select Regression Model", tabName = "panel_5", icon = icon("chart-line"))
          ),
          shinydashboard::menuItem("BiasCorrected Results", tabName = "panel_6", icon = icon("angellist"))
        )
      })

    } else if (rv$type_locus_sample == "2"){
      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          shinydashboard::menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          shinydashboard::menuItem("Regression Results", icon = icon("chart-line"), startExpanded = F,
                                   shinydashboard::menuSubItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
                                   shinydashboard::menuSubItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line")),
                                   shinydashboard::menuSubItem("Corrected Regression Plots", tabName = "panel_7", icon = icon("angellist"))
          ),
          shinydashboard::menuItem("BiasCorrected Results", tabName = "panel_6", icon = icon("angellist"))
        )
      })
    }
    shinydashboard::updateTabItems(session, "tabs", "panel_6")

    # reset reactive values
    rv$calculate_results <- TRUE
    rv$finalResults <- NULL
  })

  ###### Calcluate Results
  shiny::callModule(moduleResultsServer, "moduleResults", rv=rv, input_re=reactive({input}))

  ###### Logs
  shiny::callModule(moduleLogServer, "moduleLog", rv=rv, input_re=reactive({input}))

  ###### Settings
  shiny::callModule(moduleSettingsServer, "moduleSettings", rv=rv, input_re=reactive({input}))

  ###### Info
  shiny::callModule(moduleInfoServer, "moduleInfo", rv=rv, input_re=reactive({input}))
})
