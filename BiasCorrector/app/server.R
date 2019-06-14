# BiasCorrector: Correct PCR-bias in DNA methylation analyses
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

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    ending = NULL,
    expFileReq = F,
    type_locus_sampe = NULL,
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
    result_list_type2_corrected = NULL
  )
  
  # run start function
  onStart(plotdir, csvdir, logfilename)
  
  
  # scientific purpose
  showModal(modalDialog(
    title = "This program is to be used for scientific research purposes only",
    "I hereby confirm to use this program only for scientific research purposes.",
    footer = tagList(actionButton("dismiss_modal",label = "Cancel"),
                     modalButton("Confirm"))
  ))
  
  observeEvent(input$dismiss_modal, {
    writeLog("dismiss modal")
    rv$modal_closed <- T
    rv$modal_type <- NULL
    removeModal()
    js$reset()
  })
  
  observeEvent(input$reset, {
    writeLog("restarting app")
    cleanUp(plotdir, csvdir)
    js$reset()
  })
  
  output$samplelocus_out <- reactive({
    paste(rv$sampleLocusName)
  })
  
  # logfileviewer
  observe({
    file <- reactiveFileReader(500, session,
                               logfilename, 
                               readLines)
    rv$logfile <- file()
    
    output$downloadLogfile <- downloadHandler(
      filename = function(){
        paste0("BC_logfile.txt")
      },
      content = function(file){
        write(rv$logfile, file)
      },
      contentType = "text/csv"
    )
  })
  output$log_out <- reactive({
    paste(paste0(rv$logfile, collapse = "\n"))
  })
  
  
  
  
  ###### Experimental data
  # Fileupload module
  callModule(moduleFileuploadServer, "moduleFileupload", rv=rv, input_re=reactive({input}))
  
  # table rendering module
  callModule(moduleExperimentalFileServer, "moduleExperimentalFile", rv=rv)
  
  # some ui stuff
  observe({
    req(rv$fileimportExp)
    cat("\nSome UI Stuff: disable Radiobuttons, experimentalFile and textInput\n")
    # enable logfile-download
    shinyjs::enable("downloadLogfile")
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
    output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Experimental Data", tabName = "panel_1", icon = icon("table"))
      )
    })
  })
  
  observeEvent(
    if (isTRUE(rv$type2cal_uploaded) | isTRUE(rv$type1cal_uploaded)) TRUE
    else return(), {
      # error handling, when uploading new data in same session
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          actionButton("run", "Run Analysis", width = "80%")
        )
      })
      updateTabItems(session, "tabs", "panel_2")
      
      cat("\nSome UI Stuff: disable calibrationFile\n")
      # disable upload possibility of calibration file
      shinyjs::disable("calibrationFile")
    })
  
  
  # ###### Calibration data
  # table rendering module
  callModule(moduleCalibrationFileServer, "moduleCalibrationFile", rv=rv, input_re=reactive({input}))
  
  observe({
    req(rv$fileimportCal)
    # enable calibrationfile download button
    shinyjs::enable("moduleCalibrationFile-downloadCalibration")
  })
  
  
  ###### Run Analysis
  observeEvent(input$run, {
    if (!is.null(rv$fileimportCal)){
      
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line"))
        )
      })
      updateTabItems(session, "tabs", "panel_3")
      shinyjs::disable("run")
    } else if (rv$type_locus_sample == "2"){
      showModal(modalDialog(
        "Please confirm the assignment of the calibration steps.",
        title = "Confirmation needed",
        footer = modalButton("OK")
      ))
    }
  })
  
  
  ###### Plotting
  callModule(modulePlottingServer, "modulePlotting", rv=rv, input_re=reactive({input}))
  
  # when plotting has finished
  observe({
    req(rv$plotting_finished)
    
    if (rv$type_locus_sample == "1"){
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
          menuItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line")),
          menuItem("Select Regression Model", tabName = "panel_5", icon = icon("chart-line"))
        )
      })
    } else {
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
          menuItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line"))
        )
      })
    }
    updateTabItems(session, "tabs", "panel_3")
  })
  
  ###### Regression Statistics
  callModule(moduleStatisticsServer, "moduleStatistics", rv=rv, input_re=reactive({input}))
  
  
  ###### Model Selection
  callModule(moduleModelSelectionServer, "moduleModelSelection", rv=rv, input_re=reactive({input}))
  
  
  # Calculate results for experimental data
  observeEvent(input$results, {
    
    # disable Biascorrection-Button
    shinyjs::disable("results")
    
    if (rv$type_locus_sample == "1"){
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
          menuItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line")),
          menuItem("Select Regression Model", tabName = "panel_5", icon = icon("chart-line")),
          menuItem("BiasCorrected Results", tabName = "panel_6", icon = icon("angellist")),
          menuItem("Corrected Plots", tabName = "panel_7", icon = icon("angellist"))
        )
      })
      
    } else if (rv$type_locus_sample == "2"){
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Experimental Data", tabName = "panel_1", icon = icon("table")),
          menuItem("Calibration Data", tabName = "panel_2", icon = icon("table")),
          menuItem("Regression Plots", tabName = "panel_3", icon = icon("chart-line")),
          menuItem("Regression Statistics", tabName = "panel_4", icon = icon("chart-line")),
          menuItem("BiasCorrected Results", tabName = "panel_6", icon = icon("angellist")),
          menuItem("Corrected Plots", tabName = "panel_7", icon = icon("angellist"))
        )
      })
    }
    updateTabItems(session, "tabs", "panel_6")
    
    # reset reactive values
    rv$calculate_results <- TRUE
    rv$finalResults <- NULL
  })
  
  ###### Calcluate Results
  callModule(moduleResultsServer, "moduleResults", rv=rv, input_re=reactive({input}))
  
  
  ###### Plot Corrected Results
  callModule(moduleCorrectedPlotsServer, "moduleCorrectedPlots", rv=rv, input_re=reactive({input}))
}