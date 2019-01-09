# run this function on start. it creates the temporary directory for the plots
# TODO in the end, generate markdown pdf with plots in temporary directory
onStart <- function(){
  writeLog("(app) starting..... running 'onStart'-Function")
  plotdir <<- paste0(tempdir(), "/plots/")
  dir.create(plotdir)
  
  # scientific purpose
  showModal(modalDialog(
    "This program is only to be used for scientific purposes.",
    title = "Confirmation of scientific use only.",
    footer = tagList(actionButton("dismiss_modal",label = "Cancel"),
                     modalButton("Confirm"))
  ))
}


# requirements error + modal view
requirementsError <- function(data_type){
  
  footer_dis = actionButton("dismiss_modal",label = "Dismiss")
  title_filereq = "File requirements error!"
  
  if (data_type %in% c("experimentalFile", "calibrationFile")){
    title = title_filereq
    message = "The file provided does not meet the file requirements! Please upload a new file! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "locusname"){
    title = "No locus specified!"
    message = "Please specify an appropriate name for the gene or locus of your experiment!"
    footer = footer_dis
  } else if (data_type == "samplename"){
    title = "No sample name specified!"
    message = "Please specify an appropriate name of the sample of your experiment!"
    footer = footer_dis
  } else if (data_type == "csv"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "dim"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! All files have to have the same number of columns and rows. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "four"){
    title = title_filereq
    message = "Please upload at least 4 CSV files containing the calibration data. For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "naming"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! All files need to have the same rownames (locus ids) and columnnames (CpG sites). Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "filename"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! Filenaming of the calibration files must be done properly. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "calibrange2"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! Calibration steps must be in range '0 <= calibration step <= 100'. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = NULL
  } else if (data_type == "calibrange3"){
    title = title_filereq
    message = "The calibration steps provided do not meet the file requirements! Calibration steps must be in range '0 <= calibration step <= 100'. Each calibration step may only be assigned once. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = NULL
  } else if (data_type == "inconsistency"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! Please specify an equal number of CpG-sites for each gene locus. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  }
  # show modal here
  showModal(modalDialog(
    message,
    title = title,
    footer = footer
  ))
}


# give back deleted rows
omitnasModal <- function(omitnas, data_type){
  if (data_type == "experimental"){
    data_type <- "experimental data"
  } else {
    data_type <- "calibration data"
  }
  if (omitnas == 1){
    message = paste0("Deleted 1 row containing missing values from ", data_type, ".")
  } else {
    message = paste0("Deleted ", omitnas, " rows containing missing values from ", data_type, ".")
  }
  # show modal here
  showModal(modalDialog(
    message,
    title = "Missing values deleted"
  ))
  # remove global omitnas variable
  rm(omitnas, pos = ".GlobalEnv")
  invisible(gc())
}

plottingUtility <- function(data, type, samplelocusname, a=NULL){
  initializeListJ()
  
  # for plotting: basic idea and some code snippets from:
  # https://gist.github.com/wch/5436415/
  plot.list <- reactive({
    regression_type1(data, vec_cal)
  })
  
  # calculate results (if this is run here, j must be resetted)
  plotlistR <- plot.list()
  
  Map(function(f) {
    plotname <- paste0(f, "_", gsub("[[:punct:]]", "", vec_cal[f]))
    
    # filname of temporary plot
    if (type == 1){
      filename <- paste0(plotdir, samplelocusname, "_", plotname, ".png")
    } else if (type == 2){
      filename <- paste0(plotdir, a, "-", samplelocusname, "_", plotname, ".png")
    }
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste("Creating plot", f), value = 0)
    
    # Increment the progress bar, and update the detail text.
    progress$inc(1/1, detail = paste0("... working hard on plot ", f))
    
    # store plots to local temporary file
    plotPNG({
      # add plot and plot statistics here, "j" is necessary to get values for curve in equations
      # always reset j to f
      j <<- f
      return(print(plotlistR[[f]] +
                     stat_function(fun = hyperbolic_equation, geom = "line", aes(colour = "Hyperbolic")) +
                     stat_function(fun = cubic_equation, geom = "line", aes(colour = "Cubic")) +
                     scale_colour_manual("Regression:", values = c("red", "green"))))
    },
    filename = filename,
    height = 400, 
    width = 600)
    
  }, 1:length(vec_cal))
}