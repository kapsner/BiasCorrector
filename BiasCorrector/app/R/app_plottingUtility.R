# plotting utility
plottingUtility <- function(data, type, samplelocusname, b=NULL, rv, mode=NULL){
  initializeListJ(rv)
  
  # for plotting: basic idea and some code snippets from:
  # https://gist.github.com/wch/5436415/
  plot.list <- reactive({
    regression_type1(data, rv$vec_cal, rv, mode)
  })
  
  
  withProgress(message = "Calculating calibration curves", value = 0, {
    incProgress(1/1, detail = "... working on calculations ...")
    # calculate results (if this is run here, j must be resetted)
    plotlistR <- plot.list()
  })
  
  # get number of CpG-sites
  length_vector <- length(rv$vec_cal)
  
  
  pl <<- plotlistR
    
  Map(function(f) {
    plotname <- paste0(gsub("[[:punct:]]", "", rv$vec_cal[f]))
    
    # filename-suffix
    fn_suffix <- ifelse(is.null(mode), "", "_corrected")
    # message suffix
    msg_suffix <- ifelse(is.null(mode), "", "BiasCorrected ")

    # filname of temporary plot
    if (type == 1){
      filename <- paste0(plotdir, samplelocusname, "_", plotname, fn_suffix, ".png")
      plotmessage <- paste0("Creating ", msg_suffix, "plot No. ", f)
    } else if (type == 2){
      filename <- paste0(plotdir, b, "-", samplelocusname, "_", plotname, fn_suffix, ".png")
      plotmessage <- paste0("Locus ID: ", b, " --> Creating ", msg_suffix, "plot No. ", f)
    }
    
    writeLog(paste(plotmessage, "- filename:", filename))
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = plotmessage, value = 0)
    
    # Increment the progress bar, and update the detail text.
    progress$inc(1/1, detail = paste("... working hard on plot", f, "of", length_vector))
    
    # store plots to local temporary file
    createPlots(plotlistR[[f]], f, rv, filename)
    
  }, 1:length_vector)
}
