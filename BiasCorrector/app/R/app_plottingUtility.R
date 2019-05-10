# plotting utility
plottingUtility <- function(data, type, samplelocusname, b=NULL, rv, mode=NULL){
  initializeListJ(rv)
  
  # for plotting: basic idea and some code snippets from:
  # https://gist.github.com/wch/5436415/
  plot.list <- reactive({
    regression_type1(data, rv$vec_cal, rv)
  })
  
  if (is.null(mode)){
    cat("\nMode is NULL\n")
    withProgress(message = "Calculating calibration curves", value = 0, {
      incProgress(1/1, detail = "... working on calculations ...")
      # calculate results (if this is run here, j must be resetted)
      plotlistR <- plot.list()
    })
    
    length_vector <- length(rv$vec_cal)
    
    Map(function(f) {
      plotname <- paste0(gsub("[[:punct:]]", "", rv$vec_cal[f]))
      
      # filname of temporary plot
      if (type == 1){
        filename <- paste0(plotdir, samplelocusname, "_", plotname, ".png")
        plotmessage <- paste("Creating plot No.", f)
      } else if (type == 2){
        filename <- paste0(plotdir, b, "-", samplelocusname, "_", plotname, ".png")
        plotmessage <- paste("Locus ID:", b, "--> Creating plot No.", f)
      }
      
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
    
  } else if (mode == "corrected"){
    cat("\nMode is corrected\n")
    # do stuff silently
    plotlistR <- plot.list()
    
    length_vector <- length(rv$vec_cal)
    
    Map(function(f) {
      plotname <- paste0(gsub("[[:punct:]]", "", rv$vec_cal[f]))
      
      # filname of temporary plot
      if (type == 1){
        filename <- paste0(plotdir, samplelocusname, "_", plotname, "_corrected.png")
      } else if (type == 2){
        filename <- paste0(plotdir, b, "-", samplelocusname, "_", plotname, "_corrected.png")
      }
      
      # store plots to local temporary file
      createPlots(plotlistR[[f]], f, rv, filename)
      
    }, 1:length_vector)
  }
}
