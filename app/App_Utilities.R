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

plottingUtility <- function(data, type, samplelocusname, b=NULL){
  initializeListJ()
  
  # for plotting: basic idea and some code snippets from:
  # https://gist.github.com/wch/5436415/
  plot.list <- reactive({
    regression_type1(data, vec_cal)
  })
  
  # calculate results (if this is run here, j must be resetted)
  plotlistR <- plot.list()
  
  Map(function(f) {
    plotname <- paste0(gsub("[[:punct:]]", "", vec_cal[f]))
    
    # filname of temporary plot
    if (type == 1){
      filename <- paste0(plotdir, samplelocusname, "_", plotname, ".png")
    } else if (type == 2){
      filename <- paste0(plotdir, b, "-", samplelocusname, "_", plotname, ".png")
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


renderRegressionStatisticTable <- function(dt){
  # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
  t <- DT::datatable(dt, colnames = c("Name", "SSE (h)", "b", "y0", "y1", "  ", "SSE (c)", "ax³", "bx²", "cx", "d", "better_model"),
                options = list(scrollX = TRUE, 
                               pageLength = 20,
                               columnDefs = list(list(targets = 12, visible = FALSE))
                )) %>%
    formatRound(columns=c(2:12), digits=3) %>%
    formatStyle(columns = 2,
                valueColumns = "better_model",
                fontWeight = styleEqual(0, "bold")) %>%
    formatStyle(columns = 2:5,
                valueColumns = "better_model",
                backgroundColor = styleEqual(0, "lawngreen")) %>%
    formatStyle(columns = 7,
                valueColumns = "better_model",
                fontWeight = styleEqual(1, "bold")) %>%
    formatStyle(columns = 7:11,
                valueColumns = "better_model",
                backgroundColor = styleEqual(1, "lawngreen")) %>%
    formatStyle(columns = c(1:11), fontSize = "80%")
  return(t)
}


# reset <- function(id) {
#   # get the Shiny session
#   session <- shinyjs:::getSession()
#   
#   # Make sure reset works with namespaces (shiny modules)
#   nsName <- ""
#   if (inherits(session, "session_proxy")) {
#     id <- session$ns(id)
#     nsName <- session$ns("")
#   }
#   
#   print(paste("ID", id))
#   print(paste("NS", nsName))
#   
#   # send a call to JavaScript to figure out what elements to reset and what
#   # values to reset them to
#   shinyInputId <- paste0("shinyjs-resettable-", id)
#   shinyInputIdJs <- shinyInputId
#   if (inherits(session, "session_proxy")) {
#     shinyInputIdJs <- session$ns(shinyInputIdJs)
#   }
#   session$sendCustomMessage("shinyjs-reset", list(id = id,
#                                                   shinyInputId = shinyInputIdJs))
#   
#   # listen for a response from javascript
#   shiny::observeEvent(session$input[[shinyInputId]], once = TRUE, {
#     messages <- session$input[[shinyInputId]]
#     
#     # go through each input element that javascript told us about and call
#     # the corresponding shiny::updateFooInput() with the correct arguments
#     lapply(
#       names(messages),
#       function(x) {
#         type <- messages[[x]][['type']]
#         value <- messages[[x]][['value']]
#         
#         # password inputs don't have an updatePass"wordInput, they use text
#         if (type == "Password") {
#           type <- "Text"
#         }
#         
#         updateFunc <- sprintf("update%sInput", type)
#         
#         # Make sure reset works with namespecing (shiny modules)
#         id <- x
#         if (substring(id, 1, nchar(nsName)) == nsName) {
#           id <- substring(id, nchar(nsName) + 1)
#         }
#         
#         funcParams <- list(session, id)
#         
#         # checkbox values need to be manually converted to TRUE/FALSE
#         if (type == "Checkbox") {
#           value <- as.logical(value)
#         }
#         
#         if (type == "Date") {
#           if (value == "NA") {
#             value <- NA
#           }
#         }
#         
#         # most input update functions use 'value' argument, some use 'selected',
#         # DateRange uses 'start' and 'end'
#         if (type == "RadioButtons") {
#           funcParams[['selected']] <- value
#         } else if (type == "CheckboxGroup" || type == "Select") {
#           if (value == '""') {
#             funcParams[['selected']] <- ""
#           } else {
#             funcParams[['selected']] <- jsonlite::fromJSON(value)
#           }
#         } else if (type == "Slider") {
#           value <- unlist(strsplit(value, ","))
#           funcParams[['value']] <- value
#         } else if (type == "DateRange") {
#           dates <- unlist(strsplit(value, ","))
#           dates[dates == "NA"] <- NA
#           funcParams[['start']] <- dates[1]
#           funcParams[['end']] <- dates[2]
#         } else {
#           funcParams[['value']] <- value
#         }
#         
#         # radio buttons don't follow the regular shiny input naming conventions
#         if (type == "RadioButtons") {
#           updateFunc <- sprintf("update%s", type)
#         }
#         
#         # for colour inputs, need to use the colourpicker package
#         if (type == "Colour") {
#           updateFunc <- utils::getFromNamespace(updateFunc, "colourpicker")
#         }
#         
#         print(paste("Func", updateFunc))
#         print(paste("Params", funcParams))
#         # update the input to its original values
#         do.call(updateFunc, funcParams)
#       }
#     )
#   })
#   
#   invisible(NULL)
# }