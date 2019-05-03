moduleExperimentalFileServer <- function(input, output, session, rv){
  # error handling with fileimport
  observeEvent({
    if (!is.null(rv$fileimportExp)) TRUE
    else return()}, {
      writeLog("(app) Entered observeEvent after fileimport of experimental file")
      
      # if type 1 data
      if (rv$type_locus_sample == "1"){
        
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimportExp, options = list(scrollX = TRUE, pageLength = 20)) %>%
            formatRound(columns=c(2:ncol(rv$fileimportExp)), digits=3)
        })
        
        output$exp_samples <- reactive({
          len <- unique(rv$fileimportExp[,sample_id])
          message <- paste0("Unique samples: ", length(len))
          writeLog(message)
          message
        })
        
        output$exp_samples_raw <- reactive({
          len <- sort(unique(rv$fileimportExp[,sample_id]))
          message <- paste0("Unique sample IDs:\n", paste(len, collapse = ", "))
          writeLog(message)
          message
        })
        
        # if type 2 data
      } else if (rv$type_locus_sample == "2"){
        
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimportExp, options = list(scrollX = TRUE, pageLength = 20)) %>%
            formatRound(columns=c(2:ncol(rv$fileimportExp)), digits=3)
        })
        
        output$exp_samples <- reactive({
          len <- unique(rv$fileimportExp[,locus_id])
          message <- paste0("Unique loci: ", length(len))
          writeLog(message)
          message
        })
        
        output$exp_samples_raw <- reactive({
          len <- sort(unique(rv$fileimportExp[,locus_id]))
          message <- paste0("Unique locus IDs:\n", paste(len, collapse = ", "))
          writeLog(message)
          message
        })
        
      }
    })
}


moduleExperimentalFileUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Experimental Data",
        div(class="row", style="margin: 0.5%"),
        verbatimTextOutput(ns("exp_samples")),
        verbatimTextOutput(ns("exp_samples_raw")),
        dataTableOutput(ns("experimental_data")),
        tags$hr(),
        tags$head(tags$style("#panel_1{margin: 0.5%;}")),
        width = 12
      ))
  )
}