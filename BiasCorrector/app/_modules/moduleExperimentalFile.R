moduleExperimentalFileServer <- function(input, output, session, rv){
  # error handling with fileimport
  observeEvent({
    if (!is.null(rv$fileimportExp)) TRUE
    else return()}, {
      writeLog("(app) Entered observeEvent after fileimport of experimental file")
      
      # if type 1 data
      if (rv$type_locus_sample == "1"){
        
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimportExp, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
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
          message <- paste0("Unique sample IDs:\n", paste(len, collapse = "\n"))
          writeLog(message)
          message
        })
        
        # if type 2 data
      } else if (rv$type_locus_sample == "2"){
        
        output$experimental_data <- DT::renderDataTable({
          DT::datatable(rv$fileimportExp, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
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
          message <- paste0("Unique locus IDs:\n", paste(len, collapse = "\n"))
          writeLog(message)
          message
        })
        
      }
      
      # Download experimental data
      output$downloadExperimental <- downloadHandler(
        
        filename = function(){
          paste0("raw_experimental_data.csv")
        },
        content = function(file){
          writeCSV(rv$fileimportExp, file)
        },
        contentType = "text/csv"
      )
    })
}


moduleExperimentalFileUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(9,
             box(title = "Experimental Data",
                 dataTableOutput(ns("experimental_data")),
                 width = 12
             )),
      column(3,
             box(verbatimTextOutput(ns("exp_samples")),
                 verbatimTextOutput(ns("exp_samples_raw")),
                 tags$head(tags$style("#exp_samples_raw{overflow-y:scroll; max-height: 10vh; background: ghostwhite;}")),
                 tags$hr(),
                 div(class="row", style="text-align: center", downloadButton(ns("downloadExperimental"),
                                                                             "Download experimental file",
                                                                             style="white-space: normal; text-align:center; 
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
                 tags$hr(),
                 width = 12
             )
      )
    )
  )
}