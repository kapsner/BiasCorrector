moduleModelSelectionServer <- function(input, output, session, rv, input_re){
  
  observe({
    req(rv$plotting_finished)
    # model selection only implemented for type 1 data
    if (rv$type_locus_sample == "1"){
      # render radio buttons for tab 5
      output$reg_radios <- renderUI({
        radio_output_list <- lapply(1:length(rv$vec_cal), function(g) {
          radioname <- paste0("radio", g)
          div(class="row",
              div(class="row", style = "margin: 0.5%;",
                  div(class="col-sm-4", style="text-align: left",
                      h5(tags$b(paste0("Regression type for ", rv$vec_cal[g], ":")))),
                  div(class="col-sm-4",
                      div(class = "row", style = "margin: 0.5%;",
                          radioButtons(inputId = radioname,
                                       label = NULL,
                                       choices = list("hyperbolic" = 0, "cubic" = 1),
                                       selected = as.character(rv$regStats[Name==rv$vec_cal[g], better_model]),
                                       inline = TRUE))
                  ),
                  div(class="col-sm-4",
                      verbatimTextOutput(paste0("moduleModelSelection-text_", radioname)))),
              tags$hr())
        })
        do.call(tagList, list(radio_output_list,
                              div(class="row", style="text-align: center", actionButton("results", "BiasCorrect your experimental data"))
        )) # needed to display properly.
      })
    } else if (rv$type_locus_sample == "2"){
      # type 2 data: 
      # trigger claculation of results (bypass manual model selection)
      #shinyjs::click("results")
    }
  })
  
  
  observe({
    if (input_re()$tabs == "panel_5"){
      
      if (rv$type_locus_sample == "1"){
        
        lapply(1:length(rv$vec_cal), function(h) {
          radioname <- paste0("text_radio", h)
          output[[radioname]] <- reactive({
            paste("SSE:",
                  as.character(ifelse(input_re()[[paste0("radio", h)]] == 1,
                                      rv$regStats[Name==rv$vec_cal[h], SSE_cubic],
                                      rv$regStats[Name==rv$vec_cal[h], SSE_hyperbolic]))
            )
          })
        })
        
        lapply(1:length(rv$vec_cal), function(k) {
          radioname <- paste0("radio", k)
          if (!is.null(input_re()[[radioname]])){
              output[[paste0("text_", radioname)]] <- reactive({
                paste("SSE:",
                      as.character(ifelse(input_re()[[radioname]] == "1",
                                          rv$regStats[Name==rv$vec_cal[k], round(SSE_cubic,3)],
                                          rv$regStats[Name==rv$vec_cal[k], round(SSE_hyperbolic, 3)]))
                )
              })
          }
        })
      }
    }
  })
}

moduleModelSelectionUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Select Regression Model",
        div(class="row", style="margin: 0.5%"),
        uiOutput(ns("reg_radios")),
        tags$hr(),
        width = 12
      ))
  )
}