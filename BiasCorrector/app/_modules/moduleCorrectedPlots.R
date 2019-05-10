moduleCorrectedPlotsServer <- function(input, output, session, rv, input_re){
  observe({
    # this is needed, to start plotting, when we have the bias corrected calibration values!
    req(rv$fileimportCal_corrected)
    
      # type 1 data: 
      if (rv$type_locus_sample == "1"){
        if (isFALSE(rv$corrected_finished)){
          
          plottingUtility(rv$fileimportCal_corrected, type=1, samplelocusname=rv$sampleLocusName, rv=rv, mode="corrected")
          
          # save regression statistics to reactive value
          rv$regStats_corrected <- statisticsList(rv$result_list)
          
          createBarErrorPlots(rv$regStats, rv$regStats_corrected, rv)
          
          # on finished
          rv$corrected_finished <- TRUE
          writeLog("Finished plotting corrected")
        }
        
        # else if type 2 data
      } else if (rv$type_locus_sample == "2"){
        cat("\nNot implemented yet\n")
        rv$corrected_finished <- TRUE
        # if (isFALSE(rv$corrected_finished)){
        #   a <- 1
        #   rv$result_list_type2_corrected <- list()
        #   
        #   for (b in names(rv$fileimportCal_corrected)){
        #     rv$vec_cal <- names(rv$fileimportCal_corrected[[a]])[-1]
        #     #print(paste("Length rv$vec_cal:", length(rv$vec_cal)))
        #     
        #     plottingUtility(rv$fileimportCal_corrected[[a]], type=2, samplelocusname=rv$sampleLocusName, b=gsub("[[:punct:]]", "", b), rv=rv, mode="corrected")
        #     
        #     # save regression statistics to reactive value
        #     rv$regStats_corrected[[b]] <- statisticsList(rv$result_list)
        #     rv$result_list_type2_corrected[[b]] <- rv$result_list
        #     a <- a + 1
        #   }
        #   # on finished
        #   rv$corrected_finished <- TRUE
        #   writeLog("Finished plotting corrected")
        # }
      }
  })
  
  # when plotting has finished
  observe({
    req(rv$corrected_finished)
    
    # type 1 data:
    if (rv$type_locus_sample == "1"){
      
      ### Plot tab ###
      # create a list of plotnames to populate selectInput
      plot_output_list <- lapply(1:length(rv$vec_cal), function(g) {
        paste0(gsub("[[:punct:]]", "", rv$vec_cal[g]))
      })
      names(plot_output_list) <- rv$vec_cal
      
      # create reactive selectinput:
      selIn2 <- reactive({
        selectInput(inputId="selectPlot_corrected", label = NULL, multiple = F, selectize = F, choices = plot_output_list, width = "50%")
      })
      
      # create download button for each plot
      output$downloadPlots_corrected <- downloadHandler(
        filename = function(){paste0(rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected.png")},
        content = function(file){
          file.copy(paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected.png"), file)
        },
        contentType = "image/png"
      )
      
      output$downloadPlotsSSE_corrected <- downloadHandler(
        filename = function(){paste0(rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_sse.png")},
        content = function(file){
          file.copy(paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_sse.png"), file)
        },
        contentType = "image/png"
      )
      
      # render head of page with selectInput and downloadbutton
      # TODO align selectinput and button aside of each other
      output$selectPlotInput_corrected <- renderUI({
        s <- selIn2()
        b <- downloadButton("moduleCorrectedPlots-downloadPlots_corrected", "Download Corrected Plot")
        c <- downloadButton("moduleCorrectedPlots-downloadPlotsSSE_corrected", "Download SSE Plot")
        do.call(tagList, list(s, b, c))
      })
      
      # for debugging
      observeEvent(input_re()$selectPlot_corrected, {
        print(input_re()$selectPlot_corrected)
      })
      
      # render plots from local temporary file
      output$plots_corrected <- renderImage({
        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected.png")
        # Return a list containing the filename
        list(src = filename)
      }, deleteFile = FALSE)
      
      # render plots from local temporary file
      output$plotsSSE_corrected <- renderImage({
        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_sse.png")
        # Return a list containing the filename
        list(src = filename)
      }, deleteFile = FALSE)
      
      # type 2 data:
    } else if (rv$type_locus_sample == "2"){
      
      #create list of loci to populate selectInput "selectPlotLocus"
          list_plot_locus <- list()
          for (i in 1:length(rv$fileimportCal_corrected)){
            list_plot_locus[[i]] <- names(rv$fileimportCal_corrected)[i]
          }

          selectPlotLocus <- reactive({
            selectInput(inputId="selectPlotLocus_corrected", label = NULL, multiple = F, selectize = F, choices = list_plot_locus, width = "50%")
          })


          # create list of cpg-sites for each locus to populate selectInput "selectPlotCpG"
          list_plot_cpg <- list()
          for (i in 1:length(rv$fileimportCal_corrected)){
            list_plot_cpg[[names(rv$fileimportCal_corrected)[i]]] <- names(rv$fileimportCal_corrected[[i]])[-1]
          }

          # only return list of CpG-sites for each locus, if there is already a selection of the locus in selectPlotLocus
          cpg_output <- reactive({
            if (!is.null(input_re()$selectPlotLocus_corrected)){
              return(list_plot_cpg[input_re()$selectPlotLocus_corrected])
            }
          })

          # always wrap selectInput into reactive-function
          selectPlotCpG <- reactive({
            selectInput(inputId="selectPlotType2_corrected", label = NULL, multiple = F, selectize = F, choices = cpg_output(), width = "50%")
          })

          # render second selectInput
          output$s2PlotOutput_corrected <- renderUI({
            s3 <- selectPlotCpG()
            s3
          })

          # create download button for each plot
          output$downloadPlots_corrected <- downloadHandler(
            filename = function(){paste0(gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected.png")},
            content = function(file){
              file.copy(paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected.png"), file)
            },
            contentType = "image/png"
          )

          # render Plot UI
          output$selectPlotInput_corrected <- renderUI({
            s1 <- selectPlotLocus()
            s2 <- uiOutput("moduleCorrectedPlots-s2PlotOutput_corrected")
            b <- downloadButton("moduleCorrectedPlots-downloadPlots_corrected", "Download Plot")
            do.call(tagList, list(s1, s2, b))
          })

          # render plot from local temporary file
          output$plots_corrected <- renderImage({
            filename <- paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected.png")
            # print(filename)
            # Return a list containing the filename
            list(src = filename)
          }, deleteFile = FALSE)
    }
  })
}

moduleCorrectedPlotsUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(8,
             box(
               title = "BiasCorrected Regression Plot",
               imageOutput(ns("plots_corrected")),
               width=12
             ),
             box(
               title = "Comparison of Sum of Sqared Errors",
               imageOutput(ns("plotsSSE_corrected")),
               width=12
             )),
      column(4,
             box(
               title = "Plot Selection",
               uiOutput(ns("selectPlotInput_corrected")),
               width = 12)
      )
    )
  )
}