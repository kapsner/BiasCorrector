modulePlottingServer <- function(input, output, session, rv, input_re){
  observe({
    # this is needed, to open new tab (Regression plots) before rendering the plots!
    if (input_re()$tabs == "panel_3"){
      
      # type 1 data: 
      if (rv$type_locus_sample == "1"){
        if (isFALSE(rv$plotting_finished)){
          
          plottingUtility(rv$fileimportCal, type=1, samplelocusname=rv$sampleLocusName, rv=rv)
          
          # on finished
          rv$plotting_finished <- TRUE
          writeLog("Finished plotting")
          
          # save regression statistics to reactive value
          rv$regStats <- statisticsList(rv$result_list)
        }
        
        # else if type 2 data
      } else if (rv$type_locus_sample == "2"){
        
        if (isFALSE(rv$plotting_finished)){
          a <- 1
          rv$result_list_type2 <- list()
          
          for (b in names(rv$fileimportCal)){
            rv$vec_cal <- names(rv$fileimportCal[[a]])[-1]
            #print(paste("Length rv$vec_cal:", length(rv$vec_cal)))
            
            plottingUtility(rv$fileimportCal[[a]], type=2, samplelocusname=rv$sampleLocusName, b=gsub("[[:punct:]]", "", b), rv=rv)
            
            # save regression statistics to reactive value
            rv$regStats[[b]] <- statisticsList(rv$result_list)
            rv$result_list_type2[[b]] <- rv$result_list
            a <- a + 1
          }
          # on finished
          rv$plotting_finished <- TRUE
          writeLog("Finished plotting")
        }
      }
    }
  })
  
  # when plotting has finished
  observe({
    req(rv$plotting_finished)
    
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
        selectInput(inputId="selectPlot", label = NULL, multiple = F, selectize = F, choices = plot_output_list, width = "50%")
      })
      
      # create download button for each plot
      output$downloadPlots <- downloadHandler(
        filename = function(){paste0(rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot), ".png")},
        content = function(file){
          file.copy(paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot), ".png"), file)
        },
        contentType = "image/png"
      )
      
      # render head of page with selectInput and downloadbutton
      # TODO align selectinput and button aside of each other
      output$selectPlotInput <- renderUI({
        s <- selIn2()
        b <- downloadButton("modulePlotting-downloadPlots", "Download Plot")
        do.call(tagList, list(s, b))
      })
      
      # for debugging
      observeEvent(input_re()$selectPlot, {
        print(input_re()$selectPlot)
      })
      
      # render plots from local temporary file
      output$plots <- renderImage({
        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot), ".png")
        # Return a list containing the filename
        list(src = filename)
      }, deleteFile = FALSE)
      
      # type 2 data:
    } else if (rv$type_locus_sample == "2"){
      
      #create list of loci to populate selectInput "selectPlotLocus"
          list_plot_locus <- list()
          for (i in 1:length(rv$fileimportCal)){
            list_plot_locus[[i]] <- names(rv$fileimportCal)[i]
          }

          selectPlotLocus <- reactive({
            selectInput(inputId="selectPlotLocus", label = NULL, multiple = F, selectize = F, choices = list_plot_locus, width = "50%")
          })


          # create list of cpg-sites for each locus to populate selectInput "selectPlotCpG"
          list_plot_cpg <- list()
          for (i in 1:length(rv$fileimportCal)){
            list_plot_cpg[[names(rv$fileimportCal)[i]]] <- names(rv$fileimportCal[[i]])[-1]
          }

          # only return list of CpG-sites for each locus, if there is already a selection of the locus in selectPlotLocus
          cpg_output <- reactive({
            if (!is.null(input_re()$selectPlotLocus)){
              return(list_plot_cpg[input_re()$selectPlotLocus])
            }
          })

          # always wrap selectInput into reactive-function
          selectPlotCpG <- reactive({
            selectInput(inputId="selectPlotType2", label = NULL, multiple = F, selectize = F, choices = cpg_output(), width = "50%")
          })

          # render second selectInput
          output$s2PlotOutput <- renderUI({
            s3 <- selectPlotCpG()
            s3
          })

          # create download button for each plot
          output$downloadPlots <- downloadHandler(
            filename = function(){paste0(gsub("[[:punct:]]", "", input_re()$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2), ".png")},
            content = function(file){
              file.copy(paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2), ".png"), file)
            },
            contentType = "image/png"
          )

          # render Plot UI
          output$selectPlotInput <- renderUI({
            s1 <- selectPlotLocus()
            s2 <- uiOutput("modulePlotting-s2PlotOutput")
            b <- downloadButton("modulePlotting-downloadPlots", "Download Plot")
            do.call(tagList, list(s1, s2, b))
          })

          # render plot from local temporary file
          output$plots <- renderImage({
            filename <- paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2), ".png")
            # print(filename)
            # Return a list containing the filename
            list(src = filename)
          }, deleteFile = FALSE)
    }
  })
}

modulePlottingUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Regression Plot",
        imageOutput(ns("plots")),
        width=8
      ),
      box(
        title = "Plot Selection",
        uiOutput(ns("selectPlotInput")),
        width = 4)
    )
  )
}