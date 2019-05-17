moduleCorrectedPlotsServer <- function(input, output, session, rv, input_re){
  observe({
    # this is needed, to start plotting, when we have the bias corrected calibration values!
    req(rv$fileimportCal_corrected)
    
      # type 1 data: 
      if (rv$type_locus_sample == "1"){
        if (isFALSE(rv$corrected_finished)){
          
          withProgress(message = "Plotting BiasCorrected results", value = 0, {
            incProgress(1/1, detail = "... working hard ...")
            plottingUtility(rv$fileimportCal_corrected, type=1, samplelocusname=rv$sampleLocusName, rv=rv, mode="corrected")
          
            # save regression statistics to reactive value
            rv$regStats_corrected <- statisticsList(rv$result_list)
            
            for (i in rv$choices_list[,Name]){
              rv$regStats_corrected[Name==i,better_model:=rv$choices_list[Name==i,as.integer(as.character(better_model))]]
            }
          
            createBarErrorPlots(rv$regStats, rv$regStats_corrected, rv, type=1)
          })
          
          # when finished
          rv$corrected_finished <- TRUE
          writeLog("Finished plotting corrected")
        }
        
        # else if type 2 data
      } else if (rv$type_locus_sample == "2"){
        
        if (isFALSE(rv$corrected_finished)){
          a <- 1
          rv$result_list_type2_corrected <- list()

          withProgress(message = "Plotting BiasCorrected results", value = 0, {
            incProgress(1/1, detail = "... working hard ...")

            for (b in names(rv$fileimportCal_corrected)){
              rv$vec_cal <- names(rv$fileimportCal_corrected[[a]])[-1]
              #print(paste("Length rv$vec_cal:", length(rv$vec_cal)))

              plottingUtility(rv$fileimportCal_corrected[[a]], type=2, samplelocusname=rv$sampleLocusName, b=gsub("[[:punct:]]", "", b), rv=rv, mode="corrected")

              # save regression statistics to reactive value
              rv$regStats_corrected[[b]] <- statisticsList(rv$result_list)
              rv$result_list_type2_corrected[[b]] <- rv$result_list

              # create barplots
              createBarErrorPlots(rv$regStats[[b]], rv$regStats_corrected[[b]], rv, type=2, b=b)

              a <- a + 1
            }
          })
          # on finished
          rv$corrected_finished <- TRUE
          writeLog("Finished plotting corrected")
        }
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
        selectInput(inputId="selectPlot_corrected", label = "Select CpG-site:", multiple = F, selectize = F, choices = plot_output_list)
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
        filename = function(){paste0(rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_errorplot.png")},
        content = function(file){
          file.copy(paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_errorplot.png"), file)
        },
        contentType = "image/png"
      )
      
      # render head of page with selectInput and downloadbutton
      output$selectPlotInput_corrected <- renderUI({
        s <- selIn2()
        b <- div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlots_corrected", "Download Corrected Plot"))
        c <- div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlotsSSE_corrected", "Download Error Plot"))
        do.call(tagList, list(s, tags$hr(), b, tags$hr(), c, tags$hr()))
      })
      
      # for debugging
      observeEvent(input_re()$selectPlot_corrected, {
        print(input_re()$selectPlot_corrected)
      })
      
      # render plots from local temporary file
      output$plots_corrected <- renderImage({
        #width  <- session$clientData[["output_moduleCorrectedPlots-plots_corrected_width"]]
        
        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_corrected.png")
        # Return a list containing the filename
        # list(src = filename,
        #      width = width)
        list(src = filename)
      }, deleteFile = FALSE)
      
      # render plots from local temporary file
      output$plotsSSE_corrected <- renderImage({
        #width  <- session$clientData[["output_moduleCorrectedPlots-plotsSSE_corrected_width"]]
        
        filename <- paste0(plotdir, rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlot_corrected), "_errorplot.png")
        # Return a list containing the filename
        # list(src = filename,
        #      width = width)
        list(src = filename)
      }, deleteFile = FALSE)
      
      
      
      ## regression statistics
      output$regression_statistics_corrected <- renderUI({
        output$dt_reg_corrected <- DT::renderDataTable({
          dt <- rv$regStats_corrected
          # use formatstyle to highlight lower SSE values
          renderRegressionStatisticTable(dt)
        })
        d <- DT::dataTableOutput("moduleCorrectedPlots-dt_reg_corrected")
        do.call(tagList, list(d))
      })
      
      # create download button for regression statistics
      output$downloadRegStat_corrected <- downloadHandler(
        filename = function(){
          paste0("BC_regression_stats_corrected_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_",
                 gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
        },
        content = function(file){
          writeCSV(rv$regStats_corrected[,-(which(colnames(rv$regStats_corrected)=="better_model")), with=F], file)
        },
        contentType = "text/csv"
      )
      
      
      
      
      # type 2 data:
    } else if (rv$type_locus_sample == "2"){
      
      #create list of loci to populate selectInput "selectPlotLocus"
          list_plot_locus <- list()
          for (i in 1:length(rv$fileimportCal_corrected)){
            list_plot_locus[[i]] <- names(rv$fileimportCal_corrected)[i]
          }

          selectPlotLocus <- reactive({
            selectInput(inputId="selectPlotLocus_corrected", label = "Select locus:", multiple = F, selectize = F, choices = list_plot_locus)
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
            selectInput(inputId="selectPlotType2_corrected", label = "Select CpG-site:", multiple = F, selectize = F, choices = cpg_output())
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
          
          output$downloadPlotsSSE_corrected <- downloadHandler(
            filename = function(){paste0(gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_errorplot.png")},
            content = function(file){
              file.copy(paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_errorplot.png"), file)
            },
            contentType = "image/png"
          )

          # render Plot UI
          output$selectPlotInput_corrected <- renderUI({
            s1 <- selectPlotLocus()
            s2 <- uiOutput("moduleCorrectedPlots-s2PlotOutput_corrected")
            b <-  div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlots_corrected", "Download Corrected Plot"))
            c <-  div(class="row", style="text-align: center", downloadButton("moduleCorrectedPlots-downloadPlotsSSE_corrected", "Download Error Plot"))
            do.call(tagList, list(s1, s2, tags$hr(), b, tags$hr(), c, tags$hr()))
          })

          # render plot from local temporary file
          output$plots_corrected <- renderImage({
            #width  <- session$clientData[["output_moduleCorrectedPlots-plots_corrected_width"]]
            
            filename <- paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_corrected.png")
            # print(filename)
            # Return a list containing the filename
            # list(src = filename,
            #      width = width)
            list(src = filename)
          }, deleteFile = FALSE)
          
          # render plots from local temporary file
          output$plotsSSE_corrected <- renderImage({
            #width  <- session$clientData[["output_moduleCorrectedPlots-plotsSSE_corrected_width"]]
            
            filename <- paste0(plotdir, gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "-", rv$sampleLocusName, "_", gsub("[[:punct:]]", "", input_re()$selectPlotType2_corrected), "_errorplot.png")
            # Return a list containing the filename
            # list(src = filename,
            #      width = width)
            list(src = filename)
          }, deleteFile = FALSE)
          
          
          
          ## regression statistics
          # create reactive df-selection:
          df_regs <- reactive({
            dt <- rv$regStats_corrected[[input_re()$selectPlotLocus_corrected]]
          })
          
          output$dt_regs_corrected <- DT::renderDataTable({
            dt <- df_regs()
            renderRegressionStatisticTable(dt)
          })
          
          # render head of page with selectInput and downloadbutton
          
          output$regression_statistics_corrected <- renderUI({
            dt <- DT::dataTableOutput("moduleCorrectedPlots-dt_regs_corrected")
            do.call(tagList, list(dt))
          })
          
          # create download button for regression statistics
          output$downloadRegStat_corrected <- downloadHandler(
            filename = function(){
              paste0("BC_regression_stats_corrected_", gsub("[[:punct:]]", "", input_re()$selectPlotLocus_corrected), "_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_",
                     gsub("\\:", "", substr(Sys.time(), 12, 16)), ".csv")
            },
            content = function(file){
              writeCSV(rv$regStats_corrected[[input_re()$selectPlotLocus_corrected]][,-(which(colnames(rv$regStats_corrected[[input_re()$selectPlotLocus_corrected]])=="better_model")), with=F], file)
            },
            contentType = "text/csv"
          )
    }
  })
}

moduleCorrectedPlotsUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(9,
             box(title = "BiasCorrected Regression Plot",
                 imageOutput(ns("plots_corrected")),
                 tags$head(tags$style(type="text/css", "#moduleCorrectedPlots-plots_corrected img {max-height: 100%; max-width: 100%; width: auto}")),
                 width=12
             ),
             box(title = "Efficiency of BiasCorrection",
                 imageOutput(ns("plotsSSE_corrected")),
                 tags$head(tags$style(type="text/css", "#moduleCorrectedPlots-plotsSSE_corrected img {max-height: 100%; max-width: 100%; width: auto}")),
                 width=12
             )
             
      ),
      column(3,
             box(title = "Plot Selection",
                 uiOutput(ns("selectPlotInput_corrected")),
                 width = 12
             )
      )
    ),
    
    fluidRow(
      column(9,
             box(title = "Regression Statistics [corrected]",
                 uiOutput(ns("regression_statistics_corrected")),
                 width = 12
             )),
      column(3,
             box(title = "Download Regression Statistics",
                 uiOutput(ns("statistics_select")),
                 div(class="row", style="text-align: center", downloadButton(ns("downloadRegStat_corrected"), "Download regression statistics")),
                 tags$hr(),
                 width = 12
             )
      )
    )
  )
}