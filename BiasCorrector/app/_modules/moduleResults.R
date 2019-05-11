moduleResultsServer <- function(input, output, session, rv, input_re){
  
  observe({
    req(rv$calculate_results)
    
    if (rv$calculate_results){
      cat("\nCalculate results\n")
      
      if (rv$type_locus_sample == "1"){
        
        rv$choices_list <- data.table("Name" = character(), "better_model" = numeric())
        lapply(1:length(rv$vec_cal), function(x) {
          radioname <- paste0("radio", x)
          rv$choices_list <- rbind(rv$choices_list, cbind("Name" = rv$vec_cal[x], "better_model" = as.numeric(eval(parse(text=paste0("input_re()$", radioname))))))
        })
        print(rv$choices_list)
        
        
        substitutions_create(rv)
        # calculating final results
        withProgress(message = "BiasCorrecting experimental data", value = 0, {
          incProgress(1/1, detail = "... working on BiasCorrection ...")
          
          # Experimental data 
          rv$finalResults <- solving_equations(rv$fileimportExp, rv$choices_list, type = 1, rv = rv)
          
          # Calibration Data (to show corrected calibration curves)
          rv$fileimportCal_corrected <- solving_equations(rv$fileimportCal, rv$choices_list, type = 1, rv = rv, mode = "corrected")
          colnames(rv$fileimportCal_corrected) <- colnames(rv$fileimportCal)
        })
        
        
      } else if (rv$type_locus_sample == "2"){
        
        # initialize temp results
        rv$temp_results <- list()
        
        substitutions_create(rv)
        # iterate over unique names in locus_id of experimental file (to correctly display
        # decreasing order of CpG-sites in final results)
        
        # calculating final results
        withProgress(message = "BiasCorrecting experimental data", value = 0, {
          incProgress(1/1, detail = "... working on BiasCorrection ...")
          
          # Experimental data
          # iterate over unique locus ids in experimental file
          for (b in rv$fileimportExp[,unique(locus_id)]){
            # get regression results
            rv$result_list <- rv$result_list_type2[[b]]
            # get copy of experimental data for that specific locus
            expdata <- rv$fileimportExp[locus_id==b,]
            # get colnames of that specific locus (different loci can have different numbers of CpG-sites)
            vec <- c("locus_id", colnames(expdata)[2:(expdata[,min(CpG_count)]+1)], "rowmeans")
            # solve equations for that locus and append temp_results
            rv$temp_results[[b]] <- solving_equations(expdata[,vec,with=F], rv$regStats[[b]][,.(Name, better_model)], type = 2, rv = rv)
          }
          
          # iterate over temp_results (key=locus-name) and iteratively append final results
          for (i in names(rv$temp_results)){
            if (is.null(rv$finalResults)){
              rv$finalResults <- rv$temp_results[[i]]
            } else {
              # set use.names = T and fill = T because, as pointed out before, different loci can have different numbers of CpG sites and!!
              # the best fitting algorithm can be cubic or hyperbolic for the same CpG site-number of different loci
              rv$finalResults <- rbind(rv$finalResults, rv$temp_results[[i]], use.names = T, fill = T)
            }
          }
          vec <- colnames(rv$finalResults)[grepl("rowmeans", colnames(rv$finalResults))]
          # reorder the columns so that the rownames are at the end!
          rv$finalResults <- cbind(rv$finalResults[,-vec, with=F], rv$finalResults[,vec,with=F], CpG_sites = unique(rv$fileimportExp[,CpG_count,by=locus_id])$CpG_count)
          
          # TODO 
          # Calibration Data (to show corrected calibration curves)
          # initialize calibration results list
          rv$fileimportCal_corrected <- list()
          # iterate over fileimportCal (in type 2 data, this is a list with one calibrationdata data.table for each locus)
          for (a in names(rv$fileimportCal)){
            # get unique elements of true_methylation for one specific locus (we are treating them here as if they were sample ids)
            for (b in rv$fileimportCal[[a]][,unique(true_methylation)]){
              # get the regression parameters of that locus (locusname is saved in "a")
              rv$result_list <- rv$result_list_type2[[a]]
              # get subset of the calibration data of that methylation step
              caldata <- rv$fileimportCal[[a]][true_methylation==b,]
              nc <- ncol(caldata)
              vec <- c("true_methylation", colnames(caldata)[2:(nc-1)], "rowmeans")
              # solve equation for that calibrationstep
              # save result of each calibrationstep in tmp object
              tmp <- solving_equations(caldata[,vec,with=F], rv$regStats[[a]][,.(Name, better_model)], type = 2, rv = rv, mode = "corrected")
              # imediatelly rename columnames
              colnames(tmp) <- vec
              # if new calibration step is saved for the first time
              if (is.null(rv$fileimportCal_corrected[[a]])){
                rv$fileimportCal_corrected[[a]] <- tmp
              } else {
                # we should not need fill, since there should be no differences in colnames for one file
                rv$fileimportCal_corrected[[a]] <- rbind(rv$fileimportCal_corrected[[a]], tmp, use.names=T, fill=F)
              }
            }
          }
        })
      }
      
      output$dtfinal <- DT::renderDataTable({
        # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
        DT::datatable(rv$finalResults, options = list(scrollX = TRUE, pageLength = 20)) %>%
          formatRound(columns=c(2:ncol(rv$finalResults)), digits=3)
      })
      
      # show corrected results for experimental data
      output$corrected_data <- renderUI({
        dt <- dataTableOutput("moduleResults-dtfinal")
        db <- div(class="row", style="text-align: center", downloadButton("moduleResults-downloadFinal", "Download corrected values"))
        dball <- div(class="row", style="text-align: center", downloadButton("moduleResults-downloadAllData", "Download zip archive (tables and plots)"))
        do.call(tagList, list(dt, tags$hr(), db, tags$hr(), dball))
      })
      
      # Download corrected results
      output$downloadFinal <- downloadHandler(
        
        filename = function(){
          paste0("BC_corrected_values_", rv$sampleLocusName, "_", getTimestamp(), ".csv")
        },
        content = function(file){
          writeCSV(rv$finalResults, file)
        },
        contentType = "text/csv"
      )
      
      
      output$downloadAllData <- downloadHandler(
        filename = paste0("BC_all_results_", rv$sampleLocusName, "_", gsub("\\-", "", substr(Sys.time(), 1, 10)), "_", 
                          gsub("\\:", "", substr(Sys.time(), 12, 16)), ".zip"),
        content = function(fname) {
          print(getwd())
          
          # temporarily set tempdir as wd
          oldwd <- getwd()
          setwd(tempdir())
          print(getwd())
          
          # create files where is no difference in export between type 1 and 2
          writeCSV(rv$fileimportExp, paste0(csvdir, "raw_experimental_data.csv"))
          writeCSV(rv$finalResults, paste0(csvdir, "BC_corrected_values.csv"))
          writeCSV(rv$substitutions, paste0(csvdir, "BC_substituted_values.csv"))
          
          # create other files
          if (rv$type_locus_sample == "1"){
            writeCSV(rv$fileimportCal, paste0(csvdir, "raw_calibration_data.csv"))
            writeCSV(rv$regStats[,-12, with=F], paste0(csvdir, "BC_regression_stats.csv"))
            
          } else if (rv$type_locus_sample == "2"){
            # regression stats
            for (key in names(rv$fileimportCal)){
              writeCSV(rv$regStats[[key]][,-12, with=F],
                       paste0(csvdir, "BC_regression_stats_", gsub("[[:punct:]]", "", key), ".csv"))
            }
            
            # raw calibrations data
            for (key in names(rv$fileimportCal)){
              writeCSV(rv$fileimportCal[[key]],
                       paste0(csvdir, "raw_calibration_data_", gsub("[[:punct:]]", "", key), ".csv"))
            }
          }
          
          
          print(list.files(".csv/"))
          print(list.files(".plots/"))
          
          zip(zipfile=fname, files=c(paste0("csv/", list.files(".csv/")), 
                                     paste0("plots/", list.files(".plots/"))
          ))
          
          if(file.exists(paste0(tempdir(), "/", fname, ".zip"))){
            file.rename(paste0(tempdir(), "/", fname, ".zip"), fname)
          }
          
          # return to old wd
          setwd(oldwd)
          print(getwd())
        },
        contentType = "application/zip"
      )
      
      # present substitutions in extra tab (only if there were some)
      if (nrow(rv$substitutions) > 0){
        rv$substitutionsCalc <- TRUE
      }
      
      rv$calculate_results <- FALSE
    }
  })
  
  # Presentation of substituted values
  observe({
    req(rv$substitutionsCalc)
    
    # this workaround is related to this issue:
    # TODO issue: https://github.com/rstudio/shiny/issues/2116
    output$substitutedOut <- renderUI({
      h <- div(class="row", style="text-align: center",
               h4("Substituted values"))
      t <- dataTableOutput("moduleResults-substituted_values")
      b <- div(class="row", style="text-align: center", downloadButton("moduleResults-downloadSubstituted", "Download substituted values"))
      do.call(tagList, list(h, tags$hr(), t, b, tags$hr()))
    })
    # change colnames for better display
    colnames(rv$substitutions) <- c("Sample ID", "CpG site", "Corrected value", "Substituted value")
    
    
    output$downloadSubstituted <- downloadHandler(
      
      filename = function(){
        paste0("BC_substituted_values_", rv$sampleLocusName, "_", getTimestamp(), ".csv")
      },
      content = function(file){
        writeCSV(rv$substitutions, file)
      },
      contentType = "text/csv"
    )
    
    output$substituted_values <- DT::renderDataTable({
      DT::datatable(rv$substitutions, options = list(scrollX = TRUE, pageLength = 20)) %>%
        formatRound(columns=c(2:ncol(rv$fileimportExp)), digits=3)
    })
    
    #msg2 <- "Please refer to the tab 'Substituted values' for further information."
    msg2 <- "Please scroll down to the section 'Substituted values' for further information."
    if (nrow(rv$substitutions) == 1){
      msg1 <- "Substituted 1 value. "
    } else{
      msg1 <- paste0("Substituted ", nrow(rv$substitutions), " values.")
    }
    
    # show modal here
    showModal(modalDialog(
      paste(msg1, msg2),
      title = "Substituted values"
    ))
  })
}


moduleResultsUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "BiasCorrected Results",
        div(class="row", style="margin: 0.5%"),
        uiOutput(ns("corrected_data")),
        tags$hr(),
        uiOutput(ns("substitutedOut")),
        tags$hr(),
        width = 12
      ))
  )
}