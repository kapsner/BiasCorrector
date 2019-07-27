# BiasCorrector: Correct PCR-bias in DNA methylation analyses
# Copyright (C) 2019 Lorenz Kapsner
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

moduleResultsServer <- function(input, output, session, rv, input_re){
  
  observe({
    req(rv$calculate_results)
    
    if (rv$calculate_results){
      cat("\nCalculate results\n")
      
      if (rv$type_locus_sample == "1"){
        
        rv$choices_list <- data.table::data.table("Name" = character(), "better_model" = numeric())
        lapply(1:length(rv$vec_cal), function(x) {
          radioname <- paste0("radio", x)
          rv$choices_list <- rbind(rv$choices_list, cbind("Name" = rv$vec_cal[x], "better_model" = as.numeric(eval(parse(text=paste0("input_re()$", radioname))))))
        })
        print(rv$choices_list)
        
        # calculating final results
        withProgress(message = "BiasCorrecting experimental data", value = 0, {
          incProgress(1/1, detail = "... working on BiasCorrection ...")
          
          # Experimental data 
          solved_eq <- PCRBiasCorrection::solvingEquations_(rv$fileimportExp, rv$choices_list, type = 1, rv = rv, logfilename = logfilename, minmax = rv$minmax)
          rv$finalResults <- solved_eq[["results"]]
          rv$substitutions <- solved_eq[["substitutions"]]
        })
        
        
      } else if (rv$type_locus_sample == "2"){
        
        # initialize temp results
        rv$temp_results <- list()
        
        rv$substitutions <- PCRBiasCorrection::substitutionsCreate_()
        
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
            vec <- c("locus_id", colnames(expdata)[2:(expdata[,min(CpG_count)]+1)], "row_means")
            # solve equations for that locus and append temp_results
            solved_eq <- PCRBiasCorrection::solvingEquations_(expdata[,vec,with=F], rv$regStats[[b]][,.(Name, better_model)], type = 2, rv = rv, logfilename = logfilename, minmax = rv$minmax)
            rv$temp_results[[b]] <- solved_eq[["results"]]
            rv$substitutions <- rbind(rv$substitutions, solved_eq[["substitutions"]])
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
          vec <- colnames(rv$finalResults)[grepl("row_means", colnames(rv$finalResults))]
          # reorder the columns so that the rownames are at the end!
          rv$finalResults <- cbind(rv$finalResults[,-vec, with=F], rv$finalResults[,vec,with=F], CpG_sites = unique(rv$fileimportExp[,CpG_count,by=locus_id])$CpG_count)
        })
      }
      
      output$dtfinal <- DT::renderDataTable({
        # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
        DT::datatable(rv$finalResults, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
          formatRound(columns=c(2:ncol(rv$finalResults)), digits=3)
      })
      
      # show corrected results for experimental data
      output$corrected_data <- renderUI({
        dt <- dataTableOutput("moduleResults-dtfinal")
        
        do.call(tagList, list(dt))
      })
      
      # Download corrected results
      output$downloadFinal <- downloadHandler(
        
        filename = function(){
          paste0("BC_corrected_values_", rv$sampleLocusName, "_", PCRBiasCorrection::getTimestamp_(), ".csv")
        },
        content = function(file){
          PCRBiasCorrection::writeCSV_(rv$finalResults, file)
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
          PCRBiasCorrection::writeCSV_(rv$fileimportExp, paste0(csvdir, "raw_experimental_data.csv"))
          PCRBiasCorrection::writeCSV_(rv$finalResults, paste0(csvdir, "BC_corrected_values.csv"))
          PCRBiasCorrection::writeCSV_(rv$substitutions, paste0(csvdir, "BC_substituted_values.csv"))
          write(rv$logfile, paste0(csvdir, "BC_logfile.txt"))
          
          # create other files
          if (rv$type_locus_sample == "1"){
            PCRBiasCorrection::writeCSV_(rv$fileimportCal, paste0(csvdir, "raw_calibration_data.csv"))
            PCRBiasCorrection::writeCSV_(rv$regStats[,-(which(colnames(rv$regStats)=="better_model")), with=F], paste0(csvdir, "BC_regression_stats.csv"))
            PCRBiasCorrection::writeCSV_(rv$regStats_corrected[,-(which(colnames(rv$regStats_corrected)=="better_model")), with=F], paste0(csvdir, "BC_regression_stats_corrected.csv"))
            
          } else if (rv$type_locus_sample == "2"){
            # regression stats
            for (key in names(rv$fileimportCal)){
              PCRBiasCorrection::writeCSV_(rv$regStats[[key]][,-(which(colnames(rv$regStats[[key]])=="better_model")), with=F],
                       paste0(csvdir, "BC_regression_stats_", gsub("[[:punct:]]", "", key), ".csv"))
            }
            
            for (key in names(rv$fileimportCal_corrected)){
              PCRBiasCorrection::writeCSV_(rv$regStats_corrected[[key]][,-(which(colnames(rv$regStats_corrected[[key]])=="better_model")), with=F],
                       paste0(csvdir, "BC_regression_stats_corrected_", gsub("[[:punct:]]", "", key), ".csv"))
            }
            
            # raw calibrations data
            for (key in names(rv$fileimportCal)){
              PCRBiasCorrection::writeCSV_(rv$fileimportCal[[key]],
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
        # workaround to tell ui, that experimental file is there
        output$gotSubstitutions <- reactive({
          return(TRUE)
        })
        outputOptions(output, 'gotSubstitutions', suspendWhenHidden=FALSE)
      }
      
      output$description <- renderText({
        str1 <- "The results table shows the BiasCorrected experimental data."
        str2 <- "Column 1 shows the sample ID (type 1 data) or the locus ID (type 2 data)."
        str3 <- "All other columns represent the BiasCorrected experimental data for the CpG-sites and the row-means of all CpG-sites respectively."
        str4 <- "The suffixes '_h' and '_c' in the column names indicate the regression algorithm used for BiasCorrection of the respective CpG-site ('_h': hyperbolic regression; '_c': cubic regression)."
        HTML(paste(str1, str2, str3, str4, sep = "<br/><br/>"))
      })
      
      rv$calculate_results <- FALSE
    }
  })
  
  # Presentation of substituted values
  observe({
    req(rv$substitutionsCalc)
    
    output$description_sub <- renderText({
      str1 <- "Substitutions occur, when no result is found in the range of plausible values between 0 and 100 during the BiasCorrection."
      str2 <- "A 'border zone' is implemented in the ranges 0 – 10% and 100 + 10%."
      str3 <- "If a result is in the range -10 < x < 0 percentage or 100  < x < 110 percentage, the value is substituted in the final results with 0 percentage or 100 percentage respectively."
      str4 <- "Values beyond these border zones will be substituted with a blank value in the final output, as they seem implausible and could indicate substantial errors in the underlying data."
      str5 <- "For a detailed feedback, the substitutions table shows the results of the algorithm 'BiasCorrected value' and the corresponding substitution 'Substituted value' for the respective CpG-site."
      
      HTML(paste(str1, str2, str3, str4, str5, sep = "<br/><br/>"))
    })
    
    # this workaround is related to this issue:
    # TODO issue: https://github.com/rstudio/shiny/issues/2116
    output$substitutedOut <- renderUI({
      t <- dataTableOutput("moduleResults-substituted_values")
      do.call(tagList, list(t))
    })
    # change colnames for better display
    colnames(rv$substitutions) <- c("Sample ID", "CpG site", "BiasCorrected value", "Substituted value")
    
    
    output$downloadSubstituted <- downloadHandler(
      
      filename = function(){
        paste0("BC_substituted_values_", rv$sampleLocusName, "_", PCRBiasCorrection::getTimestamp_(), ".csv")
      },
      content = function(file){
        PCRBiasCorrection::writeCSV_(rv$substitutions, file)
      },
      contentType = "text/csv"
    )
    
    output$substituted_values <- DT::renderDataTable({
      DT::datatable(rv$substitutions, options = list(scrollX = TRUE, pageLength = 20, dom="ltip"), rownames = F) %>%
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
      column(9,
             box(title = "BiasCorrected Results",
                 uiOutput(ns("corrected_data")),
                 width = 12
             )),
      column(3,
             box(title = "Download BiasCorrected Results",
                 div(class="row", style="text-align: center", downloadButton("moduleResults-downloadFinal", "Download corrected values", style="white-space: normal; text-align:center; 
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
                 tags$hr(),
                 div(class="row", style="text-align: center", downloadButton("moduleResults-downloadAllData", "Download zip archive (tables and plots)", style="white-space: normal; text-align:center; 
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
                 tags$hr(),
                 width = 12
             ),
             box(title = "Description",
                 htmlOutput(ns("description")),
                 width = 12
             )
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = "output['moduleResults-gotSubstitutions']",
        column(9,
               box(title = "Substituted values",
                   uiOutput(ns("substitutedOut")),
                   width = 12
               )),
        column(3,
               box(title = "Download Substitutions",
                   div(class="row", style="text-align: center", downloadButton("moduleResults-downloadSubstituted", "Download substituted values", style="white-space: normal; text-align:center; 
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;")),
                   tags$hr(),
                   width = 12
               ),
               box(title = "What are 'substitutions'?",
                   htmlOutput(ns("description_sub")),
                   width = 12
               )
        )
      )
      
    )
  )
}