headless <- function(experimental, calibration, type=1, csvdir="./csvdir", plotdir="./plotdir", logfilename="./log.txt"){
  # fix directories to work with all functions
  # therefore we need a "/" at the end of the dir-string
  plotdir <- gsub("([[:alnum:]])$", "\\1/", plotdir)
  csvdir <- gsub("([[:alnum:]])$", "\\1/", csvdir)
  
  # initialize some stuff
  onStart(plotdir, csvdir, logfilename)
  
  
  # initialize our list for reactive values
  rv <- list()
  
  # load data
  if (type == 1){
    # experimental data
    rv$fileimportExp <- cleanDT(fread(experimental, header = T), "experimental", 1)[["dat"]]
    
    # calibration data
    cal_type_1 <- cleanDT(fread(calibration, header = T), "calibration", 1)
    rv$fileimportCal <- cal_type_1[["dat"]]
    rv$vec_cal <- cal_type_1[["vec_cal"]]
  } else if (type == 2){
    print("Not implemented yet")
  } else {
    return("ERROR. Please specify correct type of data to correct")
  }
  
  if (type == 1){
    # reconstruct parts from app_plottingUtility.R
    regression_results <- regressionUtility(rv$fileimportCal, "Testlocus", locus_id = NULL, rv = rv, mode = NULL, headless = TRUE)
    plotlistR <- regression_results[["plot_list"]]
    rv$result_list <- regression_results[["result_list"]]
    
    plottingUtility(rv$fileimportCal, plotlistR, 1, "Testlocus", locus_id = NULL, rv = rv, mode = NULL, headless = TRUE, plotdir = plotdir)
    
  }
  
  # save regression statistics to reactive value
  rv$regStats <- statisticsList(rv$result_list)

  # calculate final results
  # default rv$choices_list == rv$regStats[,.(Name, better_model)]
  solved_eq <- solving_equations(rv$fileimportExp, rv$regStats[,c("Name", "better_model"),with=F], type = 1, rv = rv)
  rv$finalResults <- solved_eq[["results"]]
  rv$substitutions <- solved_eq[["substitutions"]]

  # Calibration Data (to show corrected calibration curves)
  solved_eq2 <- solving_equations(rv$fileimportCal, rv$regStats[,c("Name", "better_model"),with=F], type = 1, rv = rv, mode = "corrected")
  rv$fileimportCal_corrected <- solved_eq2[["results"]]
  colnames(rv$fileimportCal_corrected) <- colnames(rv$fileimportCal)
  
  return(TRUE)
}
