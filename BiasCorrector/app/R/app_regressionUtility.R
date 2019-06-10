# regressionUtility
regressionUtility <- function(data, samplelocusname, locus_id = NULL, rv, mode = NULL, headless = FALSE){
  
  if (!is.null(locus_id)){
    writeLog(paste0("### Starting with regression calculations ###\n\nLocus ID: ", locus_id))
  } else {
    writeLog(paste0("### Starting with regression calculations ###"))
  }
  
  
  # workaround to hide shiny-stuff, when going headless
  if (isFALSE(headless)){
    # for plotting: basic idea and some code snippets from:
    # https://gist.github.com/wch/5436415/
    regression <- reactive({
      regression_type1(data, rv$vec_cal, mode)
    })
    
    withProgress(message = "Calculating calibration curves", value = 0, {
      incProgress(1/1, detail = "... working on calculations ...")
      # calculate results (if this is run here, j must be resetted)
      regression_results <- regression()
    })
  } else {
    regression_results <- regression_type1(data, rv$vec_cal, mode)
  }
  
  return(list("plot_list" = regression_results[["plot_list"]],
              "result_list" = regression_results[["result_list"]]))
}
