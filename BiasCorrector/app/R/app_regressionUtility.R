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
