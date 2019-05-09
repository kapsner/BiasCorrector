# initialize resultslist and reset "j"
initializeListJ <- function(rv){
  # initialize result_list and j
  
  # save all goodness of fit statistics and for equation necessary parameters in list
  rv$result_list <- list()
  
  # "j" is necessary to get values for curve in equations
  rv$j <- 1
}
