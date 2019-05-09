# give back deleted rows
omitnasModal <- function(omitnas, data_type){
  if (data_type == "experimental"){
    data_type <- "experimental data"
  } else {
    data_type <- "calibration data"
  }
  if (omitnas == 1){
    message = paste0("Deleted 1 row containing missing values from ", data_type, ".")
  } else {
    message = paste0("Deleted ", omitnas, " rows containing missing values from ", data_type, ".")
  }
  # show modal here
  showModal(modalDialog(
    message,
    title = "Missing values deleted"
  ))
  invisible(gc())
}
