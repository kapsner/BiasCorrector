% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moduleCalibrationFile.R
\name{module_calibrationfile_ui}
\alias{module_calibrationfile_ui}
\title{module_calibrationfile_ui}
\usage{
module_calibrationfile_ui(id)
}
\arguments{
\item{id}{A character. The identifier of the shiny object}
}
\value{
The function returns a shiny ui module.
}
\description{
module_calibrationfile_ui
}
\examples{
if (interactive()) {
shinydashboard::tabItems(
  shinydashboard::tabItem(
    tabName = "calibration",
    module_calibrationfile_ui(
      "moduleCalibrationFile"
    )
  )
)
}

}
\seealso{
\url{https://shiny.rstudio.com/articles/modules.html}
}
