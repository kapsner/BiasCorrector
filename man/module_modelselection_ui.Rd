% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moduleModelSelection.R
\name{module_modelselection_ui}
\alias{module_modelselection_ui}
\title{module_modelselection_ui}
\usage{
module_modelselection_ui(id)
}
\arguments{
\item{id}{A character. The identifier of the shiny object}
}
\value{
The function returns a shiny ui module.
}
\description{
module_modelselection_ui
}
\examples{
if (interactive()) {
shinydashboard::tabItems(
  shinydashboard::tabItem(
    tabName = "modelselection",
    module_modelselection_ui(
      "moduleModelSelection"
    )
  )
)
}

}
\seealso{
\url{https://shiny.rstudio.com/articles/modules.html}
}
