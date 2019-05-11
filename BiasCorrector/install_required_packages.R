# required packages:
req_packages <- c("shiny", 
                  "shinyjs", 
                  "shinydashboard",
                  "DT", 
                  "data.table", 
                  "ggplot2", 
                  "magrittr",
                  "polynom",
                  "ggpubr",
                  "ggsci")

# check, if required packages are already installed, otherwise install them
vec <- setdiff(req_packages, installed.packages()[,"Package"])
if (length(vec) != 0){
  for (i in vec){
    cat("Installing required package: ", i, "\n\n")
    suppressMessages(install.packages(i, repos = "https://ftp.fau.de/cran/"))
  }
}
rm(vec, req_packages)
