onStart <- function(){
  
  if (dir.exists(plotdir)){
    cleanUp()
  }
  
  # create directories
  dir.create(plotdir)
  dir.create(csvdir)
  
  # initialize logfile here
  suppressMessages(suppressWarnings(file.create(logfilename)))
}

cleanUp <- function(){
  # on session end, remove plots and and all other files from tempdir
  do.call(file.remove, list(list.files(plotdir, full.names = TRUE)))
  unlink(plotdir, recursive = T)
  do.call(file.remove, list(list.files(csvdir, full.names = TRUE)))
  unlink(csvdir, recursive = T)
}

# directories
tempdir <- tempdir()
plotdir <- paste0(tempdir, "/plots/")
csvdir <- paste0(tempdir, "/csv/")

# logfilename
logfilename <- paste0(tempdir, "/biascorrector.log")

# maximum filesize in MB
maxfilesize <- 100

# set shiny option here
options(shiny.maxRequestSize = maxfilesize*1024^2)


# include modules
source("./_modules/moduleFileupload.R", encoding = "UTF-8")
source("./_modules/moduleExperimentalFile.R", encoding = "UTF-8")
source("./_modules/moduleCalibrationFile.R", encoding = "UTF-8")
source("./_modules/modulePlotting.R", encoding = "UTF-8")
source("./_modules/moduleStatistics.R", encoding = "UTF-8")
source("./_modules/moduleModelSelection.R", encoding = "UTF-8")
source("./_modules/moduleResults.R", encoding = "UTF-8")

# include global functions
source("Functions.R", echo = F, encoding = "UTF-8")
source("App_Utilities.R", echo = F, encoding = "UTF-8")

openModal <- function(description, rv){
  rv$modal_closed <- F
  rv$modal_type <- description
  requirementsError(description)
}