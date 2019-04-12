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