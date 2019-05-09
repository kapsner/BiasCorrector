# open modal help function -> handling reactive values
openModal <- function(description, rv){
  rv$modal_closed <- F
  rv$modal_type <- description
  requirementsError(description)
}

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
