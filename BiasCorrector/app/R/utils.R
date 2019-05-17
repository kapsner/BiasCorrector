# handle user text inputs
handleTextInput <- function(textinput){
  textinput <- gsub("[^[:alnum:]]", "", textinput)
  
  # max 15 chars:
  if (nchar(textinput) > 15){
    textinput <- substr(textinput, 1, 15)
  }
  
  return(textinput)
}

# get timestamp
getTimestamp <- function(){
  return(paste(gsub("\\-", "", substr(Sys.time(), 1, 10)), gsub("\\:", "", substr(Sys.time(), 12, 20)), sep="_"))
}

# write log messages
writeLog <- function(message){
  print(paste0("[", getTimestamp(), "]: ", message))
  message_out <- paste0("===========================================  \n",
                        "[Timestamp: ", getTimestamp(), "]  \n  \n",
                        message, "  \n  \n")
  write(message_out, file = logfilename, append = T)
}

# write csv files
writeCSV <- function(table, filename){
  return(fwrite(x = table, 
                file = filename, 
                row.names = F, 
                sep = ",", 
                dec = ".", 
                eol = "\n"))
}

# create substitutions dataframe
substitutions_create <- function(rv){
  rv$substitutions <- data.table(id = character(), 
                                 CpG_site = character(),
                                 corrected = character(),
                                 replacement = character())
}
