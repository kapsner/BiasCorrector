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
substitutions_create <- function(){
  substitutions <- data.table(id = character(), 
                              CpG_site = character(),
                              corrected = character(),
                              replacement = character())
  return(substitutions)
}

# R-squared function
rsq <- function(true, fitted){
  return(cor(true, fitted) ^ 2)
}

sdm <- function(vector){
  I((vector-mean(vector))^2)
}
