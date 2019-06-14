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

# open modal help function -> handling reactive values
openModal <- function(description, rv){
  rv$modal_closed <- F
  rv$modal_type <- description
  requirementsError(description)
}

onStart <- function(plotdir, csvdir, logfilename){
  
  if (dir.exists(plotdir)){
    cleanUp(plotdir, csvdir)
  }
  
  # create directories
  dir.create(plotdir)
  dir.create(csvdir)
  
  # initialize logfile here
  suppressMessages(suppressWarnings(file.create(logfilename)))
}

cleanUp <- function(plotdir, csvdir){
  # on session end, remove plots and and all other files from tempdir
  do.call(file.remove, list(list.files(plotdir, full.names = TRUE)))
  unlink(plotdir, recursive = T)
  do.call(file.remove, list(list.files(csvdir, full.names = TRUE)))
  unlink(csvdir, recursive = T)
}
