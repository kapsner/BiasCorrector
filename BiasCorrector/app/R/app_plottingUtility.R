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

# plotting utility
plottingUtility <- function(data, plotlistR, type, samplelocusname, locus_id = NULL, rv, mode=NULL, headless=FALSE, plotdir){
  
  if (!is.null(locus_id)){
    writeLog(paste0("### Starting with plotting ###\n\nLocus ID: ", locus_id))
  } else {
    writeLog(paste0("### Starting with plotting ###"))
  }
  
  # get number of CpG-sites
  length_vector <- length(rv$vec_cal)
    
  Map(function(f) {
    plotname <- paste0(gsub("[[:punct:]]", "", rv$vec_cal[f]))
    
    # filename-suffix
    fn_suffix <- ifelse(is.null(mode), "", "_corrected")
    # message suffix
    msg_suffix <- ifelse(is.null(mode), "", "BiasCorrected ")

    # filname of temporary plot
    if (type == 1){
      filename <- paste0(plotdir, samplelocusname, "_", plotname, fn_suffix, ".png")
      plotmessage <- paste0("Creating ", msg_suffix, "plot No. ", f)
    } else if (type == 2){
      filename <- paste0(plotdir, locus_id, "-", samplelocusname, "_", plotname, fn_suffix, ".png")
      plotmessage <- paste0("Locus ID: ", locus_id, " --> Creating ", msg_suffix, "plot No. ", f)
    }
    
    writeLog(paste(plotmessage, "- filename:", filename))
    
    # workaround to hide shiny-stuff, when going headless
    if (isFALSE(headless)){
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = plotmessage, value = 0)
      
      # Increment the progress bar, and update the detail text.
      progress$inc(1/1, detail = paste("... working hard on plot", f, "of", length_vector))
    }
    
    # store plots to local temporary file
    createPlots(plotlistR[[f]], f, rv, filename)
    
  }, 1:length_vector)
}
