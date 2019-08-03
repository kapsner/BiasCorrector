# BiasCorrector: A GUI to Correct PCR Bias in DNA Methylation Analyses
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

# requirements error + modal view
requirementsError <- function(data_type){

  footer_dis = actionButton("dismiss_modal",label = "Dismiss")
  title_filereq = "File requirements error!"

  if (data_type %in% c("experimentalFile", "calibrationFile")){
    title = title_filereq
    message = "The file provided does not meet the file requirements! Please upload a new file! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "locusname"){
    title = "No locus specified!"
    message = "Please specify an appropriate name for the gene or locus of your experiment!"
    footer = footer_dis
  } else if (data_type == "samplename"){
    title = "No sample name specified!"
    message = "Please specify an appropriate name of the sample of your experiment!"
    footer = footer_dis
  } else if (data_type == "csv"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "dim"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! All files have to have the same number of columns and rows. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "four"){
    title = title_filereq
    message = "Please upload at least 4 CSV files containing the calibration data. For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "naming"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! All files need to have the same rownames (locus ids) and columnnames (CpG sites). Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "filename"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! Filenaming of the calibration files must be done properly. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "calibrange"){
    title = title_filereq
    message = "The file provided does not meet the file requirements! Calibration steps must be in range '0 <= calibration step <= 100'. Please upload a new file! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "calibrange2"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! Calibration steps must be in range '0 <= calibration step <= 100'. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  } else if (data_type == "calibrange3"){
    title = title_filereq
    message = "The calibration steps provided do not meet the file requirements! Calibration steps must be in range '0 <= calibration step <= 100'. Each calibration step may only be assigned once. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = modalButton("OK")
  } else if (data_type == "inconsistency"){
    title = title_filereq
    message = "The files provided do not meet the file requirements! Please specify an equal number of CpG-sites for each gene locus. Please upload new files! For the specific CSV file requirements please refere to our FAQ."
    footer = footer_dis
  }
  shinyjs::logjs(message)
  # show modal here
  showModal(modalDialog(
    message,
    title = title,
    footer = footer
  ))
}
