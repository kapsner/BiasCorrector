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

moduleSettingsServer <- function(input, output, session, rv, input_re){
  
  # observe Radiobuttonevents 
  observeEvent(input_re()[["moduleSettings-settings_minmax"]], {
    PCRBiasCorrection::writeLog_(paste0("Settings: minmax = ", input_re()[["moduleSettings-settings_minmax"]]), logfilename)
    rv$minmax <- input_re()[["moduleSettings-settings_minmax"]]
  })
}

moduleSettingsUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # type of data box
      box(
        title = "Settings",
        checkboxInput(ns("settings_minmax"), label = "Use min-max (very experimental)", value = FALSE),
        width = 9
      ),
      box(
        title = "Description",
        width = 3
      )
    )
  )
}