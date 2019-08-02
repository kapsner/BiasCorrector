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

moduleInfoServer <- function(input, output, session, rv, input_re){
  output$citation_correction <- renderPrint({
    citation("PCRBiasCorrection")
  })
  output$citation_corrector <- renderPrint({
    citation("PCRBiasCorrection")
  })
}

moduleInfoUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Info",
        tabsetPanel(
          tabPanel(title = "Citation",
                   h5("If you use this 'BiasCorrector' or 'PCRBiasCorrection' to correct PCR measurement biases for a publication, please cite these packages as follows:"),
                   h5(tags$b("PCRBiasCorrection:")),
                   verbatimTextOutput(ns("citation_correction")),
                   h5(tags$b("BiasCorrector:")),
                   verbatimTextOutput(ns("citation_corrector"))
          )
        ),
        width = 12
      )
    )
  )
}