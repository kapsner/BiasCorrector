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

# define UI
shiny::shinyUI(shiny::tagList(
  # https://github.com/rstudio/shinydashboard/issues/255
  shiny::tags$head(shiny::tags$style(
    shiny::HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
  )),
  
  shinydashboard::dashboardPage(skin = "black",
                                                          
                                             # App title --> change for development
                                             shinydashboard::dashboardHeader(title = "BiasCorrector"),
                                             # h5("based on Moskalev et al. 2011"),
                                             
                                             # Sidebar Layout with input and output definitions
                                             shinydashboard::dashboardSidebar(
                                               # shinyjs stuff
                                               shinyjs::useShinyjs(), # Include shinyjs in the UI
                                               
                                               #Sidebar Panel
                                               shinydashboard::sidebarMenu(id = "tabs",
                                                                           shinydashboard::menuItem("File Upload", tabName = "dashboard", icon = icon("file")),
                                                                           shinydashboard::sidebarMenuOutput("menu"),
                                                                           shinydashboard::menuItem("Log", tabName = "panel_9", icon = icon("file-alt")),
                                                                           shiny::tags$hr(),
                                                                           shinydashboard::menuItem("Settings", tabName = "settings", icon = icon("user-cog")),
                                                                           shinydashboard::menuItem("Info", tabName = "info", icon = icon("info-circle")),
                                                                           shiny::actionButton("reset", "Reset App", width = "80%") # Restart session
                                               ),
                                               shiny::div(style = "position:fixed; bottom:0; left:0; white-space: normal; text-align:left;
                                                                              padding: 9.5px 9.5px 9.5px 9.5px; margin: 6px 10px 6px 10px; box-sizing:border-box; heigth: auto; width: 230px;",
                                                          shiny::HTML("\u00A9 Lorenz A. Kapsner</i>"))
                                             ),
                                             
                                             shinydashboard::dashboardBody(
                                               # shinyjs stuff
                                               shinyjs::useShinyjs(), # Include shinyjs in the UI
                                               # https://stackoverflow.com/questions/25062422/restart-shiny-session
                                               shinyjs::extendShinyjs(script = "reset.js", functions = "reset"), # Add the js code to the page
                                               
                                               shinydashboard::tabItems(
                                                 shinydashboard::tabItem(tabName = "dashboard",
                                                                         moduleFileuploadUI("moduleFileupload")
                                                 ),
                                                 
                                                 # experimental data panels
                                                 shinydashboard::tabItem(tabName = "panel_1",
                                                                         moduleExperimentalFileUI("moduleExperimentalFile")
                                                 ),
                                                 
                                                 # calibration data panel
                                                 shinydashboard::tabItem(tabName = "panel_2",
                                                                         moduleCalibrationFileUI("moduleCalibrationFile")
                                                 ),
                                                 
                                                 # regression plots
                                                 shinydashboard::tabItem(tabName = "panel_3",
                                                                         modulePlottingUI("modulePlotting")
                                                 ),
                                                 
                                                 # regression statistics
                                                 shinydashboard::tabItem(tabName = "panel_4",
                                                                         moduleStatisticsUI("moduleStatistics")
                                                 ),
                                                 
                                                 # select regression model
                                                 shinydashboard::tabItem(tabName = "panel_5",
                                                                         moduleModelSelectionUI("moduleModelSelection")
                                                 ),
                                                 
                                                 # select regression model
                                                 shinydashboard::tabItem(tabName = "panel_6",
                                                                         moduleResultsUI("moduleResults")
                                                 ),
                                                 
                                                 # select regression model
                                                 shinydashboard::tabItem(tabName = "panel_7",
                                                                         moduleCorrectedPlotsUI("moduleCorrectedPlots")
                                                 ),
                                                 
                                                 # select regression model
                                                 shinydashboard::tabItem(tabName = "panel_8",
                                                                         moduleCorrectedStatisticsUI("moduleCorrectedStatistics")
                                                 ),
                                                 
                                                 shinydashboard::tabItem(tabName = "panel_9",
                                                                         moduleLogUI("moduleLog")
                                                 ),
                                                 
                                                 shinydashboard::tabItem(tabName = "settings",
                                                                         moduleSettingsUI("moduleSettings")
                                                 ),
                                                 
                                                 shinydashboard::tabItem(tabName = "info",
                                                                         moduleInfoUI("moduleInfo")
                                                 )
                                               )
                                             )
)))
