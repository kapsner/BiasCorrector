# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
# Copyright (C) 2019-2022 Lorenz Kapsner
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
  shiny::tags$head(shiny::tags$style(shiny::HTML(
    paste0(
      ".wrapper {height: auto !important; ",
      "position:relative; overflow-x:hidden; ",
      "overflow-y:hidden}"
    )
  ))),

  shinydashboard::dashboardPage(
    skin = "black",

    # App title --> change for development
    shinydashboard::dashboardHeader(title = "BiasCorrector"),
    # h5("based on Moskalev et al. 2011"),

    # Sidebar Layout with input and output definitions
    shinydashboard::dashboardSidebar(
      # shinyjs stuff
      shinyjs::useShinyjs(),

      #Sidebar Panel
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("File Upload",
                                 tabName = "dashboard",
                                 icon = icon("file")),
        shinydashboard::sidebarMenuOutput("menu"),
        shinydashboard::menuItem("Log",
                                 tabName = "panel_9",
                                 icon = icon("file-alt")),
        shiny::tags$hr(),
        shinydashboard::menuItem("Settings",
                                 tabName = "settings",
                                 icon = icon("user-cog")),
        shinydashboard::menuItem("Info",
                                 tabName = "info",
                                 icon = icon("info-circle")),
        shiny::actionButton("reset",
                            "Reset App",
                            width = "80%") # Restart session
      ),
      shiny::div(
        style = paste0(
          "position:fixed; bottom:0; left:0; ",
          "white-space: normal; text-align:left; ",
          "padding: 9.5px 9.5px 9.5px 9.5px; ",
          "margin: 6px 10px 6px 10px; ",
          "box-sizing:border-box; heigth: auto; ",
          "width: 230px;"
        ),
        shiny::HTML(paste0(
          "Version:",
          "<br/>rBiasCorrection: ", utils::packageVersion("rBiasCorrection"),
          "<br/>BiasCorrector: ", utils::packageVersion("BiasCorrector")
        ))
      )
    ),

    shinydashboard::dashboardBody(
      # shinyjs stuff
      shinyjs::useShinyjs(),

      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "dashboard",
          module_fileupload_ui("moduleFileupload",
                               maxfilesize = maxfilesize)
        ),

        # experimental data panels
        shinydashboard::tabItem(
          tabName = "panel_1",
          module_experimentalfile_ui("moduleExperimentalFile")
        ),

        # calibration data panel
        shinydashboard::tabItem(
          tabName = "panel_2",
          module_calibrationfile_ui("moduleCalibrationFile")
        ),

        # regression plots
        shinydashboard::tabItem(
          tabName = "panel_3",
          module_plotting_ui("modulePlotting")
        ),

        # regression statistics
        shinydashboard::tabItem(
          tabName = "panel_4",
          module_statistics_ui("moduleStatistics")
        ),

        # select regression model
        shinydashboard::tabItem(
          tabName = "panel_5",
          module_modelselection_ui("moduleModelSelection")
        ),

        # select regression model
        shinydashboard::tabItem(
          tabName = "panel_6",
          module_results_ui("moduleResults")
        ),

        # select regression model
        shinydashboard::tabItem(
          tabName = "panel_7",
          module_correctedplots_ui("moduleCorrectedPlots")
        ),

        # select regression model
        shinydashboard::tabItem(
          tabName = "panel_8",
          module_correctedstatistics_ui("moduleCorrectedStatistics")
        ),

        shinydashboard::tabItem(
          tabName = "panel_9",
          module_log_ui("moduleLog")
        ),

        shinydashboard::tabItem(
          tabName = "settings",
          module_settings_ui("moduleSettings")
        ),

        shinydashboard::tabItem(
          tabName = "info",
          module_info_ui("moduleInfo")
        )
      )
    )
  )
))
