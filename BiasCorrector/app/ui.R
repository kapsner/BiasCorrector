# define UI
ui <- dashboardPage(skin = "black",
                    
                    # App title --> change for development
                    dashboardHeader(title = "BiasCorrector"),
                    # h5("based on Moskalev et al. 2011"),
                    
                    # Sidebar Layout with input and output definitions
                    dashboardSidebar(
                      # shinyjs stuff
                      shinyjs::useShinyjs(), # Include shinyjs in the UI
                      
                      #Sidebar Panel
                      sidebarMenu(id = "tabs",
                                  menuItem("File Upload", tabName = "dashboard", icon = icon("file")),
                                  sidebarMenuOutput("menu"),
                                  menuItem("Log", tabName = "panel_9", icon = icon("file-alt")),
                                  tags$hr(),
                                  actionButton("reset", "Reset App", width = "80%") # Restart session
                      )
                    ),
                    
                    dashboardBody(
                      # shinyjs stuff
                      shinyjs::useShinyjs(), # Include shinyjs in the UI
                      # https://stackoverflow.com/questions/25062422/restart-shiny-session
                      extendShinyjs(script = "reset.js", functions = "reset"), # Add the js code to the page
                      
                      tabItems(
                        tabItem(tabName = "dashboard",
                                moduleFileuploadUI("moduleFileupload")
                        ),
                        
                        # experimental data panels
                        tabItem(tabName = "panel_1",
                                moduleExperimentalFileUI("moduleExperimentalFile")
                        ),
                        
                        # calibration data panel
                        tabItem(tabName = "panel_2",
                                moduleCalibrationFileUI("moduleCalibrationFile")
                        ),
                        
                        # regression plots
                        tabItem(tabName = "panel_3",
                                modulePlottingUI("modulePlotting")
                        ),
                        
                        # regression statistics
                        tabItem(tabName = "panel_4",
                                moduleStatisticsUI("moduleStatistics")
                        ),
                        
                        # select regression model
                        tabItem(tabName = "panel_5",
                                moduleModelSelectionUI("moduleModelSelection")
                        ),
                        
                        # select regression model
                        tabItem(tabName = "panel_6",
                                moduleResultsUI("moduleResults")
                        ),
                        
                        # select regression model
                        tabItem(tabName = "panel_7",
                                moduleCorrectedPlotsUI("moduleCorrectedPlots")
                        ),
                        
                        tabItem(tabName = "panel_9",
                                fluidRow(
                                  box(
                                    title = "Log",
                                    verbatimTextOutput("log_out"),
                                    tags$head(tags$style("#log_out{overflow-y:scroll; max-height: 80vh; background: ghostwhite;}")),
                                    width = 9
                                  ),
                                  box(title = "Download Log File",
                                      div(class="row", style="text-align: center;", shinyjs::disabled(downloadButton("downloadLogfile",
                                                                                                   "Download Log File",
                                                                                                   style="white-space: normal; text-align:center; 
                                                                                               padding: 9.5px 9.5px 9.5px 9.5px;
                                                                                               margin: 6px 10px 6px 10px;"))),
                                      tags$hr(),
                                      width=3
                                  ))
                        )
                      )
                    )
)