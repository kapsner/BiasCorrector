# define UI
ui <- dashboardPage(skin = "black",
                    
                    # App title --> change for development
                    dashboardHeader(title = "BiasCorrector v0.9.3"),
                    # titlePanel("BiasCorrector v0.0.1"),
                    # h5("based on Moskalev et al. 2011"),
                    
                    # Sidebar Layout with input and output definitions
                    dashboardSidebar(
                      # shinyjs stuff
                      shinyjs::useShinyjs(), # Include shinyjs in the UI
                      
                      #Sidebar Panel
                      sidebarMenu(id = "tabs",
                                  menuItem("Fileupload", tabName = "dashboard", icon = icon("file")),
                                  sidebarMenuOutput("menu"),
                                  menuItem("Log", tabName = "panel_9", icon = icon("file-alt")),
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
                                fluidRow(
                                  # type of data box
                                  box(
                                    h4(tags$b("Type of Data")),
                                    
                                    # Radiobuttons: Type of data
                                    radioButtons(inputId = "type_locus_sample", label = h5("Please specify the type of DNA methylation data to be corrected for measurement biases"),
                                                 choices = list("One locus in many samples (e.g. pyrosequencing data)" = 1,
                                                                "Many loci in one sample (e.g. next-generation sequencing data or microarray data)" = 2),
                                                 selected = character(0)),
                                    
                                    tags$hr(),
                                    
                                    conditionalPanel(
                                      condition = "input.type_locus_sample == 1",
                                      textInput("locusname",
                                                label = NULL,
                                                placeholder = "Locus name")
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "input.type_locus_sample == 2",
                                      textInput("samplename",
                                                label = NULL,
                                                placeholder = "Sample-ID")
                                    ),
                                    conditionalPanel(
                                      condition = "input.type_locus_sample != null",
                                      verbatimTextOutput("samplelocus_out")
                                    ), width = 5)
                                ),
                                
                                # experimental fileupload box
                                fluidRow(
                                  conditionalPanel(
                                    condition = "input.type_locus_sample != null",
                                    
                                    box(
                                      h4(tags$b("Datainput: Experimental Data")),
                                      h5("Please upload the CSV files* containing the experimental data."),
                                      
                                      # Input: Select a file
                                      fileInput("experimentalFile", "Please choose one CSV file containing the experimental data that is to be corrected.",
                                                multiple = FALSE,
                                                accept = c(".csv", "text/csv")),
                                      h6(paste("Max. filesize: ", maxfilesize, " MB")),
                                      
                                      h6("*For the specific CSV file requirements please refere to our", a("FAQ!", href="https://github.com/kapsner/BiasCorrector/blob/master/FAQ.md")),
                                      width = 5)
                                  ),
                                  
                                  # calibration fileupload box
                                  conditionalPanel(
                                    condition =  "output.fileUploaded",
                                    
                                    box(
                                      h4(tags$b("Datainput: Calibraration Data")),
                                      h5("Please upload the CSV files* containing the calibration data."),
                                      
                                      uiOutput("fileInputCal"),
                                      
                                      # fileInput("calibrationFile", "Calibration data: choose one CSV file containing the calibration data",
                                      #             multiple = rv$import_type2,
                                      #             accept = c(".csv")),
                                      
                                      h6(paste("Max. filesize: ", maxfilesize, " MB")),
                                      h6("*For the specific CSV file requirements please refere to our", a("FAQ!", href="https://github.com/kapsner/BiasCorrector/blob/master/FAQ.md")),
                                      width = 5)
                                  )
                                )
                        ),
                        
                        # experimental data panels
                        tabItem(tabName = "panel_1",
                                fluidRow(
                                  box(
                                    title = "Experimental Data",
                                    div(class="row", style="margin: 0.5%"),
                                    verbatimTextOutput("exp_samples"),
                                    verbatimTextOutput("exp_samples_raw"),
                                    dataTableOutput("experimental_data"),
                                    tags$hr(),
                                    tags$head(tags$style("#panel_1{margin: 0.5%;}")),
                                    width = 12
                                  ))
                        ),
                        
                        # calibration data panel
                        tabItem(tabName = "panel_2",
                                fluidRow(
                                  box(
                                    title = "Calibration Data",
                                    div(class="row", style="margin: 0.5%"),
                                    verbatimTextOutput("cal_samples"),
                                    verbatimTextOutput("cal_samples_raw"),
                                    uiOutput("calibration_data"),
                                    uiOutput("calibration_data2"),
                                    tags$hr(),
                                    width = 12
                                  ))
                        ),
                        
                        # regression plots
                        tabItem(tabName = "panel_3",
                                fluidRow(
                                  box(
                                    title = "Regression Plot",
                                    imageOutput("plots"),
                                    width=6
                                  ),
                                  box(
                                    title = "Plot Selection",
                                    uiOutput("selectPlotInput"),
                                    width = 4)
                                  )
                                ),
                        
                        # regression statistics
                        tabItem(tabName = "panel_4",
                                fluidRow(
                                  box(
                                    title = "Regression Statistics",
                                    div(class="row", style="margin: 0.5%"),
                                    uiOutput("regression_statistics"),
                                    tags$hr(),
                                    div(class="row", style="text-align: center", downloadButton("downloadRegStat", "Download regression statistics")),
                                    tags$hr(),
                                    width = 12
                                  ))
                        ),
                        
                        # select regression model
                        tabItem(tabName = "panel_5",
                                fluidRow(
                                  box(
                                    title = "Select Regression Model",
                                    div(class="row", style="margin: 0.5%"),
                                    uiOutput("reg_radios"),
                                    tags$hr(),
                                    width = 9
                                  ))
                        ),
                        
                        # select regression model
                        tabItem(tabName = "panel_6",
                                fluidRow(
                                  box(
                                    title = "BiasCorrected Results",
                                    div(class="row", style="margin: 0.5%"),
                                    uiOutput("corrected_data"),
                                    tags$hr(),
                                    uiOutput("substitutedOut"),
                                    tags$hr(),
                                    width = 12
                                  ))
                        ),
                        
                        tabItem(tabName = "panel_9",
                                fluidRow(
                                  box(
                                    title = "Log",
                                    verbatimTextOutput("log_out"),
                                    tags$head(tags$style("#log_out{overflow-y:scroll; max-height: 80vh; background: ghostwhite;}")),
                                    width = 6
                                  ))
                        )
                      )
                    )
)