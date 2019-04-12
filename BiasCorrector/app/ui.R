# define UI
ui <- fluidPage(
  
  shinyjs::useShinyjs(), # Include shinyjs in the UI
  # https://stackoverflow.com/questions/25062422/restart-shiny-session
  extendShinyjs(script = "reset.js", functions = "reset"), # Add the js code to the page
  
  # design the main-page
  div(id="mainpage",
      
      # App title --> change for development
      titlePanel("BCv0.9"),
      # titlePanel("BiasCorrector v0.0.1"),
      # h5("based on Moskalev et al. 2011"),
      
      # Sidebar Layout with input and output definitions
      fluidRow(
        
        # Sidebar Panel
        sidebarPanel(
          
          h4(tags$b("Type of data")),
          
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
          
          tags$hr(id = "tag1"),
          
          
          conditionalPanel(
            condition = "input.type_locus_sample != null",
            
            verbatimTextOutput("samplelocus_out"),
            
            tags$hr(),
            
            h4(tags$b("Datainput")),
            h5("Please upload the CSV files* containing the experimental data and the calibration data."),
            
            # Input: Select a file
            fileInput("experimentalFile", "Experimental data: choose one CSV file containing the experimental data to be corrected",
                      multiple = FALSE,
                      accept = c(".csv", "text/csv")),
            h6(paste("Max. filesize: ", maxfilesize, " MB")),
            
            h6("*For the specific CSV file requirements please refere to our", a("FAQ!", href="https://github.com/kapsner/BiasCorrector/blob/master/FAQ.md")),
            
            tags$hr(),
            
            conditionalPanel(
              condition =  "output.experimental_data != null",
              
              uiOutput("fileInputCal"),
              
              # fileInput("calibrationFile", "Calibration data: choose one CSV file containing the calibration data",
              #             multiple = rv$import_type2,
              #             accept = c(".csv")),
              
              h6(paste("Max. filesize: ", maxfilesize, " MB")),
              
              tags$hr(id = "tag2"),
              
              
              conditionalPanel(
                condition = "output.calibration_data != null",
                
                # Actionbutton to start analysis
                actionButton("run", "Run Analysis"),
                
                tags$hr(),
                
                # Restart session
                actionButton("reset", "Reset App")
              )
            )
          ),
          # sidepanal width in fluidlayout -> max. 12 units
          width = 3),
        
        mainPanel(
          tabsetPanel(id = "tabs",
                      tabPanel(title = "Experimental data", value = "panel_1",
                               div(class="row", style="margin: 0.5%"),
                               verbatimTextOutput("exp_samples"), 
                               verbatimTextOutput("exp_samples_raw"), 
                               dataTableOutput("experimental_data"),
                               tags$hr())
          )
        )
      )
  )
)