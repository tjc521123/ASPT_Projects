library(pacman)
pacman::p_load(shiny,
               tidyverse,
               zoo,
               ggplot2,
               grid,
               readxl,
               writexl,
               plotly,
               DT,
               tools,
               rmarkdown)

# -------------------------------------
# Create Fluid Page Container
# -------------------------------------
fluidPage(
  
  # -----------------------------------
  # Dashboard Title
  # -----------------------------------
  titlePanel('Wellness Dashboard'),
  
  # -----------------------------------
  # Create Sidebar layout
  # -----------------------------------
  sidebarLayout(
    sidebarPanel(
      
      # -------------------------------
      # Create file input widget
      # -------------------------------
      fileInput(inputId = 'file',
                label   = 'Select file',
                accept  = c('.xlsx')),
      
      # -------------------------------
      # Create Location select widget
      # -------------------------------
      selectInput(inputId = 'locationSelect',
                  label   = 'Select Location',
                  choices = c('All',
                              'Ballantyne',
                              'Matthews',
                              'Pineville',
                              'University')),
      
      # -------------------------------
      # Create Therapist Selection Widget
      # -------------------------------
      selectInput(inputId = 'therapistSelect',
                  label   = 'Select Therapist',
                  choices = ""),
      
      # -------------------------------
      # Create Patient Selection widget
      # -------------------------------
      selectInput(inputId = 'patientSelect',
                  label   = 'Select Patient',
                  choices = ""),
      
      # -------------------------------
      # Create Rolling Window widget
      # -------------------------------
      selectInput(inputId  = 'rollWindow',
                  label    = 'Rolling Window',
                  choices  = ""),
      
      #--------------------------------
      # Create report generation button
      #--------------------------------
      downloadButton(outputId = "createReport",
                     label    = "Generate Report"),
    ),
    
    # ---------------------------------
    # Create Main Panel
    # ---------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel('Raw Data', wellPanel(dataTableOutput(outputId = 'table'))),
        tabPanel('Patient Overview', 
                 wellPanel(fluidRow(
                   fluidRow(
                     column(width = 4, h3('SANE'), verbatimTextOutput('txtSANE')),
                     column(width = 4, h3('Performance'), verbatimTextOutput('txtPerformance')),
                     column(width = 4, h3('Wellness'), verbatimTextOutput('txtWellness'))
                   ),
                   fluidRow(
                     column(width = 4, h3('Frequency'), verbatimTextOutput('txtFrequency')),
                     column(width = 4, h3('Severity'), verbatimTextOutput('txtSeverity')),
                     column(width = 4, h3('Sleep'), verbatimTextOutput('txtSleep'))
                   )
                 ),
                 plotlyOutput(outputId = 'plot', height = '600px'))
        )
      )
    )
  ),
  

)