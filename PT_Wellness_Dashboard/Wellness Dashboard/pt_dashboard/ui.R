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
  titlePanel(div(
    div(img(src = 'logo.png', style = "height: 100px;")),
    div("Wellness Dashboard"))
  ),
  
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
        tabPanel('Patient Overview', 
                 wellPanel(
                   h3("Change from first response to current average"),
                   fluidRow(
                     fluidRow(
                       column(width = 1),
                       column(width = 2, align = 'center', h4('SANE', align = 'center'), textOutput('txtSANE')),
                       column(width = 2),
                       column(width = 2, align = 'center', h4('Performance', align = 'center'), textOutput('txtPerformance')),
                       column(width = 2),
                       column(width = 2, align = 'center', h4('Wellness', align = 'center'), textOutput('txtWellness')),
                       column(width = 1)
                     ),
                     fluidRow(
                       column(width = 1),
                       column(width = 2, align = 'center', h4('Frequency', align = 'center'), textOutput('txtFrequency')),
                       column(width = 2),
                       column(width = 2, align = 'center', h4('Severity', align = 'center'), textOutput('txtSeverity')),
                       column(width = 2),
                       column(width = 2, align = 'center', h4('Sleep', algin = 'center'), textOutput('txtSleep')),
                       column(width = 1)
                     )
                   ),
                   plotlyOutput(outputId = 'plot', height = '600px')
                 )
        ),
        tabPanel('Raw Data', wellPanel(dataTableOutput(outputId = 'table')))
      )
    )
  )
  

)