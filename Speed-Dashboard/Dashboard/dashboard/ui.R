library(pacman)
pacman::p_load(shiny,
               tidyverse,
               ggplot2,
               readxl,
               writexl,
               plotly,
               DT,
               tools,
               rmarkdown)

#-------------------------------------------------------------------------------
# Create Fluid Page for UI
#-------------------------------------------------------------------------------
fluidPage(

  #-----------------------------------------------------------------------------
  # Dashboard Title
  #-----------------------------------------------------------------------------
  titlePanel("Architech Sports - Ballantyne"),

  #-----------------------------------------------------------------------------
  # Create sidebar layout
  #-----------------------------------------------------------------------------
  sidebarLayout(
    
    #---------------------------------------------------------------------------
    # Create sidebar panel
    #---------------------------------------------------------------------------
    sidebarPanel(
      
      #-------------------------------------------------------------------------
      # Create Speed File input widget
      #-------------------------------------------------------------------------
      fileInput(inputId = "speedFile",
                label   = "Select data file",
                accept  = c(".xlsx")),
      
      #-------------------------------------------------------------------------
      # Create Testing File input widget
      #-------------------------------------------------------------------------
      # fileInput(inputId = 'testFile',
      #           label   = 'Select Testing File',
      #           accept  = c('.xlsx')),
      
      #-------------------------------------------------------------------------
      # Create data download button
      #-------------------------------------------------------------------------
      downloadButton(outputId = "downloadData",
                     label    = "Download Data"),
      
      #-------------------------------------------------------------------------
      # Create report generation button
      #-------------------------------------------------------------------------
      downloadButton(outputId = "createReport",
                     label    = "Generate Report"),
      
      #-------------------------------------------------------------------------
      # Create athlete filter
      #-------------------------------------------------------------------------
      selectInput(inputId = "selectAthlete",
                  label   = "Select Athlete to view",
                  choices = ""),
      
      #-------------------------------------------------------------------------
      # Create column filter
      #-------------------------------------------------------------------------
      selectInput(inputId = "selectCol",
                  label   = "Select variable to display",
                  choices = ""),
      
      #-------------------------------------------------------------------------
      # Create header for data input
      #-------------------------------------------------------------------------
      h2("Enter new test"),
      
      #-------------------------------------------------------------------------
      # Region where dynamically created inputs will appear
      #-------------------------------------------------------------------------
      uiOutput(outputId = "inputs"),
      
      #-------------------------------------------------------------------------
      # Add data button
      #-------------------------------------------------------------------------
      actionButton(inputId = "addData",
                   label   = "Add data"),
      width = 2
    ),

    #---------------------------------------------------------------------------
    # Create main panel
    #---------------------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Raw Data", plotlyOutput("rawPlot")),
        tabPanel(title = "Summary", plotlyOutput("summPlot")),
        tabPanel(title = "Table", dataTableOutput("speedTable")),
        tabPanel(title = "Reporting", 
                 uiOutput(outputId = "reportParams"))
      ),

      width = 10
    )
  )
)
