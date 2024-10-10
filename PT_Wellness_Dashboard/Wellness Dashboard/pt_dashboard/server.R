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
               rmarkdown,
               RColorBrewer)

# -----------------------------------
# Custom Functions/Variables
# -----------------------------------
palettes <- list(
  SANE        = brewer.pal(3, 'Set1'),
  Performance = brewer.pal(3, 'Set1'),
  Wellness    = brewer.pal(3, 'Set1'),
  Severity    = brewer.pal(3, 'Set1'),
  Frequency   = brewer.pal(3, 'Set1'),
  Sleep       = brewer.pal(3, 'Set1')
)

rolling_mean <- function(x) {
  rollmean(x, k = 3, fill = NA, align = 'right')
}

scale_func <- function(x) {
  sprintf("%.1f", x)
}

perc_change <- function(x) {
  round((tail(x, 1) - head(x, 1)) / head(x, 1) * 100, digits = 1)
}

get_palette <- function(x) {
  palettes[x]
}

function(input, output, sesssion) {
  data <- reactiveValues(raw = NULL)
  
  #------------------------------------
  # Read data from the selected file
  #------------------------------------
  observeEvent(
    eventExpr = input$file,
    {
      file <- input$file
      req(file)
      data$raw <- read_excel(path = as.character(file$datapath),
                             sheet = 'Form1') %>%
        as.data.frame() %>%
        rename(start       = names(.)[2],
               end         = names(.)[3],
               Name        = names(.)[4],
               Therapist   = names(.)[5],
               SANE        = names(.)[6],
               Performance = names(.)[7],
               Wellness    = names(.)[8],
               Severity    = names(.)[9],
               Frequency   = names(.)[10],
               Sleep       = names(.)[11]) %>%
        mutate(
          Date        = date(start),
          SANE        = round(as.double(SANE), digits = 1),
          Performance = round(as.double(Performance), digits = 1),
          Wellness    = round(as.double(Wellness), digits = 1),
          Severity    = round(as.double(Severity), digits = 1),
          Frequency   = round(as.double(Frequency), digits = 1),
          Sleep       = round(as.double(Sleep), digits = 1)
        ) %>%
        select(Date, Name, Therapist, SANE, Performance, Wellness, Severity, Frequency, Sleep)
      
      
    }
  )
  
  #-----------------------------------------------------------------------------
  # Update Therapist select drop-down list
  #-----------------------------------------------------------------------------   
  observeEvent(
    input$file,
    {
      file <- input$file
      req(file)
      
      tmp <- data$raw
      
      choices <- sort(unique(tmp$Therapist))
      
      updateSelectInput(inputId  = "therapistSelect",
                        choices  = choices,
                        selected = head(choices, 1))
    }
  )
  
  #-----------------------------------------------------------------------------
  # Update Rolling Window Drop-Down List
  #-----------------------------------------------------------------------------
  observeEvent(
    input$file,
    {
      file <- input$file
      req(file)

      tmp <- data$raw

      choices <- c(1: min(3, length(tmp$Name[tmp$Name == input$patientSelect])))

      updateSelectInput(
        inputId  = 'rollWindow',
        choices  = choices,
        selected = tail(choices, 1)
      )
    }
  )
  
  observeEvent(
    input$patientSelect,
    {
      file <- input$file
      req(file)
      
      tmp <- data$raw
      
      choices <- c(1: min(3, length(tmp$Name[tmp$Name == input$patientSelect])))
      
      updateSelectInput(
        inputId  = 'rollWindow',
        choices  = choices,
        selected = tail(choices, 1)
      )
    }
  )
  
  
  #-----------------------------------------------------------------------------
  # Update Patient select drop-down list
  #-----------------------------------------------------------------------------   
  observeEvent(
    input$file,
    {
      file <- input$file
      req(file)
      
      tmp <- data$raw
      
      choices <- sort(unique(tmp$Name))
      
      updateSelectInput(inputId  = "patientSelect",
                        choices  = choices,
                        selected = head(choices, 1))
    }
  )
  
  observeEvent(
    input$therapistSelect,
    {
      file <- input$file
      req(file)
      
      tmp <- data$raw[data$raw$Therapist == input$therapistSelect, ]
      
      choices <- sort(unique(tmp$Name))
      
      updateSelectInput(inputId  = "patientSelect",
                        choices  = choices,
                        selected = head(choices, 1))
    }
  )
  
  #-----------------------------------------------------------------------------
  # Create text summaries
  #-----------------------------------------------------------------------------
  changes <- reactiveValues(
    SANE        = 0,
    Performance = 0,
    Wellness    = 0,
    Frequency   = 0,
    Severity    = 0,
    Sleep       = 0
  )
  
  observeEvent(
    input$patientSelect,
    {
      tmp <- data$raw[data$raw$Name == input$patientSelect, ]
      changes$SANE        <- perc_change(tmp$SANE)
      changes$Performance <- perc_change(tmp$Performance)
      changes$Wellness    <- perc_change(tmp$Wellness)
      changes$Frequency   <- perc_change(tmp$Frequency)
      changes$Severity    <- perc_change(tmp$Severity)
      changes$Sleep       <- perc_change(tmp$Sleep)
    }
  )
  
  output$txtSANE <- renderText({
    paste(changes$SANE, "%",
          sep = '')
  })
  
  output$txtPerformance <- renderText({
    paste(changes$Performance, "%", 
          sep = '')
  })
  
  output$txtWellness <- renderText({
    paste(changes$Wellness, "%", 
          sep = '')
  })
  
  output$txtFrequency <- renderText({
    paste(changes$Frequency, "%", 
          sep = '')
  })
  
  output$txtSeverity <- renderText({
    paste(changes$Severity, "%", 
          sep = '')
  })
  
  output$txtSleep <- renderText({
    paste(changes$Sleep, "%", 
          sep = '')
  })
  
  #-----------------------------------------------------------------------------
  # Create plot
  #-----------------------------------------------------------------------------
  output$plot <- renderPlotly(
    expr = {
      req(input$file)
      
      Sys.sleep(3)
      
      tmp <- data$raw[data$raw$Name == input$patientSelect, ]
      levels <- c('SANE', 'Performance', 'Wellness', 'Sleep', 'Frequency', 'Severity')
      
      plot <- tmp %>%
        pivot_longer(
          cols = !c(Date, Name, Therapist),
          names_to = "Question",
          values_to = "Response",
          values_drop_na = TRUE
        ) %>%
        group_by(Question) %>%
        mutate(roll_mean = rollmean(Response, 
                                    k = as.integer(input$rollWindow),
                                    fill = NA, align = 'right')) %>%
        mutate(Question = factor(Question, 
                                 levels = levels)) %>%
        ggplot(mapping = aes(x = Date, y = Response)) +
        geom_area(aes(color = Question, fill = Question, alpha = 0.5)) +
        geom_point()
      
      if (length(tmp$Name) > 1) {
        plot <- plot + 
          geom_line(aes(y = roll_mean))
      }
      plot <- plot +
        facet_wrap(~Question, nrow = 3, scales = 'free') +
        theme_gray() +
        theme(legend.position = 'none',
              panel.spacing = unit(2, 'lines'),
              axis.title.x = element_blank()) +
        # scale_color_manual(values = unlist(lapply(unique(levels), get_palette))) +
        scale_y_continuous(labels = scale_func)
      
      plot
    }
  )
  
  # ----------------------------------------------------------------------------
  # Report Button
  # ----------------------------------------------------------------------------
  output$createReport <- downloadHandler(
    filename = function() {
      paste("Report_", input$therapistSelect, "_", input$patientSelect, "_", Sys.Date(), ".html",
            sep = "")
    },
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # tempReport <- file.path(tempdir(), "wellness_report.Rmd")
      # file.copy("wellness_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data        = data$raw[data$raw$Therapist == input$therapistSelect, ],
                     therapist   = input$therapistSelect,
                     patient     = input$patientSelect,
                     roll_window = as.integer(input$rollWindow))
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render('wellness_report.Rmd', output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #-----------------------------------------------------------------------------
  # Create table of pivot data
  #-----------------------------------------------------------------------------
  output$table <- renderDataTable(
    expr = {
      req(input$file)

      tmp <- data$raw

      tmp[tmp$Name == input$patientSelect, ] %>%
        datatable()
    }
  )
}