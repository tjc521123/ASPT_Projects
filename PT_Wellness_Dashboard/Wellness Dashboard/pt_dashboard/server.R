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

# -----------------------------------
# Rolling Mean Function
# -----------------------------------
rolling_mean <- function(x) {
  rollmean(x, k = 3, fill = NA, align = 'right')
}

scale_func <- function(x) {
  sprintf("%.1f", x)
}

perc_change <- function(x) {
  round((tail(x, 1) - head(x, 1)) / head(x, 1) * 100, digits = 1)
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
      
      choices <- c(1: min(3, nrow(data[data$Name == input$patientSelect, ])))
      
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
  output$txtSANE <- renderText({
    paste((tail(data$SANE[data$Name == input$patientSelect], 1) - head(data$SANE[data$Name == input$patientSelect], 1))/head(data$SANE[data$Name == input$patientSelect], 1),
          "%",
          sep = '')
  })
  
  output$txtPerformance <- renderText({
    paste(perc_change(data$Performance[data$Name == input$patientSelect]), "%", 
          sep = '')
  })
  
  output$txtWellness <- renderText({
    paste(perc_change(data$Wellness[data$Name == input$patientSelect]), "%", 
          sep = '')
  })
  
  output$txtFrequency <- renderText({
    paste(perc_change(data$Frequency[data$Name == input$patientSelect]), "%", 
          sep = '')
  })
  
  output$txtSeverity <- renderText({
    paste(perc_change(data$Severity[data$Name == input$patientSelect]), "%", 
          sep = '')
  })
  
  output$txtSleep <- renderText({
    paste(perc_change(data$Sleep[data$Name == input$patientSelect]), "%", 
          sep = '')
  })
  
  #-----------------------------------------------------------------------------
  # Create plot of select metric
  #-----------------------------------------------------------------------------
  output$plot <- renderPlotly(
    expr = {
      req(input$file)
      
      tmp <- data$raw[data$raw$Name == input$patientSelect, ]
      
      tmp %>%
        pivot_longer(
          cols = !c(Date, Name, Therapist),
          names_to = "Question",
          values_to = "Response",
          values_drop_na = TRUE
        ) %>%
        group_by(Question) %>%
        mutate(roll_mean = rollmean(Response, k = as.integer(input$rollWindow), fill = NA, align = 'right')) %>%
        mutate(Question = factor(Question, 
                                 levels = c('SANE', 'Performance', 'Wellness', 'Sleep', 'Frequency', 'Severity'))) %>%
        ggplot(mapping = aes(x = Date, y = Response)) +
        geom_area(aes(color = Question, fill = Question, alpha = 0.5)) +
        geom_point() +
        geom_line(aes(y = roll_mean)) +
        facet_wrap(~Question, nrow = 3, scales = 'free') +
        theme_gray() +
        theme(legend.position = 'none',
              panel.spacing = unit(2, 'lines'),
              axis.title.x = element_blank()) +
        scale_y_continuous(labels = scale_func)
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