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
# Create functions for aggregation of data
#-------------------------------------------------------------------------------

mean_agg <- function(x) {
  mean(x, na.rm = TRUE)
}

min_agg <- function(x) {
  if (class(x) != "factor") {
    min(x, na.rm = TRUE)
  } else {
    return(NULL)
  }
}

max_agg <- function(x) {
  if (class(x) != "factor") {
    max(x, na.rm = TRUE)
  } else {
    return(NULL)
  }
}

perc_change <- function(x, na.rm = TRUE) {
  (x - lag(x))/lag(x)
}

#-------------------------------------------------------------------------------
# Server logic
#-------------------------------------------------------------------------------

function(input, output, session) {
  
  data_out <- reactiveValues(data = NULL, orig = NULL)
  newData <- reactiveVal()
  
  create_row <- function(x) {
    tib <- tibble(col = 1)
    
    for (name in names(x)) {
      tib <- cbind(tib, tibble("{name}" := input[[name]]))
    }
    
    tib$col <- NULL
    
    return(tib)
  }  

  #-----------------------------------------------------------------------------
  # Read data from the selected file
  #-----------------------------------------------------------------------------
  observeEvent(
    eventExpr = input$speedFile,
    {
      file <- input$speedFile
      req(file)
      data_out$orig <- read_excel(path = as.character(file$datapath)) %>%
        mutate(Date = as.Date(Date)) %>%
        as.data.frame()
      data_out$data <- data_out$orig
    }
  )
  
  #-----------------------------------------------------------------------------
  # Read data from test file and join it with speed data
  #-----------------------------------------------------------------------------
  # observeEvent(
  #   eventExpr = input$testFile,
  #   {
  #     test_file <- input$testFile
  #     req(test_file)
  #     speed_file <- input$speedFile
  #     req(speed_file)
  #     
  #     data_test <- read_excel(path = as.character(test_file$datapath)) %>%
  #       mutate(Split  = SPLIT_10,
  #              Sprint = SPRINT_20,
  #              Run    = NA) %>%
  #       select(Date, Athlete, Run, Split, Sprint) %>%
  #       as.data.frame()
  #     print(data_test)
  #     data_out$data <- rbind(data_out$data, 
  #                            data_test[data_test$Athlete %in% data_out$data$Athlete, ])
  #   }
  # )
  
  #-----------------------------------------------------------------------------
  # Download handler for the current data
  #-----------------------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      ext  <- paste(".", file_ext(input$speedFile$name), sep = "")
      name <- gsub(ext, "", input$speedFile$name)
      paste(name, "_", gsub("-", "_", Sys.Date()), '.xlsx', sep = '')
    },
    
    content  = function(file) {
      writexl::write_xlsx(data_out$data, path = file)
    }
  )
  
  #-----------------------------------------------------------------------------
  # Update athlete select drop-down list
  #-----------------------------------------------------------------------------
  observeEvent(
    input$speedFile,
    {
      file <- input$speedFile
      req(file)
      #req(input$testFile)
      
      choices <- rbind("All", sort(unique(data_out$data$Athlete)))
      
      updateSelectInput(inputId  = "selectAthlete",
                        choices  = choices,
                        selected = head(choices, 1))
    }
  )
  
  #-----------------------------------------------------------------------------
  # Update variable select drop-down list
  #-----------------------------------------------------------------------------   
  observeEvent(
    input$speedFile,
    {
      file <- input$speedFile
      req(file)
      #req(input$testFile)
      
      choices <- data_out$orig %>%
        select(-c(Date, Athlete)) %>%
        names()
      
      updateSelectInput(inputId  = "selectCol",
                        choices  = choices,
                        selected = head(choices, 1))
    }
  )

  #-----------------------------------------------------------------------------
  # Add Data button functionality
  #-----------------------------------------------------------------------------
  observeEvent(
    eventExpr   = input$addData,
    handlerExpr = {
      tib <- tibble(col = 1)
      
      for (name in names(data_out$orig)) {
        tib <- cbind(tib, tibble("{name}" := input[[name]]))
      }
      
      tib$col <- NULL
      
      data_out$data <-
        data_out$data %>%
        rbind(tib)
    }
  )
  
  # ----------------------------------------------------------------------------
  # Report Button
  # ----------------------------------------------------------------------------
  output$createReport <- downloadHandler(
    filename = "report.html",

    content  = function(file) {
      report_dat <- data_out$data[, c("Date", "Athlete", input$selectReport)]

      report_path <- tempfile(fileext = ".Rmd")
      file.copy("createReport.Rmd", report_path, overwrite = TRUE)

      params_list <- list(
        data = report_dat,
        aths = input$selectAthlete,
        vars = input$selectReport,
        mins = input$selectMin
      )
      
      render(
        input         = report_path,
        output_file   = file,
        output_format = "html_document",
        params        = params_list
      )
    }
  )
  

  

  
  #-----------------------------------------------------------------------------
  # Create inputs for new data based on input file
  #-----------------------------------------------------------------------------
  output$inputs <- renderUI({
    lapply(names(data_out$orig), function(name) {
      label_name <- gsub(pattern = "_", replacement = " ", x = name)
      switch(class(get(name, data_out$orig))[1],
             "Date"      = dateInput(inputId = name,
                                     label   = label_name),
             "POSIXct"   = dateInput(inputId = name,
                                     label   = label_name),
             "POSIXt"    = dateInput(inputId = name,
                                     label   = label_name),
             "numeric"   = numericInput(inputId = name,
                                        label   = label_name,
                                        value   = 0),
             "factor"    = textInput(inputId = name,
                                     label   = label_name),
             "character" = textInput(inputId = name,
                                     label   = label_name))
    })
  })
  

  
  #-----------------------------------------------------------------------------
  # Render Raw Data plot using the data
  #-----------------------------------------------------------------------------  
  output$rawPlot <- renderPlotly({
    req(input$speedFile)
    #req(input$testFile)
    
    if (!is.null(data_out$orig)) {
      if (input$selectAthlete == "All") {
        tmp <- data_out$data
      } else {
        tmp <- filter(data_out$data, data_out$data$Athlete == input$selectAthlete)
      }
      
      tmp <- tmp %>%
        pivot_longer(!c(Date, Athlete), values_drop_na = TRUE) %>%
        group_by(Date, Athlete, name) %>%
        summarise_if(is.numeric, min, na.rm = TRUE) %>%
        filter(name == input$selectCol)
      
      title <- gsub("_", " ", input$selectCol)
      
      plot <- 
        tmp %>%
        ggplot(mapping = aes(x = Date, y = value, color = Athlete)) + 
        geom_line() +
        geom_point() +
        ylim(0, NA) +
        labs(
          title = title,
          x     = "Date",
          y     = "Result"
        )
      
      ggplotly(plot)
    }
  })
  
  #-----------------------------------------------------------------------------
  # Render Summary Data plot using the data
  #----------------------------------------------------------------------------- 
  output$summPlot <- renderPlotly({
    req(input$speedFile)
    #req(input$testFile)
    
    if (!is.null(data_out$orig)) {
      if (input$selectAthlete == "All") {
        tmp <- data_out$data
      } else {
        tmp <- filter(data_out$data, data_out$data$Athlete == input$selectAthlete)
      }
      
      tmp <- tmp %>%
        pivot_longer(!c(Date, Athlete), values_drop_na = TRUE) %>%
        group_by(Athlete, name) %>%
        mutate_if(is.numeric, perc_change) %>%
        filter(name == input$selectCol)
      
      title <- gsub("_", " ", input$selectCol)
      
      plot <- 
        tmp %>%
        ggplot(mapping = aes(x = Date, y = value)) + 
        geom_boxplot(fill = 'red') +
        geom_hline(yintercept = 0) + 
        labs(
          title = title,
          x     = "Date",
          y     = "Percentage Change"
        )
      
      ggplotly(plot)
    }
  })
  
  # ----------------------------------------------------------------------------
  # Create data table
  # ----------------------------------------------------------------------------
  output$speedTable <- renderDataTable({
    req(input$speedFile)
    #req(input$testFile)
    
    if (input$selectAthlete == "All") {
      tmp <- data_out$data
    } else {
      tmp <- filter(data_out$data, data_out$data$Athlete == input$selectAthlete)
    }
    
    tmp %>%
      datatable(rownames   = FALSE,
                width      = "90%",
                filter     = "top",
                editable   = "cell",
                extensions = c('RowGroup', 'Scroller'),
                options    = list(
                  columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(data_out$data) - 1))),
                  pageLength  = 20,
                  lengthMenu  = c(5, 10, 20, 50),
                  scrollX     = TRUE,
                  dom         = 'Bfrtip',
                  rowGroup    = list(dataSrc = which(colnames(data_out$data) == "Date") - 1),
                  deferRender = TRUE
                ))
  })
  
  observeEvent(input$speedTable_cell_edit, {
    row  <- input$speedTable_cell_edit$row
    col  <- input$speedTable_cell_edit$col
    data_out$data[row, col + 1] <- input$speedTable_cell_edit$value
  })
  
  # ----------------------------------------------------------------------------
  # Create Reporting Wizard Tab
  # ----------------------------------------------------------------------------
  output$reportParams <- renderUI({
    req(input$speedFile)
    #req(input$testFile)
    
    var_names <- data_out$data %>%
      select(where(is.numeric)) %>%
      names()
    
    wellPanel(
      h3("Select which variables and metrics you would like in the report."),
      
      selectInput(inputId  = "selectReport",
                  label    = "",
                  choices  = var_names,
                  multiple = TRUE),
      
      checkboxGroupInput(inputId = "selectMin",
                         label   = "Report minimum value?",
                         choices = NULL)
    )
  })
  
  observeEvent(
    eventExpr = input$selectReport,
    handlerExpr = {
      updateCheckboxGroupInput(
        inputId = "selectMin",
        choices = input$selectReport
      )
    }
  )
  
}