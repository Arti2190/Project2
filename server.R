library(shiny)
#library(dplyr)
library(DT)
#library(ggplot2)
#library(readxl)
library(bslib)
library(palmerpenguins)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(ggplot2)


function(input, output, session) {
  # Reactive data container for subsetted data
  subsetted_data <- reactiveValues(data = super_data)
  
  # Reactive value to store the summary statistics
  summary_data <- reactiveVal(NULL)
  
  # Update the subsetted data on button click
  observeEvent(input$subset_button, {
    data_filtered <- super_data
    
    # Apply categorical filtering if not select anyone then by default is show ALL
    if (input$cat_var1 != "All") {
      data_filtered <- data_filtered %>% filter(Category == input$cat_var1)
    }
    if (input$cat_var2 != "All") {
      data_filtered <- data_filtered %>% filter(Segment == input$cat_var2)
    }
    
    # Apply numeric filtering if selected
    if (input$num_var1 != "None" && !is.null(input$num_range1)) {
      data_filtered <- data_filtered %>% filter(between(!!sym(input$num_var1), input$num_range1[1], input$num_range1[2]))
    }
    if (input$num_var2 != "None" && !is.null(input$num_range2)) {
      data_filtered <- data_filtered %>% filter(between(!!sym(input$num_var2), input$num_range2[1], input$num_range2[2]))
    }
    
    subsetted_data$data <- data_filtered
  })
  
  # Generate summary statistics when the "Generate Summary" button is clicked
  observeEvent(input$summary_button, {
    # Get filtered data
    data <- subsetted_data$data  
    
    # Check if the selected numeric variable exists and it is numeric
    if (input$numeric_summary_var %in% names(data) && is.numeric(data[[input$numeric_summary_var]])) {
      
      # Summarize the data
      summary <- data %>%
        summarise(
          Mean = mean(!!sym(input$numeric_summary_var), na.rm = TRUE),
          Median = median(!!sym(input$numeric_summary_var), na.rm = TRUE),
          SD = sd(!!sym(input$numeric_summary_var), na.rm = TRUE)
        )
      
      # Store the summary in the reactive value
      summary_data(summary)
    } else {
      # Return NA if no valid numeric variable
      summary_data(data.frame(Mean = NA, Median = NA, SD = NA))  
    }
  })
  
  # Render summary table from reactive value
  output$summary_table <- renderTable({
    # To Ensure summary data is available
    req(summary_data())  
    summary_data()  
  })
  
  # Render subsetted data table in the Data Download tab
  output$data_table <- DT::renderDataTable({
    req(subsetted_data$data)
    DT::datatable(subsetted_data$data)
  })
  
  # Download handler for the subsetted data
  output$download_data <- downloadHandler(
    
    filename = function() { paste("subsetted_data", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      # Data is store in .csv format
      write.csv(subsetted_data$data, file, row.names = FALSE)
    }
  )
  
  # Generate summary plot based on selected variables
  output$summary_plot <- renderPlot({
    req(input$plot_x, input$plot_y, subsetted_data$data)
    
    p <- ggplot(subsetted_data$data, aes(x = .data[[input$plot_x]], y = .data[[input$plot_y]]))
    
    # Add color if a color variable is selected
    if (input$plot_color != "None") {
      p <- p + aes(color = .data[[input$plot_color]])
    }
    
    # Add scatter plot or bar plot based on input types
    if (is.numeric(subsetted_data$data[[input$plot_x]]) && is.numeric(subsetted_data$data[[input$plot_y]])) {
      p + geom_point()
    } else {
      p + geom_bar(stat = "identity", position = "dodge")
    }
  })
}