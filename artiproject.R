# install the necessary libraries

# Load libraries
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(readxl)
library(bslib)

# Load the data (adjust the file path if needed)
super_data <- read_excel("US_Superstore_data.xls")
ui <- fluidPage(
  titlePanel("Superstore Data Exploration App"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdowns for categorical variables
      selectInput("cat_var1", "Select Category:", choices = c("All", unique(super_data$Category))),
      selectInput("cat_var2", "Select Segment:", choices = c("All", unique(super_data$Segment))),
      
      # Dynamic UI for numeric variables (displayed as sliders after selection)
      selectInput("num_var1", "Select Numeric Variable 1:", choices = c("None", "Sales", "Profit")),
      uiOutput("num_slider1"),
      
      selectInput("num_var2", "Select Numeric Variable 2:", choices = c("None", "Quantity", "Discount")),
      uiOutput("num_slider2"),
      
      # Action button to subset data
      actionButton("subset_button", "Subset Data")
    ),
    
    mainPanel(
      # Tab layout
      tabsetPanel(
        tabPanel("About",
                 h4("App Purpose"),
                 p("This app allows users to explore the Superstore sales data."),
                 p("Data source: [Superstore Sales Data](https://www.kaggle.com/datasets/sdolezel/superstore-sales)"),
                 p("Use the sidebar to filter data and navigate the tabs for different features."),
                 img(src = "path_to_image/logo.png", height = 100)),  # Adjust image path as needed
        
        tabPanel("Data Download",
                 DT::dataTableOutput("data_table"),
                 downloadButton("download_data", "Download Subsetted Data")),
        
        tabPanel("Data Exploration",
                 h4("Explore Numeric and Categorical Summaries"),
                 uiOutput("summary_selector"),
                 textOutput("dynamic_summary"),
                 plotOutput("summary_plot"),
                 plotOutput("category_plot"))
      )
    )
  )
)


server <- function(input, output, session) {
  # Reactive data container for subsetted data
  subsetted_data <- reactiveValues(data = super_data)
  
  # Dynamic UI for numeric variable sliders
  output$num_slider1 <- renderUI({
    req(input$num_var1 != "None")  # Show slider if a variable is selected
    range <- range(super_data[[input$num_var1]], na.rm = TRUE)
    sliderInput("num_range1", paste("Range for", input$num_var1), min = range[1], max = range[2], value = range)
  })
  
  output$num_slider2 <- renderUI({
    req(input$num_var2 != "None")
    range <- range(super_data[[input$num_var2]], na.rm = TRUE)
    sliderInput("num_range2", paste("Range for", input$num_var2), min = range[1], max = range[2], value = range)
  })
  
  # Update subsetted data on button click
  observeEvent(input$subset_button, {
    data_filtered <- super_data
    
    # Apply categorical filtering
    if (input$cat_var1 != "All") {
      data_filtered <- data_filtered %>% filter(Category == input$cat_var1)
    }
    if (input$cat_var2 != "All") {
      data_filtered <- data_filtered %>% filter(Segment == input$cat_var2)
    }
    
    # Apply numeric filtering
    if (input$num_var1 != "None" && !is.null(input$num_range1)) {
      data_filtered <- data_filtered %>% filter(between(!!sym(input$num_var1), input$num_range1[1], input$num_range1[2]))
    }
    if (input$num_var2 != "None" && !is.null(input$num_range2)) {
      data_filtered <- data_filtered %>% filter(between(!!sym(input$num_var2), input$num_range2[1], input$num_range2[2]))
    }
    
    subsetted_data$data <- data_filtered
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
      write.csv(subsetted_data$data, file, row.names = FALSE)
    }
  )
  
  # Dynamic UI for summary selection in Data Exploration tab
  output$summary_selector <- renderUI({
    tagList(
      selectInput("summary_var", "Choose Variable for Summary:", choices = names(super_data)),
      actionButton("summary_button", "Generate Summary")
    )
  })
  
  # Generate summary plot based on selected variable
  output$summary_plot <- renderPlot({
    req(input$summary_var, subsetted_data$data)
    
    # Check if the selected variable is numeric or categorical
    if (is.numeric(subsetted_data$data[[input$summary_var]])) {
      # Plot histogram for numeric variable
      ggplot(subsetted_data$data, aes(x = !!sym(input$summary_var))) +
        geom_histogram(fill = "blue", alpha = 0.6, bins = 30) +
        labs(title = paste("Distribution of", input$summary_var))
    } else {
      # Plot bar chart for categorical variable
      ggplot(subsetted_data$data, aes(x = !!sym(input$summary_var))) +
        geom_bar(fill = "blue", alpha = 0.6) +
        labs(title = paste("Count of", input$summary_var))
    }
  })
  
}

# Run the app
shinyApp(ui, server)
