# Load necessary libraries
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(readxl)
library(bslib)

# Load the data
super_data <- read_excel("US_Superstore_data.xls")

ui <- fluidPage(
  titlePanel("Superstore Data Exploration App"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdowns for filtering data
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
      tabsetPanel(
        tabPanel("About",
                 h4("App Purpose"),
                 p("This app allows users to explore the Superstore sales data."),
                 p("Data source: [Superstore Sales Data](https://www.kaggle.com/datasets/sdolezel/superstore-sales)"),
                 p("Use the sidebar to filter data and navigate the tabs for different features.")
        ),
        
        tabPanel("Data Download",
                 DT::dataTableOutput("data_table"),
                 downloadButton("download_data", "Download Subsetted Data")),
        
        tabPanel("Data Exploration",
                 h4("Explore Numeric and Categorical Summaries"),
                 
                 # UI for selecting summary options
                 selectInput("numeric_summary_var", "Choose Numeric Variable for Summary:", 
                             choices = names(super_data)[sapply(super_data, is.numeric)]),
                 selectInput("categorical_summary_var", "Choose Categorical Variable to Group By:", 
                             choices = c("None", names(super_data)[sapply(super_data, is.factor)])),
                 actionButton("summary_button", "Generate Summary"),
                 tableOutput("summary_table"),
                 
                 # Plot customization options
                 selectInput("plot_x", "Select X-Axis Variable:", choices = names(super_data)),
                 selectInput("plot_y", "Select Y-Axis Variable:", choices = names(super_data)),
                 selectInput("plot_color", "Select Color Variable:", choices = c("None", names(super_data))),
                 plotOutput("summary_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive data container for subsetted data
  subsetted_data <- reactiveValues(data = super_data)
  
  # Dynamic UI for numeric variable sliders
  output$num_slider1 <- renderUI({
    req(input$num_var1 != "None")
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
  
  # Generate summary statistics for selected numeric variable grouped by categorical variable
  output$summary_table <- renderTable({
    req(input$numeric_summary_var)
    
    data <- subsetted_data$data
    if (input$categorical_summary_var != "None") {
      data %>%
        group_by(!!sym(input$categorical_summary_var)) %>%
        summarise(
          Mean = mean(!!sym(input$numeric_summary_var), na.rm = TRUE),
          Median = median(!!sym(input$numeric_summary_var), na.rm = TRUE),
          SD = sd(!!sym(input$numeric_summary_var), na.rm = TRUE)
        )
    } else {
      data %>%
        summarise(
          Mean = mean(!!sym(input$numeric_summary_var), na.rm = TRUE),
          Median = median(!!sym(input$numeric_summary_var), na.rm = TRUE),
          SD = sd(!!sym(input$numeric_summary_var), na.rm = TRUE)
        )
    }
  })
  
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

# Run the app
shinyApp(ui, server)
