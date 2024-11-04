 
# App Requirements
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(readxl)

# Load your dataset
# Load the Data 
super_data <- read_excel("US_Superstore_data.xls")
head(super_data)
ui <- fluidPage(
  titlePanel("Data Exploration App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cat_var1", "Select Categorical Variable 1:",
                  choices = unique(super_data$Category), selected = "Furniture"),
      selectInput("cat_var2", "Select Categorical Variable 2:",
                  choices = unique(super_data$Segment), selected = "Consumer"),
      uiOutput("numeric_var1"),
      uiOutput("numeric_var2"),
      actionButton("subset_button", "Subset Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 h4("App Purpose"),
                 p("This app allows users to explore the Superstore sales data."),
                 p("Data source: [Superstore Sales Data](https://www.kaggle.com/datasets/sdolezel/superstore-sales)"),
                 img(src = "path_to_image/logo.png", height = 100)),  # Adjust image path
        tabPanel("Data Download",
                 DT::dataTableOutput("data_table"),
                 downloadButton("download_data", "Download Subsetted Data")),
        tabPanel("Data Exploration",
                 h4("Explore Numeric and Categorical Summaries"),
                 uiOutput("summary_selector"),
                 plotOutput("summary_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Dynamic UI for numeric variable selection
  output$numeric_var1 <- renderUI({
    numericInput("num_var1", "Select Numeric Variable 1:", value = 0)
  })
  
  output$numeric_var2 <- renderUI({
    numericInput("num_var2", "Select Numeric Variable 2:", value = 0)
  })
  
  # Reactive values for subsetted data
  subsetted_data <- reactiveValues(data = super_data)
  
  observeEvent(input$subset_button, {
    req(input$cat_var1, input$cat_var2)  # Ensure inputs are available
    
    # Subset data based on selections
    subsetted_data$data <- super_data %>%
      filter(Category == input$cat_var1 & Segment == input$cat_var2)
  })
  
  # Render data table
  output$data_table <- DT::renderDataTable({
    req(subsetted_data$data)  # Ensure data is available
    DT::datatable(subsetted_data$data)
  })
  
  # Download handler for subsetted data
  output$download_data <- downloadHandler(
    filename = function() { paste("subsetted_data", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(subsetted_data$data, file, row.names = FALSE)
    }
  )
  
  # Summary tab functionality
  output$summary_selector <- renderUI({
    tagList(
      selectInput("summary_var", "Choose Summary Variable:", choices = names(super_data)),
      actionButton("summary_button", "Generate Summary")
    )
  })
  
  output$summary_plot <- renderPlot({
    req(input$summary_var)  # Ensure the variable is selected
    ggplot(subsetted_data$data, aes_string(x = input$summary_var)) +
      geom_histogram(fill = "blue", alpha = 0.5) +
      labs(title = paste("Distribution of", input$summary_var))
  })
}

shinyApp(ui, server)






















