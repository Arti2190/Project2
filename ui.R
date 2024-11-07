#install.packages("rsconnect")

library(rsconnect)
#install.packages(c("readxl","shiny","dplyr","DT","ggplot2","readxl","bslib"))
#install.packages("shiny")      
#install.packages("dplyr")      
#install.packages("DT")          
#install.packages("ggplot2")    
#install.packages("readxl")      
#install.packages("bslib")      

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


# Load the data and save it into super_data
super_data <- read_excel("US_Superstore_data.xls")

# Define UI
ui <- fluidPage(
  titlePanel("Superstore Data Exploration App"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdowns for filtering data :- Choose 2 categorical variables 
      selectInput("cat_var1", "Select Category:", choices = c("All", unique(super_data$Category))),
      selectInput("cat_var2", "Select Segment:", choices = c("All", unique(super_data$Segment))),
      
      # Dynamic UI for numeric variables (displayed as sliders after selection)
      selectInput("num_var1", "Select Numeric Variable 1:", choices = c("None", "Sales", "Profit")),
      # show the slider after selecting the numerical variable1
      uiOutput("num_slider1"),
      
      selectInput("num_var2", "Select Numeric Variable 2:", choices = c("None", "Quantity", "Discount")),
      #show the slider after selecting the numerical variable2
      uiOutput("num_slider2"),
      
      # Action button to subset data according to selection 
      actionButton("subset_button", "Subset Data")
    ),
    
    # Main Panel:-- here we are using different tabs.  
    mainPanel(
      tabsetPanel(
        # About tab:-- Describe the purpose of the app
        tabPanel("About",
                 h4("App Purpose"),
                 p("This app allows users to explore the Superstore sales data. Here they can find the information with different category and segment. Acoording to that can see how much they got the profit and sales."),
                 # Find the link for data and its source-- check the link for more information
                 p("Data source: [Superstore Sales Data](https://www.kaggle.com/datasets/juhi1994/superstore/data)"),
                 # here is different tabs according to choice can navigate 
                 p("Use the sidebar to filter data and navigate the tabs for different features."),
                 # show the picture of logo
                 #img(src = "superstore_logo.jpg", height = "200px", alt = "SuperStore Data Logo")
                 tags$div(
                   style = "display: flex; justify-content: center; align-items: center;",
                   img(src = "superstore_logo.jpg", style = "max-width: 100%; height: auto;", alt = "SuperStore Data Logo")
                 )
                 
        ),
        # Data download Tab :--click the data download tab and we can save the filtered data after clicking on the tab
        
        tabPanel("Data Download",
                 DT::dataTableOutput("data_table"),
                 downloadButton("download_data", "Download Subsetted Data")),
        #Data exploration Tab:--It represent the numerical and graphical representation of the data according to filtered data
        tabPanel("Data Exploration",
                 h4("Explore Numeric and Categorical Summaries"),
                 
                 # UI for selecting summary options
                 selectInput("numeric_summary_var", "Choose Numeric Variable for Summary:", 
                             choices = names(super_data)[sapply(super_data, is.numeric)]),
                 # selectInput("categorical_summary_var", "Choose Categorical Variable to Group By:", 
                 #    choices = c("None", names(super_data)[sapply(super_data, is.factor)])),
                 # Select category for the categorical variable1
                 selectInput("cat_var1", "Select Category:", 
                             choices = c("All", levels(factor(super_data$Category)))),
                 # Select segment for the categorical variable2
                 selectInput("cat_var2", "Select Segment:", 
                             choices = c("All", levels(factor(super_data$Segment)))),
                 # Click on action button for getting the summary of the data
                 actionButton("summary_button", "Generate Summary"),
                 tableOutput("summary_table"),
                 
                 # Graph Plots according to choice
                 selectInput("plot_x", "Select X-Axis Variable:", choices = names(super_data)),
                 selectInput("plot_y", "Select Y-Axis Variable:", choices = names(super_data)),
                 selectInput("plot_color", "Select Color Variable:", choices = c("None", names(super_data))),
                 plotOutput("summary_plot")
        )
      )
    )
  )
)

