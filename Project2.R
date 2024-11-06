# Project 2



#Get the data



#install.packages("ggside")
#library(ggside)
#install.packages("ggpubr")
#install.packages("viridis")
#library(viridis)
#library(ggpubr)
#install.packages("ggstatsplot")  
library(ggstatsplot)              
#install.packages("palmerpenguins")
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

# Load the Data 
super_data <- read_excel("US_Superstore_data.xls")
head(super_data)
View(super_data)
names(super_data) <- make.names(names(super_data))
head(super_data)

# check is there any missing data
sum_na <- function(...) {
  sum(is.na(...))
}
na_counts <- super_data %>%
  summarize(across(everything(), sum_na))
print(na_counts)
#colSums(is.na(super_data)) # there is no missing data


# Produce numerical and graphical summaries to investigate the data
#  One-way 
# Frequency of each segment
#----table(super_data$Segment)
#Frequency of each category
#------table(super_data$Category)

super_data |>
  group_by(Segment) |>
  summarize(count = n())

#Two-way contingency tables
# cross tabulation of segment and category
#-----table(super_data$Segment,super_data$Category)

super_data |>
  group_by(Segment, Category) |>
  summarize(count = n()) |>
  pivot_wider(names_from = Category, values_from = count)



# Numerical summaries (means, medians, sds, etc.) for quantitative variables at levels of categorical variables
# summarize the center and spread of Sales and Profit
summary_stats <- super_data |>
  group_by(State,Category) |>
  summarize(
    across(c(Sales, Profit), .fns = list("mean" = mean,
                             "median" = median,
                             "var" = var,
                             "sd" = sd,
                             "IQR" = IQR), .names = "{.fn}_{.col}")
           )
print(summary_stats)

    
# Create at least six plots.    
    
# check total sales per category for each segment
# 1. Bar plot
g_bar <- ggplot(
  super_data, aes(x = Category, fill = Segment)) +
  geom_bar(position = "dodge") +
  labs(title = "Total sales by Category and Segment",
       x = "Category",
       y = "Total Sales") +
  theme_minimal()
print(g_bar)

#2. Box Plot
# Distribution of Profit by category and segment 
g_box <- ggplot(
  super_data, aes(x = Category, y = Profit, fill = Segment)) +
  geom_boxplot() +
  labs(title = "Profit Distribution by Category and Segment",
       x = "Category",
       y = "Profit") 
print(g_box) +
  theme_minimal()

# 3. Scatter Plot
# scatter Plot of sales versus points,:- points colored by segment and smoothing line

g_scatter <- ggplot(super_data, aes(x = Sales, y = Profit, color = Segment)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~Segment)
  labs(title = "Sales vs. Profit ",
       x = "Sales",
       y = "Profit") + 
  theme_minimal()

print(g_scatter) 

# 4. Faceted Scatter Plot
# Faceted scatter Plot of sales vs. Profit, faceted by segment

ggplot(super_data, aes(x = Sales, y = Profit)) +
  geom_point(aes(color = Category), alpha = 0.6) +
  facet_wrap(~ Segment) +
  labs(title = "Sales vs. Profit by Segment",
       x = "Sales",
       y = "Profit") +
  theme_minimal()


# 5. Create a 

# Create a cowplot 
#install.packages("cowplot")
library(cowplot)

theme_set(theme_cowplot())
ggplot(super_data, aes(Sales, fill = Category)) +
  geom_density(alpha = 0.5) + 
  
  #geom_point() +
  #theme_cowplot()
 # theme_minimal_grid(12)
  #geom_point(alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  #theme_minimal_hybrid(12)
  theme_minimal_grid(12)





# Custom theme based on theme_cowplot with gridlines


# Apply the custom theme in your plot
ggplot(super_data, aes(Sales, Profit, color = Category)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  custom_theme +
  labs(title = "Scatter Plot of Sales vs. Profit by Category")


################################
install.packages("magick")
library(magick)
p <- ggplot(super_data, aes(Sales, Profit)) + 
  geom_point(size = 1.5, color = "blue") +
  theme_cowplot(12)

logo_file <- system.file("extdata", "logo.png", package = "cowplot")

ggdraw(p) + 
  draw_image(logo_file, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2)

##########################
#library("palmerpenguins")

# Create the plot

#library(palmerpenguins)
# Ensure the necessary libraries are loaded
library(ggplot2)
library(ggplot2)

super_data$Category <- as.factor(super_data$Category)

# Create the plot with density contours and facets by Category
ggplot(super_data, aes(Sales, Profit)) +
  stat_density_2d(aes(fill = Category), geom = "polygon", color = "white") +  # Fill based on Category
  geom_point(aes(color = Category), shape = 21, alpha = 0.5) +  # Scatter points colored by Category
  coord_cartesian(xlim = c(160, 240), ylim = c(30, 70)) +  # Set coordinate limits
  facet_wrap(vars(Category)) +  # Create facets for each category
  theme_minimal() +  # Use a minimal theme
  labs(title = "Density Plot of Sales vs. Profit by Category",  # Title
       x = "Sales",  # X-axis label
       y = "Profit",  # Y-axis label
       fill = "Category",  # Fill legend label
       color = "Category")  # Color legend label for points

str(super_data)
################################
# 5. Patchwork

#install.packages("devtools")
library(patchwork)
# Sample plots based on super_data
# 1. Scatter plot
scatter_plot <- ggplot(super_data, aes(Sales, Profit, color = Category)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sales vs Profit") +
  theme_minimal()

# 2. Density plot
density_plot <- ggplot(super_data, aes(Sales, fill = Category)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Sales") +
  theme_minimal()

# 3. Bar plot (example)
bar_plot <- ggplot(super_data, aes(x = Category)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Count of Categories") +
  theme_minimal()

# 4. smooth graph
smooth_plot <- ggplot(super_data, aes(Profit, Sales)) +
  geom_smooth()

# Combine the plots using patchwork
combined_plot <- scatter_plot + density_plot + bar_plot + smooth_plot +
  plot_layout(ncol = 2) +  # Adjust the layout as needed
  plot_annotation(title = "Super Data Visualizations")  # Overall title

# Display the combined plot
print(combined_plot)


######## 6. 

#install.packages("ggsignif")
library(ggsignif)
# Ensure 'Segment' is a factor
super_data$Segment <- as.factor(super_data$Segment)

# Remove NA values
#super_data <- na.omit(super_data)

# Check unique levels in Segment
print(unique(super_data$Segment))  # Ensure these levels match your comparisons

# Create a box plot with significance annotations
ggplot(super_data, aes(x = Segment, y = Sales)) +
  geom_boxplot(aes(fill = Segment)) +  # Use Segment for filling
  geom_signif(comparisons = list(
    c("Consumer", "Home Office","Corporate"),          # Update these based on your actual factor levels
    c("Consumer", "Home Office","Corporate")
  ), map_signif_level = TRUE) +  # Automatically map significance levels
  ylim(NA, 6.3) +                # Set limits for the y-axis
  labs(title = "Box Plot of Sales by Segment",
       x = "Segment",
       y = "Sales") +
  theme_minimal()

###################################################################

# App Requirements
# Load necessary libraries
#install.packages("readxl")
#install.packages("shiny")      
#install.packages("dplyr")      
#install.packages("DT")          
#install.packages("ggplot2")    
#install.packages("readxl")      
#install.packages("bslib")      


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


# Define Server
server <- function(input, output, session) {
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

shinyApp(ui, server)