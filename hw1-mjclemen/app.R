library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)

# read in csv file containing data on Pittsburgh's capital project budgets
budget <- read.csv("2014 Pittsburgh Capital Project Budget.csv")

# clean the data by removing dollar signs and commas
for(i in 4:15) {
  budget[,i] <- as.numeric(gsub('[$,]', '', budget[,i]))
}

# replace any missing budgets with 0
budget[is.na(budget)] <- 0

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Pittsburgh's 2014 Capital Project Budget"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Select variable for x-axis ----------------------------------
        selectInput(inputId = "x", 
                    label = "X-axis:",
                    choices = c("2014 Total Budget" = "X2014_Total",
                              "2015 Total Budget" = "X2015_Total",
                              "2016 Total Budget" = "X2016_Total",
                              "2017 Total Budget" = "X2017_Total",
                              "2018 Total Budget" = "X2018_Total",
                              "2019 Total Budget" = "X2019_Total",
                              "2020 Total Budget" = "X2020_Total"),
                    selected = "X2014_Total"),
        
        # Select variable for y-axis ----------------------------------
        selectInput(inputId = "y", 
                    label = "Y-axis:",
                    choices = c("2014 Total Budget" = "X2014_Total",
                                "2015 Total Budget" = "X2015_Total",
                                "2016 Total Budget" = "X2016_Total",
                                "2017 Total Budget" = "X2017_Total",
                                "2018 Total Budget" = "X2018_Total",
                                "2019 Total Budget" = "X2019_Total",
                                "2020 Total Budget" = "X2020_Total"),
                    selected = "X2020_Total"),
        
        # Select what department to plot ------------------------
        checkboxGroupInput(inputId = "selected_department",
                           label = "Select Department(s) to view in Data Table:",
                           choices = sort(unique(budget$"Responsible_Department")),
                           selected = "Public Works"),
        
        # Set alpha level ---------------------------------------------
        sliderInput(inputId = "alpha", 
                    label = "Alpha:", 
                    min = 0, max = 5, 
                    value = 3)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # Show scatterplot --------------------------------------------
        plotOutput(outputId = "scatterplot"),
        
        # Line break for visual
        br(),
        
        # Show data table ---------------------------------------------
        DT::dataTableOutput(outputId = "budgettable"),
        
        # Line break for visual
        br(),
        
        plotOutput(outputId = "barplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types ------
  budget_subset <- reactive({
    req(input$selected_department) # ensure availablity of value before proceeding
    filter(budget_updated, input$selected_department)
  })
  
  # observe({
  #   
  #   initial_choices <- c("2014 Total Budget" = "X2014_Total",
  #               "2015 Total Budget" = "X2015_Total",
  #               "2016 Total Budget" = "X2016_Total",
  #               "2017 Total Budget" = "X2017_Total",
  #               "2018 Total Budget" = "X2018_Total",
  #               "2019 Total Budget" = "X2019_Total",
  #               "2020 Total Budget" = "X2020_Total")
  #   
  #   updateSelectInput(session, inputId = y, choices = remove()
  # })
   
  output$scatterplot <- renderPlot({
    
    ggplot(data = budget_updated, aes_string(x = input$x, y = input$y)) +
      geom_point() +
      scale_x_continuous(limits = c(min(as.numeric(input$x)), max(as.numeric(input$x)))) +
      scale_y_continuous(limits = c(min(as.numeric(input$y)), max(as.numeric(input$y))))
    
  })
  
  # output$barplot <- renderPlot({
  #   value <- count(budget_updated, "Selected_Department")
  #   
  #   ggplot(budget_updated) + geom_bar(aes(input$selected_department, value))
  # })
  
  output$budgettable <- DT::renderDataTable({
    DT::datatable(budget_subset, options = list(orderClasses = TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

