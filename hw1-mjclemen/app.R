library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
budget <- read.csv("2014 Pittsburgh Capital Project Budget.csv")
budget_2 <- as.data.frame(sapply(budget[, c(4:15)], as.numeric))
budget_updated <- cbind(budget[,c(1:3)],budget_2)

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
                           label = "Select Department(s):",
                           choices = sort(unique(budget$"Responsible_Department")),
                           selected = "Public Works"),
        
        # Set x-axis budget range ----------------------------------------------
        sliderInput(inputId = "xrange", 
                    label = "X-Axis Budget Range", 
                    min = 0, max = 400000000, 
                    value = c(2000000,500000)),
        
        # Set y-axis budget range ----------------------------------------------
        sliderInput(inputId = "yrange", 
                    label = "Y-Axis Budget Range", 
                    min = 0, max = 400000000, 
                    value = c(200000,500000)),
        
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
        DT::dataTableOutput(outputId = "budgettable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$scatterplot <- renderPlot({
    
    ggplot(data = budget_updated, aes_string(x = input$x, y = input$y)) +
      geom_point()
    
  })
  
  output$budgettable <- DT::renderDataTable({
    DT::datatable(budget_updated, options = list(orderClasses = TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

