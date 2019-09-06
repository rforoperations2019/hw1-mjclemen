library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(rlist)

# read in csv file containing data on Pittsburgh's capital project budgets
budget <- read.csv("2014 Pittsburgh Capital Project Budget.csv")

# clean the data by removing dollar signs and commas
for(i in 4:15) {
  budget[,i] <- as.numeric(gsub('[$,]', '', budget[,i]))
}

budget <- budget[!(budget$Responsible_Department == ""), ]

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
                    label = "X-axis for Scatterplot:",
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
                    label = "Y-axis for Scatterplot:",
                    choices = c("2014 Total Budget" = "X2014_Total",
                                "2015 Total Budget" = "X2015_Total",
                                "2016 Total Budget" = "X2016_Total",
                                "2017 Total Budget" = "X2017_Total",
                                "2018 Total Budget" = "X2018_Total",
                                "2019 Total Budget" = "X2019_Total",
                                "2020 Total Budget" = "X2020_Total"),
                    selected = "X2020_Total"),
        
        # Select variable for color -----------------------------------
        selectInput(inputId = "color_by", 
                    label = "Choose how to color the scatterplot:",
                    choices = c("Responsible Department" = "Responsible_Department", 
                                "Functional Area" = "Functional_Area"),
                    selected = "Functional_Area"),
        
        # Select what department to plot ------------------------
        checkboxGroupInput(inputId = "selected_department",
                           label = "Select Department(s) to view in Data Table:",
                           choices = sort(unique(budget$"Responsible_Department")),
                           selected = "Public Works"),
        
        sliderInput(inputId = "year_bargraph",
                  label = "Choose a year to plot in bar graph (2014-2020)",
                  min = 2014, max = 2020, value = 2015),
        
        downloadButton("download_button", "Download Budget Data File")
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
  budget_filtered <- reactive({
    req(input$selected_department) # ensure availablity of value before proceeding
    budget <- droplevels(filter(budget, Responsible_Department %in% input$selected_department))
    # cat(file=stderr(), "budget function returns: ", budget$Functional_Area, "\n")
  })
  
  observe({

    initial_choices <- c("2014 Total Budget" = "X2014_Total",
                "2015 Total Budget" = "X2015_Total",
                "2016 Total Budget" = "X2016_Total",
                "2017 Total Budget" = "X2017_Total",
                "2018 Total Budget" = "X2018_Total",
                "2019 Total Budget" = "X2019_Total",
                "2020 Total Budget" = "X2020_Total")
    
    x_axis_name <- names(which(initial_choices == input$x))
    new_choices <- list.remove(initial_choices, x_axis_name)

    updateSelectInput(session, inputId = "y", choices = new_choices)
  })
   
  output$scatterplot <- renderPlot({
    
    ggplot(data = budget, aes_string(x = input$x, y = input$y)) +
      geom_point() + labs(title = "Capital Project Budget Comparison between Years")
    # +
    #   scale_x_continuous(limits = c(min(as.numeric(input$x)), max(as.numeric(input$x)))) +
    #   scale_y_continuous(limits = c(min(as.numeric(input$y)), max(as.numeric(input$y))))
    
  })
  
  output$barchart <- renderPlot({
    
    ggplot(data = budget, aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(title = "Capital Project Budget Comparison between Years")
    +
      scale_x_continuous(limits = c(0, max(as.numeric(input$x)))) +
      scale_y_continuous(limits = c(0, max(as.numeric(input$y))))
    
  })
  
  output$barplot <- renderPlot({

    budget_sum <- tapply(budget_filtered()$X2014_Total, budget_filtered()$Responsible_Department, FUN=sum)

    cat(file=stderr(), "budget sum is ", budget_sum, "\n")
    
    cat(file=stderr(), "department is ", unique(budget_filtered()$Responsible_Department), "\n")
    
    ggplot(data = budget_filtered(), aes(x = unique(budget_filtered()$Responsible_Department), y = budget_sum)) +
      geom_col() + xlab("Department") + ylab("Total Budget for Projects")
  })
  
  output$budgettable <- DT::renderDataTable({
    DT::datatable(data = budget_filtered()[,1:10], options = list(orderClasses = TRUE))
  })
  
  # Downloadable csv of budget data filtered by department
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("2014 Pittsburgh Capital Project Budget -", input$selected_department,
            ".csv", sep = "")
    },
    content = function(file) {
      write.csv(budget, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

