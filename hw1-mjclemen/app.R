library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(rlist)
library(scales)

# Read in csv file containing data on Pittsburgh's capital project budgets
budget <- read.csv("2014 Pittsburgh Capital Project Budget.csv")

# Clean the data by removing dollar signs and commas
for(i in 4:15) {
  budget[,i] <- as.numeric(gsub('[$,]', '', budget[,i]))
}

# Remove any projects that aren't linked to a department
budget <- budget[!(budget$Responsible_Department == ""), ]

# Replace any missing budget amounts with 0
budget[is.na(budget)] <- 0

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel ("Pittsburgh Capital Project Budget Information"),
   
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
        
        # Select functional area to plot in a bar plot -----------------------------------
        radioButtons(inputId = "chooseFunction", 
                    label = "Choose which Functional Area to Plot in a Bar plot:",
                    choices = sort(unique(budget$"Functional_Area")),
                    selected = "Engineering and Construction"),
        
        # Select what department to plot ------------------------
        checkboxGroupInput(inputId = "selected.department",
                           label = "Select Department(s) to view in Data Table and Bar Chart:",
                           choices = sort(unique(budget$"Responsible_Department")),
                           selected = "Public Works"),
        
        downloadButton("downloadBudget", "Download Budget Data File")
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
        
        plotOutput(outputId = "bargraph")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
 # Create a subset of data filtering for selected title types ------
  budget_filtered <- reactive({
    req(input$selected.department) # ensure availablity of value before proceeding
    budget <- droplevels(filter(budget, Responsible_Department %in% input$selected.department))
  })
  
  observe({

    initial.choices <- c("2014 Total Budget" = "X2014_Total",
                "2015 Total Budget" = "X2015_Total",
                "2016 Total Budget" = "X2016_Total",
                "2017 Total Budget" = "X2017_Total",
                "2018 Total Budget" = "X2018_Total",
                "2019 Total Budget" = "X2019_Total",
                "2020 Total Budget" = "X2020_Total")
    
    x.axis.name <- names(which(initial.choices == input$x))
    new.choices <- list.remove(initial.choices, x.axis.name)

    updateSelectInput(session, inputId = "y", choices = new.choices)
  })
   
  output$scatterplot <- renderPlot({
    
    ggplot(data = budget, aes_string(x = input$x, y = input$y)) +
      geom_point() + labs(x = str_replace_all(str_replace_all(input$x, "_", " "),"X",""),
                          y = str_replace_all(str_replace_all(input$y, "_", " "),"X",""),
                          title = "Individual Capital Project Budget Comparison between Years (in $)") + xlim(0, 1000000) + ylim(0, 1000000)
  })
  
  # Plot bar graph, displaying the department(s) selected by the user with the its Total 2020 Budget Cost
  output$bargraph <- renderPlot({
    ggplot(data = budget_filtered(), aes(x = Responsible_Department), y = X2020_Total) +
      geom_bar() + labs(x = "Department", y = "Total Budget for Projects", title = "2020 Total Budget For Given Departments in Pittsburgh") +
      scale_y_continuous(labels = comma)
  })
  
  # Display a data table that shows all of the budget info from 2014 - 2020 for each indiviudal projects, filtered on the project's
  # department. The departments to be shown are selected by the user
  output$budgettable <- DT::renderDataTable({
    DT::datatable(data = budget_filtered()[,1:10], options = list(orderClasses = TRUE))
  })
  
  # Downloadable csv of budget data filtered by department.
  # Note -- filename and file type (csv) work in web browser, not RStudio. RStudio glitch from what I have read about it
  output$downloadBudget <- downloadHandler(
    filename = function() {
      paste("2014 Pittsburgh Capital Project Budget -", input$selected.department,
            ".csv", sep = "")
    },
    content = function(file) {
      write.csv(budget_filtered(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

