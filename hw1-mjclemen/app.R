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
        
        
        # Select what department to plot ------------------------
        checkboxGroupInput(inputId = "selected.department",
                           label = "Select Department(s) to view in Data Table and Bar Chart:",
                           choices = sort(unique(budget$"Responsible_Department")),
                           selected = c("Public Works", "Finance")),
        
        # Select year to display projects in a boxplot -----------------------------------
        radioButtons(inputId = "x.box", 
                    label = "Choose which Year to View Project Budgets Disparity in Boxplot:",
                    choices = c("2014" = "X2014_Total",
                                "2015" = "X2015_Total",
                                "2016" = "X2016_Total",
                                "2017" = "X2017_Total",
                                "2018" = "X2018_Total",
                                "2019" = "X2019_Total",
                                "2020" = "X2020_Total"),
                    selected = "X2016_Total"),
        
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
        
        # Show bar graph ---------------------------------------------
        plotOutput(outputId = "barchart"),
        
        # Line break for visual
        br(),
        
        plotOutput(outputId = "boxplot")
      )
   )
)

# Define server logic required to plot graphs, chart, and dataframe
server <- function(input, output, session) {
  
 # Create a subset of data filtering for selected title types ------
  budget_filtered <- reactive({
    req(input$selected.department) # ensure availablity of value before proceeding
    budget <- droplevels(filter(budget, Responsible_Department %in% input$selected.department))
  })
  
  # Update the y-axis options, given the selected x-axis options. Avoid plotting the same year on both x and y axis
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
   
  # Plot scatterplot, displaying individual project's budgets between any two years from 2014 - 2020
  output$scatterplot <- renderPlot({
    ggplot(data = budget, aes_string(x = input$x, y = input$y)) +
      geom_point() + labs(x = str_replace_all(str_replace_all(input$x, "_", " "),"X",""),
                          y = str_replace_all(str_replace_all(input$y, "_", " "),"X",""),
                          title = "Individual Capital Project Budget Comparison between Years (in $)") +
      xlim(0, 1000000) + ylim(0, 1000000)
  })
  
  # Plot bar chart, displaying the department(s) selected by the user with the its Total 2020 Budget Cost
  output$barchart <- renderPlot({
    ggplot(data = budget_filtered(), aes(x = Responsible_Department, y = X2020_Total)) +
      geom_bar(stat="identity") + labs(x = "Department(s)",
                        y = "Total Budget for Projects in a Given Department",
                        title = "2020 Total Budget For Given Departments in Pittsburgh") +
      scale_y_continuous(labels = comma)
  })
  
  output$boxplot <- renderPlot({
    x.axis <- switch(input$x.box,
                     "X2014_Total" = 4,
                     "X2015_Total" = 5,
                     "X2016_Total" = 6,
                     "X2017_Total" = 7,
                     "X2018_Total" = 8,
                     "X2019_Total" = 9,
                     "X2020_Total" = 10)
    
    
    options(scipen=10)
    boxplot(budget[,x.axis],
            xlab = str_replace_all(str_replace_all(input$x.box, "_Total", " Projects"),"X",""),
            ylab = "Budget ($)",
            main = "Project Budgets in a Given Year")
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

