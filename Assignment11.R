# Assignment 11 -  ShinyR Application
# Install packages

install.packages("shiny")
install.packages("dplyr")
install.packages("ggplot2")

# Import libraries

library(shiny)
library(dplyr)
library(ggplot2)

# Use setwd() to point R to this correct folder
setwd("C:/Users/skrai/Desktop/UCBerkeley/Week11")

# Import income.csv to an object called income
income <- read.csv("income.csv")

# Shiny apps are composed of two pieces:
# an object called ui
# a function called server


# ---- Basic App ----
# Define the user interface (ui)

ui <- fluidPage(
  # App Title
  titlePanel("Module 10 Assignment"),
  # Page Layout
  sidebarLayout(
    # Side Panel
    sidebarPanel(
      # Interactive piece 1: inputID = "subset_income"
      selectInput(inputId = "subset_income",
                  label = "subset_income",
                  choices = c("<=50K", ">50K"),
                  multiple = FALSE),
      # Interactive piece 2: inputId = "set_yaxis"
      selectInput(inputId = "set_yaxis",
                  label = "set_yaxis", 
                  choices = c("hours_per_week","capital_loss")), 
      # Interactive piece 3: inputId = "subset_occupation"
      checkboxGroupInput(inputId = "subset_occupation",
                         label = "Include Occupations:",
                         choices = unique(income$occupation), 
                         selected = unique(income$occupation))),
    # Main panel
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)


# Define the server function

server <- function(input, output) 
{
  # Create a reactive subset of the data
  create_subset <- reactive(income %>% 
                              filter(capital_loss > 0 & 
                                       income == input$subset_income &
                                       occupation %in% input$subset_occupation))
  
  # Render Plot
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                                  # Boxplot of x = occupation, y = defined by input
                                  geom_boxplot(aes_string(x = "occupation", y = input$set_yaxis)) +
                                  theme_bw(18) +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

# Run app
shinyApp(ui,server)
