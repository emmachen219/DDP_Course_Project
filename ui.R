#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Predicting Child Height"),
    tags$p("This application allows you to explore height prediction models."),
  

    # Sidebar with a slider input for number of age
    sidebarLayout(
        sidebarPanel(
          width = 3, 
          helpText("Select an age to see model predictions."),
            sliderInput("age",
                        "What's the child's age in years:",
                        min = 1,
                        max = 18,
                        step = 0.25,
                        value = 5
                        ),
            submitButton("Submit")
        ),

        # Show a plot of model fitting
        mainPanel(
            width = 9, 
            tabsetPanel(type = "tabs",
                        tabPanel("About",
                                 fluidPage(
                                   h3("This app models height vs age using linear and logistic models."),
                                   h3("How to Use"),
                                   p("1. Choose an age using the slider bar "),
                                   p("2. Submit"),
                                   h3("Data Source"),
                                   p("Simulated data is obtained from the child_growth dataset in the doBy package.")
                                 )
                        ),
                        tabPanel("Boy", 
                                 plotOutput("plot_male", height = "500px", width = "600px"),
                                 h3("Predicted Boy's Height from Model 1:"),
                                 textOutput("pred1_male"),
                                 h3("Predicted Boy's Height from Model 2:"),
                                 textOutput("pred2_male")
                                 ),
                        tabPanel("Girl",
                                 plotOutput("plot_female", height = "500px", width = "600px"),
                                 h3("Predicted Girl's Height from Model 1:"),
                                 textOutput("pred1_female"),
                                 h3("Predicted Girl's Height from Model 2:"),
                                 textOutput("pred2_female")
                                 )
              
            )
            
        )
    )
)
)