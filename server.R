#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

library(doBy)
data("child_growth")
my_data <- child_growth
my_data$gender <- factor(my_data$gender)
male_data <- subset(my_data, gender == "boy")
female_data <- subset(my_data, gender == "girl")



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    model1_male <- lm(height ~ age, data = male_data)
    model1_female <- lm(height ~ age, data = female_data)
    
    
    # Fit separate models
    model2_male <- nls(height ~ Asym / (1 + exp((xmid - age)/scal)),
                      data = male_data,
                      start = list(Asym = 180, xmid = 13, scal = 2))
    
    model2_female <- nls(height ~ Asym / (1 + exp((xmid - age)/scal)),
                        data = female_data,
                        start = list(Asym = 170, xmid = 11, scal = 2))
    
    
    age_seq <- seq(1, 18, length.out = 200)
    pred_male <- data.frame(
      age = age_seq,
      height = predict(model2_male, newdata = data.frame(age = age_seq)),
      gender = "boy"
    )
    
    pred_female <- data.frame(
      age = age_seq,
      height = predict(model2_female, newdata = data.frame(age = age_seq)),
      gender = "girl"
    )
    
    model1pred_male <- reactive({
      ageInput <- input$age
      predict(model1_male, newdata = data.frame(age = ageInput))
    })
    
    model2pred_male <- reactive({
      ageInput <- input$age
      predict(model2_male, newdata = data.frame(age = ageInput))
    })

    model1pred_female <- reactive({
      ageInput <- input$age
      predict(model1_female, newdata = data.frame(age = ageInput))
    })
    
    model2pred_female <- reactive({
      ageInput <- input$age
      predict(model2_female, newdata = data.frame(age = ageInput))
    })
    
    
    output$plot_male <- renderPlot({
        set.seed(123) 
        ageInput <- input$age
        
        p <- ggplot(male_data, aes(x = age, y = height)) +
          geom_point(position = "jitter", alpha = 0.5, color = "lightblue") +
          labs(
            x = "Age", 
            y = "Height"
          ) +
          xlim(c(1, 18)) +
          ylim(c(20, 210)) +
          theme_classic() +
          theme(
            text = element_text(size = 20)
          )
        
        # Create a small data frame to plot the linear model line with color mapping for legend
        linear_line_df <- data.frame(
          age = c(min(male_data$age), max(male_data$age))
        )
        linear_line_df$height <- coef(model1_male)[1] + coef(model1_male)[2] * linear_line_df$age
        linear_line_df$model <- "Model 1: Linear"
        
        # Add linear model line as geom_line with color mapped to model for legend
        p <- p + geom_line(data = linear_line_df, aes(x = age, y = height, color = model), size = 1)
        
        # Add nonlinear model line with color mapped to model for legend
        pred_male$model <- "Model 2: Logistic"  # make sure this column exists in pred_male
        p <- p + geom_line(data = pred_male, aes(x = age, y = height, color = model), size = 1)
        
        p <- p + scale_color_manual(
          name = "Model Type",
          values = c("Model 1: Linear" = "red", "Model 2: Logistic" = "blue")
        )
        p <- p + geom_point(aes(x = ageInput, y = model1pred_male()), color = "red", size = 4)
        p <- p + geom_point(aes(x = ageInput, y = model2pred_male()), color = "blue", size = 4)
        
        p
    })
    
    output$pred1_male <- renderText({
      paste(round(model1pred_male(), 1), "cm")
    })
    
    output$pred2_male <- renderText({
      paste(round(model2pred_male(), 1), "cm")
    })

    output$plot_female <- renderPlot({
      set.seed(123) 
      ageInput <- input$age
      
      p <- ggplot(female_data, aes(x = age, y = height)) +
        geom_point(position = "jitter", alpha = 0.5, color = "lightpink") +
        labs(
          x = "Age", 
          y = "Height"
        ) +
        xlim(c(1, 18)) +
        ylim(c(20, 210)) +
        theme_classic() +
        theme(
          text = element_text(size = 20)
        )
      
      # Create a small data frame to plot the linear model line with color mapping for legend
      linear_line_df <- data.frame(
        age = c(min(female_data$age), max(female_data$age))
      )
      linear_line_df$height <- coef(model1_female)[1] + coef(model1_female)[2] * linear_line_df$age
      linear_line_df$model <- "Model 1: Linear"
      
      # Add linear model line as geom_line with color mapped to model for legend
      p <- p + geom_line(data = linear_line_df, aes(x = age, y = height, color = model), size = 1)
      
      # Add nonlinear model line with color mapped to model for legend
      pred_female$model <- "Model 2: Logistic"  # make sure this column exists in pred_male
      p <- p + geom_line(data = pred_female, aes(x = age, y = height, color = model), size = 1)
      
      p <- p + scale_color_manual(
        name = "Model Type",
        values = c("Model 1: Linear" = "red", "Model 2: Logistic" = "blue")
      )        
        
      p <- p + geom_point(aes(x = ageInput, y = model1pred_female()), color = "red", size = 4)
      p <- p + geom_point(aes(x = ageInput, y = model2pred_female()), color = "blue", size = 4)
      
      p
    })
    
    output$pred1_female <- renderText({
      paste(round(model1pred_female(), 1), "cm")
    })
    
    output$pred2_female <- renderText({
      paste(round(model2pred_female(), 1), "cm")
    })
}
)