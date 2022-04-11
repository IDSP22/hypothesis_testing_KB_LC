#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    race_dat <- reactive({
        dat <- tibble(
            animals = c(rep("tortoise", times = input$tort_races), 
                         rep("hare", times = input$hare_races)),
            time = c(rnorm(input$tort_races, mean = input$tort_speed, sd = 3),
                     rnorm(input$hare_races, mean = input$hare_speed, sd = 3))
        )
        
        return(dat)
    })
    
    t_test_output <- reactive({
        t_test <- t.test(race_dat()$time[which(race_dat()$animals == "tortoise")],
                         race_dat()$time[which(race_dat()$animals == "hare")],
                         alternative = "less",
                         conf.level = input$sig_level)
    })

    output$distPlot <- renderPlot({
        ggplot(race_dat(), aes(x = time, group = animals, fill = animals)) +
            geom_histogram(alpha = 0.5, position = "identity")
    })
    
    output$p_val <- renderPrint(t_test_output()$p.val)

})


## 
