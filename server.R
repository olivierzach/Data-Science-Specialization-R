#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library("RSQLite"); library(tidyverse); library(ggthemes)

p4k.df <- read.csv("p4k.df.csv", stringsAsFactors = F) %>% 
        dplyr::select(., -X) %>% 
        mutate(pub_date = lubridate::ymd(pub_date),
               genre = factor(genre),
               log.score = log(score)) %>% 
        filter(genre != "NA",
               score > 2.5,
               pub_date > as.Date("2010-01-01")) 

server <- function(input, output) {
        
        # Return the requested dataset ----
        # By declaring datasetInput as a reactive expression we ensure
        # that:
        #
        # 1. It is only called when the inputs it depends on changes
        # 2. The computation and result are shared by all the callers,
        #    i.e. it only executes a single time
        datasetInput <- reactive({
                p4k.filter <- p4k.df %>% 
                        filter(genre == input$Genre,
                               score > 2) %>% 
                        arrange(., -score) %>% 
                        base::as.data.frame()
        })
        
        # Create caption ----
        # The output$caption is computed based on a reactive expression
        # that returns input$caption. When the user changes the
        # "caption" field:
        #
        # 1. This function is automatically called to recompute the output
        # 2. New caption is pushed back to the browser for re-display
        #
        # Note that because the data-oriented reactive expressions
        # below don't depend on input$caption, those expressions are
        # NOT called when input$caption changes
        output$caption <- renderText({
                input$caption
        })
        
        # Generate a summary of the dataset ----
        # The output$summary depends on the datasetInput reactive
        # expression, so will be re-executed whenever datasetInput is
        # invalidated, i.e. whenever the input$dataset changes
        output$summary <- renderPrint({
                dataset <- datasetInput()
                data.frame(Average = round(mean(dataset$score),2),
                Median = median(dataset$score),
                Max = max(dataset$score),
                Min = min(dataset$score),
                BNM_Count = sum(dataset$best_new_music),
                BNM_perc = round(sum(dataset$best_new_music) / nrow(dataset),3)*100,
                Total = nrow(dataset),
                "Cor(Score_Year)" = cor(dataset$score,as.numeric(dataset$pub_year))
                )
        })
        
        # Show the first "n" observations ----
        # The output$view depends on both the databaseInput reactive
        # expression and input$obs, so it will be re-executed whenever
        # input$dataset or input$obs is changed
        output$plot <- renderPlot({
                dataset.p = datasetInput()
                
                ggplot(data = dataset.p, aes(pub_date, score, color= genre)) +
                        geom_point(pch = 21, color = "dark blue", alpha = .5) +
                        theme_few() +
                        geom_smooth(span = .09, color = "blue", lwd = 1.2)+
                        xlab("") +
                        ylab("Review Score") +
                        labs(title = "Pitchfork Review Score by Year",
                              subtitle = "Selected Genre Shown")
                
        })
        
        # Show the first "n" observations ----
        # The output$view depends on both the databaseInput reactive
        # expression and input$obs, so it will be re-executed whenever
        # input$dataset or input$obs is changed
        output$view <- renderTable({
                dataset = datasetInput()
                dataset.filter <- dataset %>% 
                        select(-pub_date)
                head(dataset.filter, n = input$obs)
        
        })
        

}
