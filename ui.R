
# Text Prediction Shiny App UI -------------------------------------------------------------------

# goal: set up UI for the Text Prediction Application
# will need a text input box, submit button, and printed output from the prediction algorithm
# https://shiny.rstudio.com
# https://www.coursera.org/learn/data-science-project/peer/EI1l4/final-project-submission


# ui shiny text prediction   --------------------------------------------------------------------

library(shiny)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
        
  # panel text header
  headerPanel('Next Word Algorithm'),
  
  # define the sidebar layout
  sidebarPanel(
            
       # create input box - gram is the label to feed the server
       textInput("gram", label = 'Enter Text',
                value = "Enter your text here"),

                   
       # define submit button to initialize the prediction algorithm
       actionButton("click", "Click to Predict"),
       
       # define the help text to be shown            
       hr(), 
       helpText('Enter a text string above. 
                 The next word prediction will be printed on the screen to the right.')
    ),
    
  
   # main panel layout
   mainPanel(
           
       # regular prediction - look for 'next_word' on server side   
       h3('Next Word Prediction:'),  
       verbatimTextOutput("next_word"), 
       
       # user text prediction - look for 'user_word' on server
       # h3('User Text Prediction:'),
       # verbatimTextOutput('user_word'), 
       # 

       # titles and links to follow predictions
       br(),
       h5('Prediction generated is based on frequency tables composed of Twitter, News article, 
          and Blogs corpus'),
       br(),
       h5('If no exact match - model will utilize a distance algorithm to try and match the closest
          string possible'),
       br(),
       h5('All user text inputs will be added to the User Text frequency table. Algorithm will 
          scan this database first for the most frequent text string matching the User\'s 
          own vernacular'),
       br(),
       h5('This Word Prediction Algorithm will learn the way YOU speak and recommend \'next words\'
           based on your past input')
       
       
  )
))










