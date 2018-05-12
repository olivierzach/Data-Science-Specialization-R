## Developing Data Products
# Data Science Specialty Course 9 of 10
# https://github.com/DataScienceSpecialization/Developing_Data_Products
# http://datasciencespecialization.github.io
# http://shiny.rstudio.com/tutorial/
# http://shiny.rstudio.com/gallery/
# https://plot.ly/r/shiny-tutorial/
# http://www.rmdk.ca/boosting_forests_bagging.html


## Welcome to Developing Data Products
# building tools to improve the data analysis process
# old content is now updated
# topics we will be covering:
        # rmarkdown
        # shiny apps
        # interactive graphics: leaflet, plotly
        # googleViz
        # how to use swirl and swirlify

# data products is the production output from an analysis
# data products automate complex analysis tasks
# data products expand the utility of our data analysis
# story telling with data is what we want with a data product!
# the key in data science is science
        # key ideas in working with data in a scientific way (reproducible)
        # introduction to tools that allow execution of the analytic strategy
        # give hands on experience
# one of the most essential ideas of this class is to work with github


## R Shiny
# R Shiny is a web application framework in R Studio
# goal is to turn analysis into interactive web applications
# we only need R knowledge no HTML, CSS or JavaScript
# a Shiny server is required to host a shiny app for the world to see
# otherwise only those who have shiny installed with access to your code will be able to run your app
# we won't cover how to set up a shiny server
# we will run our apps locally and run them locally using shiny apps servers shinyapps.io
# RStudio will do the server work for us - we just have to focus on builing our app
# shinyapps.io is free until you have more than 5 apps running on it 

# what is R shiny?
# R shiny is the tool we will use to develop our data products
# we only need to know R - we can use R for backend and user interface
# shiny = web application framework
# allows you to create graphical interfaces so that users can interact with your analysis
# we can create simple web based interactive data products where the scripting in minimized
# Shiny is made by R Studio - and is free unless you hit the server limits
# servers host the webpage and call the results from your R code
# we could create our own server - AmazonAWS - look into this
# we can share our apps with people who have access to R Studio

# all code is in R but you should have some knowledge with HTML
# HTML gives a web page structure and sectioning as well as markup instructions
# CSS gives the style
# Javascript gives us interactivity
# Bootstrap allows for mobile rendering
# look at free code camp and khan academy


# getting started
# load shiny
install.packages("shiny")
library(shiny)

# a shiny project is a directory containing two files
        # ui.R = for user interface = controls how your app looks
        # server.R =  controls what your apps does


# code examples:
# we need to create a new shiny app similiar to how we would create a markdown document
# develop your "ui" then develop your server instructions
# once you pick a new  R shiny app - you will get both files that you can manipulate
# we can fill in the "working" code into both the ui and the server
library(shiny)

# need to have the shinyUI function - controls user interface
# we will have a main panel with analysis and a sidebar panel with available filters
shinyUI(fluidPage( # describes the page type we use to create the interface
        titlePanel("Data Science is FUCKING RAD! - BITCH!"), # set a GUI element - title
        sidebarLayout( # fluid page has main panel and a side bar panel
                sidebarPanel(
                        h3("FUCK YEAH!!") # h3 gives heading level for sidebar
                ),
                mainPanel(
                        h3("APPS BITCH!!") # gives heading for main panel
                )
        )
))


# SERVER definition
library(shiny)

# key to the server page of our apps is the function Shiny Server
shinyServer(function(input, output) {
        
})

# we can get our app to run by clicking RUN APP at the top of our source editor
# we can also run our app using the runApp() function in the R console
# make sure to set the working directory to the directory that contains our ui and server files!!
# the app will run on a local web app server than RStudio can access
# at some point we will want to push our apps up into a public web page!!
# we can share with Rstudio to Rstudio

# HTML Tags in Shiny
# shiny provides several wrapper functions for using standard HTML tags in ui.R file
        # h1 - h6 = header tags
        # p 
        # a
        # div
        # span
# see ?builder for more details for all the different HTML tags
# we will use the built in functions for now

# need to have the shinyUI function - controls user interface
# we will have a main panel with analysis and a sidebar panel with available filters
shinyUI(fluidPage( # describes the page type we use to create the interface
        titlePanel("Data Science is FUCKING RAD! - BITCH!"), # set a GUI element - title
        sidebarLayout( # fluid page has main panel and a side bar panel
                sidebarPanel(
                        h3("FUCK YEAH!!"), # h3 gives heading level for sidebar
                        h2("trey"),
                        h1("bucket"),
                        em("emphasized text?") # emphasized text HTML tag
                ),
                mainPanel(
                        h3("APPS BITCH!!"), # gives heading for main panel
                        code("some code!! - bitch") # put code in the main panel
                )
        )
))

# look at all the options in builder for all the different HTML tags
# we can open up the browser and use control+u to view the source!
# the source code will be R code converted to HTML CODE!!
# our webpage shows all the libraries with the head and body
# take a look at all the HTML tags - these are the same tags we used in the ui.R file
# we translate our ui.R file into HTML to make the actual webpage!!


# Apps with inputs and outputs
# we started with the look of our app and now we want to add functionality!
# we want R to take in values and produce them in the the web app!
# this is user functionality!!
# shiny provides several types of inputs:
        # buttons
        # checkboxes
        # text boxes
        # calendars
# let's experiment with these inputs

## UI
library(shiny)
# initiate the ui
# we put a slider in the sidebar
# the value of the slider in the main panel
# our expressions need to be consistent between the ui and the server files!!
shinyUI(fluidPage(
        titlePanel("Slider App"),
        sidebarLayout(
                sidebarPanel(
                        h1("Move the Slider!"),
                        sliderInput("slider2", "Slide Me!", 0, 100, 0) #create a slider function
                ),
                mainPanel(
                        h3("Slider Value:"),
                        textOutput("text1") # this needs to match the output from the server function!!
                ) # results will show the slider value in the main panel 
        ) # expressions do not run exactly "linearly" = reactively 
))

## SERVER
library(shiny)
# define server function
# there is an input and output in this function
# server will take input from ui and return an output
# we want to render the data and name it text 1
shinyServer(function(input, output) {
        output$text1 <- renderText(input$slider2 * 10) # add 10 to the input of the slider!
}) # results will take input from user and return a manipulation using R!

## ALL THESE RESULTS ARE HAPPENING REACTIVELY NOT LINEARLY!!!



## Apps with Plots
# we want to create interactive graphics!!
# shiny provides plotOutput function() for ui.R and the renderPlot() function in server.R
# lets build an app with text boxes, sliders that generate different sets of random normal data points
# we have to get used to reactive coding between ui.R and server.R

# UI
library(shiny)
shinyUI(fluidPage(
        titlePanel("Plot Random Numbers"),
        sidebarLayout(
                sidebarPanel(
                        # labels numeric, title, value, max / min, and the steps of the text box increments
                        numericInput("numeric", "How Many Random Numbers Should be Plotted?", 
                                     value = 1000, min = 1, max = 1000, step = 1),
                        # define slider on the x axis
                        sliderInput("sliderX", "Pick Minimum and Maximum X Values",
                                    -100, 100, value = c(-50, 50)),
                        # define the slider on the y axis
                        sliderInput("sliderY", "Pick Minimum and Maximum Y Values",
                                    -100, 100, value = c(-50, 50)),
                        # define set of check boxes - 
                        checkboxInput("show_xlab", "Show/Hide X Axis Label", value = TRUE),
                        checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = TRUE),
                        checkboxInput("show_title", "Show/Hide Title") # this is all above will populate in the sidebar
                ),
                mainPanel(
                        h3("Graph of Random Points"),
                        plotOutput("plot1") # main panel will plot the output of "plot1"
                        # we will need to create something in the server that takes in the input from sidebar and outputs plot1!
                )
        )
))


# SERVER FUNCTION
library(shiny)
# server function with input and output
shinyServer(function(input, output) {
        output$plot1 <- renderPlot({ # output form shiny server matches the main panel title
                set.seed(2016-05-25) # renderPlot is the function used to produce the plot 
                # all these are "reactive expressions" based on user inputs from the UI page defintions!!
                number_of_points <- input$numeric # takes in the input from the UI function and renaming
                minX <- input$sliderX[1] # input from sliderX
                maxX <- input$sliderX[2] # input from slider X
                minY <- input$sliderY[1] # input from slider Y
                maxY <- input$sliderY[2] # input from slider Y
                dataX <- runif(number_of_points, minX, maxX) # generate runiform based on input above and selected in "ui"
                dataY <- runif(number_of_points, minY, maxY)
                xlab <- ifelse(input$show_xlab, "X Axis", "") # xlab if else based on checkbox selection "ui"
                ylab <- ifelse(input$show_ylab, "Y Axis", "") # ylab if else based on checkbox selection "ui"
                main <- ifelse(input$show_title, "Title", "")
                plot(dataX, dataY, xlab = xlab, ylab = ylab, main = main, # plot our data - this will be the output for function plot1!!
                     xlim = c(-100, 100), ylim = c(-100, 100))
        })
}) # curely braces are used in reactive coding


# what is this app doing?
# the first slider gives us the two x values
# the second slider gives us the two y values
# check boxes will be the inputs for the ifelse statements
# as we get new inputs from the UI side panel - shiny server will re-run the results
# the result is new visualizations of the main panel plot based on the inputs from the sidebar!
# we can put in a done button to have the user select when they are done with thier inputs!



## Shiny: Part 2
# reactive expressions: how web app coding is different
# how can we do more complex calculations
# a reactive expression is a recipie that manipulates inputs from Shiny and then returns a vluae
# reactivity provides a way for your app to respond since inputs will change depending on interaction with the UI layer
# expressions wrapped by reactive() should be expressions that are subject to change based on user input
# we want out app to respond to the user input!!

# creating reactive expressions is just like creating a function:
# you will put this in the SERVER LAYER!!!
# remember inputs are in the UI LAYER
# reactive calculations must be in the SERVER LAYER
# reactive statments will have the normal function naming plus the curly braces
# this tells this function to "refresh" as new values are given from the users input!!
calc_sum <- reactive({
        input$box1 + input$box2
})

calc_sum


## REACTIVE EXPRESSIONS CODE DEMO
# first reactive application
# horsepower prediction

## UI LAYER
library(shiny)
# define the UI!!!
# we are trying to demonstrate reactive statements
shinyUI(fluidPage(
        titlePanel("Predict Horsepower from MPG"), #overall title
        sidebarLayout( # side bar panel
                sidebarPanel( # inputs from the sidebar panel
                        sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20), # slider input
                        checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE), # check box inputs
                        checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE)  # will show certain model values
                ),
                mainPanel( # main panel will show the output based on these inputs!! - these RESULTS WILL COME FROM SERVER!!
                        plotOutput("plot1"), # plot 1 output using inputs above
                        h3("Predicted Horsepower from Model 1:"), # title
                        textOutput("pred1"), # output from the prediction 
                        h3("Predicted Horsepower from Model 2:"),
                        textOutput("pred2") 
                )
        ) # be careful to understand the outputs printed on the main panel - each of these will need to be named the same in SERVER
        # we are going to make sure: plot1, pred1, and pred2 each come out of definitions in the SERVER LAYER!!
))


## SERVER LAYER
library(shiny)

# Define server layer
# this is an example of reactive statments in the server layer
shinyServer(function(input, output) {
        mtcars$mpgsp <- ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0) # load mtcars, 
        model1 <- lm(hp ~ mpg, data = mtcars) # fit first model with mpg
        model2 <- lm(hp ~ mpgsp + mpg, data = mtcars) # fit second model with mpg and a "breakpoint"
        
        model1pred <- reactive({ # lets get the prediction for model1 = THIS NEEDS TO BE REACTIVE!!
                mpgInput <- input$sliderMPG # sliderMPG is what we named the slider in the UI layer
                predict(model1, newdata = data.frame(mpg = mpgInput)) # use model 1 and predict at the new input value from slider
        })
        
        model2pred <- reactive({ # prediction logic for model2 = THIS ALSO NEEDS TO BE REACTIVE
                mpgInput <- input$sliderMPG
                predict(model2, newdata = # model2 predict will also contain the same input from the UI layer
                                data.frame(mpg = mpgInput,
                                           mpgsp = ifelse(mpgInput - 20 > 0, # additional spline terms for model2
                                                          mpgInput - 20, 0)))
        })
        output$plot1 <- renderPlot({ # reactive render the plot1
                mpgInput <- input$sliderMPG
                
                plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", # acutal call of the plot to be rendered
                     ylab = "Horsepower", bty = "n", pch = 16,
                     xlim = c(10, 35), ylim = c(50, 350))
                if(input$showModel1){ # if model1 available - show the model1 fit line 
                        abline(model1, col = "red", lwd = 2)
                }
                if(input$showModel2){ # if model2 available from a UI input - show the model2 fit line
                        model2lines <- predict(model2, newdata = data.frame(
                                mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0)
                        ))
                        lines(10:35, model2lines, col = "blue", lwd = 2)
                }
                legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
                       col = c("red", "blue"), bty = "n", cex = 1.2)
                points(mpgInput, model1pred(), col = "red", pch = 16, cex = 2) # add points at mpg input and put in the point from the reative expression (use ())
                points(mpgInput, model2pred(), col = "blue", pch = 16, cex = 2) # add points of true data and the numbers of the model2 reactive expression
        })
        
        output$pred1 <- renderText({ # render the prediction from model1 - REACTIVE
                model1pred()
        })
        
        output$pred2 <- renderText({ # render the prediction from model2 - REACTIVE
                model2pred()
        })
})


# what will this app do?
# it will take the inputs from UI: slider input and checkboxes input
# fit two models on the input, publish text of the predictions, and output plot1 to the main panel
# each statement is wrapped in a REACTIVE LAYER = as new inputs are recieving from UI, SERVER WILL RECALCULATE AND REFRESH THE VALUES!
# remember that the input names from UI need to match AND the output names need to match the output names in the UI MAIN PANEL!!
# THE UI PANEL IS LOOKING FOR NAMES FROM THE SERVER LAYERS THAT MATCH THE RESULTS IT WANTS IN THE MAIN PANEL


## Delayed Reactivity
# you might not want your app to immediately react to changes right away!!
# this will cause shiny to break if we have large complex calculations in the "back end"
# we define a "submit" buttom to all the use to click initialize when the data is available
# we change one line from the previous code to get this!!

library(shiny)
# define the UI!!!
# we are trying to demonstrate reactive statements
shinyUI(fluidPage(
        titlePanel("Predict Horsepower from MPG"), #overall title
        sidebarLayout( # side bar panel
                sidebarPanel( # inputs from the sidebar panel
                        sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20), # slider input
                        checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE), # check box inputs
                        checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE),  # will show certain model values
                        submitButton("Submit") # adding a submit button to the UI layer!
                ),
                mainPanel( # main panel will show the output based on these inputs!! - these RESULTS WILL COME FROM SERVER!!
                        plotOutput("plot1"), # plot 1 output using inputs above
                        h3("Predicted Horsepower from Model 1:"), # title
                        textOutput("pred1"), # output from the prediction 
                        h3("Predicted Horsepower from Model 2:"),
                        textOutput("pred2") 
                )
        ) # be careful to understand the outputs printed on the main panel - each of these will need to be named the same in SERVER
        # we are going to make sure: plot1, pred1, and pred2 each come out of definitions in the SERVER LAYER!!
))




## Advanced UI in Shiny
# there are several types of UI components
# including tabs, navbars, sidebars
# we use tabsetPanel() function to specific a set of tabs
# then the tabPanel function specifies the content of each tab

## Shiny Apps with Tabs

# UI LAYER - notice the tabsetPanel and tabPanel functions nested within the mainPanel arguement
library(shiny)
# goal is a tabbed shiny tab - each with a sidebar and a main panel
shinyUI(fluidPage(
        titlePanel("Tabs!"),
        sidebarLayout(
                sidebarPanel( # sidebar panel will have three text values the user can manipulate
                        textInput("box1", "Enter Tab 1 Text:", value = "Tab 1!"), # each will create an object we need to feed to SERVER
                        textInput("box2", "Enter Tab 2 Text:", value = "Tab 2!"),
                        textInput("box3", "Enter Tab 3 Text:", value = "Tab 3!")
                ),
                mainPanel( # start with main panel and then start the tab panel
                        tabsetPanel(type = "tabs", 
                                    tabPanel("Tab 1", br(), textOutput("out1")), # three tab panel function to create each tab
                                    tabPanel("Tab 2", br(), textOutput("out2")), # tab will be looking for "out1-out3" from the SERVER LAYER
                                    tabPanel("Tab 2", br(), textOutput("out3"))
                        )
                )
        )
))


# SERVER LAYER = notice it "looking" for the inputs from the UI layers box1, box2, box3
# server layer produces the outputs out1, out2, out3 that the main panel of the UI layer is looking for!!
library(shiny)
shinyServer(function(input, output) {
        output$out1 <- renderText(input$box1) # output to the first of the three tabs - need to render text
        output$out2 <- renderText(input$box2) # output to the second of the three tabs
        output$out3 <- renderText(input$box3) # output to the third of the three tabs
})


## Custom HTML Shiny Application
# sometimes we will not want to use the default side and main panels or one of the options "out of the box" in shiny
# we can customize our shiny apps with our own HTML
# what we can do it take a simple shiny page - output the html and use that as your source for customize the HTML files
# reviewing the HTML code of your Shiny apps is a great way to learn how HTML is working
# we can see under the hood of our shiny app in real web development code!!
# you can use a straight HTML file to code in elements you want to see in your shiny app!!


## Interactive Graphics
# we can create actual graphics that a end user can interact with!
# we do this using the brush arguement in the plotOutput() function on the ui.R side
# and then we use the brushedPoints() function on the server.R side
# our example app fits a linear model for the selected points and then draws a line of the best fit

# UI LAYER
library(shiny)
shinyUI(fluidPage( # standard fluid page layout with sidebar and main panel
        titlePanel("Visualize Many Models"),
        sidebarLayout(
                sidebarPanel(
                        h3("Slope"), # will look for this on server side
                        textOutput("slopeOut"), # will look for this on server side
                        h3("Intercept"), # will look for this on the server side
                        textOutput("intOut") # will look for this on server side
                ),
                mainPanel( # we need to recieve plot1 from the server side
                        plotOutput("plot1", brush = brushOpts( # here is our brush call that we also need to see on server side
                                id = "brush1" # this is the label we will use can output from the server side
                        ))
                )
        )
))


# SERVER LAYER
library(shiny)
shinyServer(function(input, output) {
        model <- reactive({ # create reative model to our inputs from the ui side
                brushed_data <- brushedPoints(trees, input$brush1, # shorthand for the brush1 input from the ui side
                                              xvar = "Girth", yvar = "Volume") # this is where we provide the data to brush on
                if(nrow(brushed_data) < 2){
                        return(NULL) # if fewer than two points in the trees data - return null
                }
                lm(Volume ~ Girth, data = brushed_data) # otherwise - fit the model on the "brushed data" which is inputed from UI
        })
        output$slopeOut <- renderText({ # if model is fit we output all of the following with render: slopeout
                if(is.null(model())){
                        "No Model Found"
                } else {
                        model()[[1]][2]
                }
        })
        output$intOut <- renderText({ # intout
                if(is.null(model())){
                        "No Model Found"
                } else {
                        model()[[1]][1]
                }
        })
        output$plot1 <- renderPlot({ # here is plot 1 that we need for the main panel - this is also reactive
                plot(trees$Girth, trees$Volume, xlab = "Girth", # actual plot call
                     ylab = "Volume", main = "Tree Measurements",
                     cex = 1.5, pch = 16, bty = "n")
                if(!is.null(model())){
                        abline(model(), col = "blue", lwd = 2) # if not null add the model line to the main panel
                }
        })
})



## Shiny Gadgets:
# small interactive plots to help your data analysis
# we create a small function to open shiny as a small app to use in data analysis
# this will be displayed in our small screen within RStudio
# Shiny Gagdet is a small single page application in the R Studio viewer page
# we will use the miniUI package to fit within the Viewer tab of RStudio

library(shiny)
library(miniUI)

# extremely simple first shiny gadget - will only be a title page
myFirstGadget <- function() { # our gadget is a function with no arguements
        ui <- miniPage( # just like a shiny app we need a UI and a SERVER layer
                gadgetTitleBar("My First Gadget") # first gadget will be a simple title bar
        )
        server <- function(input, output, session) { # server layer is a function as well
                # The Done button closes the app
                observeEvent(input$done, {
                        stopApp() # reactivity carries over into shiny gadgets
                })
        }
        runGadget(ui, server) # final call to run the UI and the SERVER layers
}


## shiny gadgets are functions that can take values as agruements and return values
# R completes the multiplication for you interactively - 
#you define the set of numbers and clicking done the console will give you the multiplcation


library(shiny)
library(miniUI)

# gadgets are functions with a UI and a SERVER!!
multiplyNumbers <- function(numbers1, numbers2) { # this gadget will have inputs into our function
        ui <- miniPage(
                gadgetTitleBar("Multiply Two Numbers"),
                miniContentPanel( # main pane of our gadget
                        selectInput("num1", "First Number", choices=numbers1), #drag down boxes with num1
                        selectInput("num2", "Second Number", choices=numbers2) # drag down box with num2
                ) # similiar to shiny apps we will need to feed inputs into server
                # numbers will come from our two arguements in our defined function!!!
        )
        server <- function(input, output, session) { # need input output and session
                observeEvent(input$done, { # curly braces for reactive code
                        num1 <- as.numeric(input$num1) 
                        num2 <- as.numeric(input$num2)
                        stopApp(num1 * num2) # take our two numbers and output the multiplcation of the numbers
                })
        }
        runGadget(ui, server) # run the gadget with the above ui and server
}



## Gadgets with Interactive Graphics
# we want to plot the trees dataset and have a crosshair to select rectangle
# the function will output the actual data frame to the UI console

library(shiny);library(miniUI);

picktrees <- function() { # start miniUI function
        ui <- miniPage( # start UI definitions
                gadgetTitleBar("select points by dragging your mouse"), # title
                miniContentPanel( # in the main contentpage give me a plot called "plot"
                        plotOutput("plot", height = "100%", brush = "brush") 
                        # brush gives us the mouse dragging functionality
                        # we will need plot and brush to be rendered in the server call
                )
        )
        
        server <- function(input, output, session) {
                output$plot <- renderPlot({ # here is the output we want to render from ui
                        plot(trees$Girth, trees$Volume, main = "Trees Brah",
                             xlab = "Girth", ylab = "Volume") 
                        # this plot will get passed back from ui interactions
                })
                observeEvent(input$done, { # stop the app after we select brush points
                        stopApp(brushedPoints(trees, input$brush, # brush input from ui
                                              xvar = "Girth", yvar = "Volume"))
                        # console will print data frame of the points we selected
                })
        }
        # run our gadget with the ui and server 
        # we can assign the output to variable to investigate we will get a dataframe with the data points you selected
        runGadget(ui, server)
}




## googleVis
# r package that connects R to google's visualization API
# google has great documentation
# we can create interactive graphics based on what google already has
# maps, plots, motion charts are all available from GOOGLE!!
# https://developers.google.com/chart/interactive/docs/gallery
# basic idea is: R function creates an HTML page - HTML page calls Google Charts - the reuslt is an interactive HTML graphic


data("Fruits")
Fruits
# create gvis motion chart
suppressPackageStartupMessages(library(googleVis))
M <- googleVis::gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=600, height=400))

# lets see under the hood of the google vis outuput - this is the javascript used to build our gvis plot
print(M,"chart")

# start up the google vis plot in a web browser
plot(M)



## other examples of Google vis tools
        # motion charts: gvisMotionChart
        # interactive maps: gvisGeoChart
        # interactive tables: gvisTable
        # line charts: gvisLineChart
        # bar charts: gvisColumnchart
        # tree maps: gvisTreeMap
# https://cran.r-project.org/web/packages/googleVis/googleVis.pdf
# https://developers.google.com/chart/interactive/docs/gallery/geochart

# plots on maps
# there are a million different options that we can pass to our visualizations
# syntax will be in json syntax under the options call in the gvis 
# chart will be built off of the Exports data frame

G <- gvisGeoChart(Exports, locationvar="Country", # set data frame and look for text to determine country
                  colorvar="Profit",options=list(width=600, height=400)) # color will be profit
print(G,"chart")

plot(G)

# specifying a region
G2 <- gvisGeoChart(Exports, locationvar="Country", # set data frame and country 
                   colorvar="Profit",options=list(width=600, height=400,region="150")) 
# set region to 150 to zoom in on our plot
print(G2,"chart")

plot(G2)

# setting options on our g-vis plots!!
# create line chart
# create toy dataset
df <- data.frame(label=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))

# create the line chart - take a look at all the options!!
# pass options exactly how google charts wants to pass them to you
Line <- gvisLineChart(df, xvar="label", yvar=c("val1","val2"),
                      options=list(title="Hello World", legend="bottom",
                                   titleTextStyle="{color:'red', fontSize:18}", # notice the syntax for options                         
                                   vAxis="{gridlines:{color:'red', count:3}}", # this syntax is json syntax
                                   hAxis="{title:'My Label', titleTextStyle:{color:'blue'}}",
                                   series="[{color:'green', targetAxisIndex: 0},
                                   {color: 'blue',targetAxisIndex:1}]",
                                   vAxes="[{title:'Value 1 (%)', format:'##,######%'}, 
                                   {title:'Value 2 (\U00A3)'}]",                          
                                   curveType="function", width=500, height=300                         
                      ))


print(Line,"chart")

plot(Line)

# combining multiple plots together
# we use gvismerge to combine multiple plots together
# google's plotting library does all the hard work in the back end!!
G <- gvisGeoChart(Exports, "Country", "Profit",options=list(width=200, height=100)) # geo chart
T1 <- gvisTable(Exports,options=list(width=200, height=270)) # create a table
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370)) # motion chart
GT <- gvisMerge(G,T1, horizontal=FALSE) # merge command stacks each chart together vertically
GTM <- gvisMerge(GT, M, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10") 
# merge the last two charts horizontally

print(GTM, "chart")

plot(GTM)

# seeing the underlying HTML code is important!!
# we use print M to see the HTML code
# we can embed our visualizations in websites with HTML code
# dynamic visualizations can be built with Shiny, Rook and R.rsp
# we can embed our visualizations in R markdown documents
# set: results ="asis" in the chunk options within R Markdown
# can be used with knitr and slidify
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=600, height=400))
print(M)
print(M, 'chart', file='myfilename.html')

# more google vis demos and additional links
demo(googleVis)
# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis.pdf
# https://developers.google.com/chart/interactive/docs/gallery
# https://developers.google.com/chart/interactive/faq





## Plotly
# what can we do with plotly??
# web application and library for R 
# we can create interactive web graphics in R with plotly 
# works with matlab, python, many different applications including Excel
# how to we create graphs and how do we share them? - use the plotly site or ship out as an HTML graphic
install.packages("plotly")
library(plotly)


# basic scatterplot - includes tooltips for your data
# specify a scatterploot by indicating mode = "markers"
# we can also put these graphics in R MARKDOWN!! wow!
# code examples
# the actual plotly document has a lot of different options to help you explore your own data
# we can publish to plotly
# we can export the webpage that we can use to share with other users
# we can also export the png image and share that as well
data("mtcars")
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, mode = "markers")


# scatterplot options
# coloring cylinder in our plotly charts
data("mtcars")
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, mode = "markers", color = as.factor(mtcars$cyl))

# continous colors
# plotly recognizes the color variable is continoues and plots a color scale for you
data("mtcars")
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, mode = "markers", color = mtcars$disp)

# changing the size of the points
# displacement is the color and horsepower is the size!
data("mtcars")
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, mode = "markers", color = mtcars$disp, size = mtcars$hp)

# 3D scatterplot in plotly!!
set.seed(2016-07-21)
temp <- rnorm(100, mean = 30, sd = 5)
pressure <- rnorm(100)
dtime <- rnorm(100)

# build the 3d scatterplot based on the variables created above
# temperature, pressure and dtime plotted in 3d scatterplot
# 100 datapoints each with 100 values
# specificy each of the 3 variables
# type is scatter3d and mode is markers for scatterplot
# we can output any plotly graph to a web page!! use this to share with other users!!
plot_ly(x = temp, y = pressure, z = dtime,
        type = "scatter3d", mode = "markers", color = temp)


# line graphs in plotly
# these are the best for showing changes over time
# load the data - time series data that we want to plot
# each point corresponds to a year
data("airmiles")

# we feed plotly our time series object
# use mode as lines
plot_ly(x = time(airmiles), y= airmiles, mode = "lines")


# stock market manipulation with plotly
# create a multi-line chart with certain stocks
library(plotly); library(tidyr); library(dplyr)
data("EuStockMarkets")

# prep data for plotting
stocks <- as.data.frame(EuStockMarkets) %>% 
        gather(index, price) %>% # pulls multiple columns into two columns in tidy format (one INDEX and one PRICE column) - goes from wide to long data
        mutate(time = rep(time(EuStockMarkets),4)) # creates our time variable to plot on the x axis

# plot the results in plotly
plot_ly(stocks, x = stocks$time, y = stocks$price, color = stocks$index, mode = "lines")



# histograms in plotly
# type = histogram and we need just one x variable
# histograms are great for showing how counts of data are distributed
plot_ly(x = precip, type = "histogram")


# boxplots in plotly
# boxplots are great for comparing how different datasets are distributed
# use type = "box" to create a boxplot
plot_ly(iris, y = iris$Petal.Length, color = iris$Species, type = "box")


# heatmaps in plotly 
# displaying three dimensional data in two dimensions - using color for the third dimension
# we use type = "heatmap" arguement

# define our matrix
terrain1 <- matrix(rnorm(100*100), nrow = 100, ncol = 100)

# create our heatmap
# a heatmap is ultimately a raster image
# we have instensity values as a legend in our heatmaps
plot_ly(z = terrain1, type = "heatmap")


# 3d surface in plotly
# create moveable 3D surfaces with type = "surface"

# define our matrix
terrain2 <- matrix(sort(rnorm(100*100)), nrow = 100, ncol = 100)

# create our surface plot
plot_ly(z = terrain2, type = "surface")



# maps with plot_ly
# maps are built with chlorpeths

# Create data frame
state_pop <- data.frame(State = state.abb, Pop = as.vector(state.x77[,1]))
# Create hover text
state_pop$hover <- with(state_pop, paste(State, '<br>', "Population:", Pop))
# Make state borders white
borders <- list(color = toRGB("red"))
# Set up some mapping options
map_options <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
)

plot_ly(z = ~state_pop$Pop, text = ~state_pop$hover, locations = ~state_pop$State, 
        type = 'choropleth', locationmode = 'USA-states', 
        color = state_pop$Pop, colors = 'Blues', marker = list(line = borders)) %>%
        layout(title = 'US Population in 1975', geo = map_options)


# Create data frame
state_pop <- data.frame(State = state.abb, Pop = as.vector(state.x77[,1]))
# Create hover text
state_pop$hover <- with(state_pop, paste(State, '<br>', "Population:", Pop))
# Make state borders white
borders <- list(color = toRGB("red"))
# Set up some mapping options
map_options <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
)

plot_ly(z = ~state_pop$Pop, text = ~state_pop$hover, locations = ~state_pop$State, 
        type = 'choropleth', locationmode = 'USA-states', 
        color = state_pop$Pop, colors = 'Blues', marker = list(line = borders)) %>%
        layout(title = 'US Population in 1975', geo = map_options)


# https://plot.ly
# https://plot.ly/r/
# https://images.plot.ly/plotly-documentation/images/r_cheat_sheet.pdf


# we can convert normal ggplots automatically to plotly charts!!

# develop data
set.seed(100)
d <- diamonds[sample(nrow(diamonds),1000),]

# ggplot we want to conver to plotly
p <- ggplot(data = d, aes(x = carat, y = price)) +
        geom_point(aes(text = paste("Clarity:",clarity)), size = 4) +
        geom_smooth(aes(color = cut, fill = cut)) +
        facet_wrap(~cut)

# convert our ggplot to a plotly plot!!
# use the command ggplotly to do this! 
ggplotly(p)


# how do we post to plotly directly?
# we use the plotly_Post(plotly object) - we post our graphic to the plotly website
# we can actually use plotly's web based graphic interface after pushing it to the plotly website
# to do this we will need to set plotly user name and plotly API key




## Quiz Week 1: Data Products
# https://rpubs.com/ryantillis/Data_Products_1


# Question 1: Which of the following are absolutely necessary for creating a functioning shiny app? (Check all that apply)
# Solution: A server .R file containing a call to shinyServer(); a ui.R file containing a call to shinyUI()



# Question 2: What is incorrect about the following syntax in ui.R?

# Solution: missing a comma in the sidebarPanel call
library(shiny)
shinyUI(pageWithSidebar(  
        headerPanel("Data science FTW!"),  
        sidebarPanel(    
                h2('Big text'), # we need to add a comma here to have this code work    
                h3('Sidebar')  
        ),  
        mainPanel(      
                h3('Main Panel text')  
        )
))




# Question 3: consider the following ui.R and the following in the server.R

# Solution: the server.R output name isn't the same as the plotOutput command used in ui.R

# ui
shinyUI(pageWithSidebar(  
        headerPanel("Example plot"),  
        sidebarPanel(    
                sliderInput('mu', 'Guess at the mu',value = 70, min = 60, max = 80, step = 0.05)), 
        mainPanel(    
                plotOutput('newHist')  
        )
))

# server call

library(UsingR)
data(galton)

shinyServer(  
        function(input, output) {    
                output$myHist <- renderPlot({      
                        hist(galton$child, xlab='child height', col='lightblue',main='Histogram')      
                        mu <- input$mu      
                        lines(c(mu, mu), c(0, 200),col="red",lwd=5)      
                        mse <- mean((galton$child - mu)^2)      
                        text(63, 150, paste("mu = ", mu))      
                        text(63, 140, paste("MSE = ", round(mse, 2)))      
                })      }
)



# Question 4: What are the main differences between creating a Shiny Gadget and creating a regular Shiny App? (Check all that apply)

# Solution: Shiny Gadgets are designed to be used by R users in the middle of a data analysis;
# Shiny Gadgets are designed to have small user interfaces that fit on one page


# Question 5: consider the following r script: why isn't it doing what we want?

# Solution: NO AGRUMENTS ARE DEFINED FOR pickXY()
library(shiny)
library(miniUI)

pickXY <- function() { # function call did not define the arguements needed - we need a "mydata" option
        ui <- miniPage(
                gadgetTitleBar("Select Points by Dragging your Mouse"),
                miniContentPanel(
                        plotOutput("plot", height = "100%", brush = "brush")
                )
        )
        
        server <- function(input, output, session) {
                output$plot <- renderPlot({
                        plot(data_frame$X, data_frame$Y, main = "Plot of Y versus X",
                             xlab = "X", ylab = "Y")
                })
                observeEvent(input$done, {
                        stopApp(brushedPoints(data_frame, input$brush,
                                              xvar = "X", yvar = "Y"))
                })
        }
        
        runGadget(ui, server)
}

my_data <- data.frame(X = rnorm(100), Y = rnorm(100))

pickXY(my_data)









## Week 2: Developing data products 
# R markdown - we want reproducible research
# built into R studio
# build HTML documents with codes and plots together into one page
# turn R work into more accessible formats
# incorporate R and R plots into documents
# R markdown is reproducible - the sources gets re-run every time a document is generated
# if data changes in the sources the output will change with it
# we make a r markdown with knitr 
# this is a products class and rmarkdown is our pitch for the product

# R Markdown Examples
# create a file with R > new file > R markdown
# stick to the detailed io slides
# we will use this as our presentation
# we know have a base presentation that we want to fill in with information


# ---
#         title: "you can change this"
# author: "Zach Olivier"
# date: "2/26/2018"
# output: ioslides_presentation
# ---
#         
#         ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = FALSE)
# ```
# 
# ## R Markdown
# 
# This is an R Markdown presentation. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.
# 
# When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.
# 
# ## Slide with Bullets
# 
# - Bullet 1
# - Bullet 2
# - Bullet 3
# 
# ## Slide with R Output
# 
# 
# ## MY NEW SLIDE!!
# wow this shit is rad
# 
# ## slide with a title
# #### a slide with out  a title - subheading
# #### another subtittle
# 
# - bullet list
# 1. ordered list
# 2. item b
# ```{r cars, echo = TRUE}
# summary(cars)
# ```
# 
# ## Slide with Plot
# 
# ```{r pressure}
# plot(pressure)
# ```
# 
# 
# ## text options
# plain text
# *italicized text*
#         **bold text**
#         
#         `for (i %in% 1:10)`
# 
# ## my new slide
# 
# look at all these different options we can choose from
# 
# ``` {r, comment = "$$$$", echo = T, eval = F}
# head(mtcars)
# 
# 
# ```
# 
# 
# 
# ## plots with figures
# lets embed plots
# ```{r, echo = F, fig.align= "center", fig.cap= "check this shit out"}
# plot(mtcars$wt, mtcars$mpg)
# 
# 
# 
# ```
# 
# 
# ## if we knit this 
# -we create a html file that we can bring up into our web browser
# we can use this to push to rpubs and github
# github does alot of the formatting for you
# we can click on the html file to see the "raw" html of our html document
# we can see the actual html uusing branch called gh-pages - all that is different is a url
# it is username .github.io [path to html file]!!!
#         we now can host our slides on github!!







## Leaflet
# interactive maps - this is a java package adapted to R
# we do not need to know any javascript
# leaflet uses dplyr style piping
# http://rstudio.github.io/leaflet/


install.packages("leaflet")
library(leaflet)

library(leaflet)
my_map <- leaflet() %>% # generates the map
        addTiles() # adds the content
my_map

# mapping data from open street map
# 
library(leaflet)
my_map <- leaflet() %>% 
        addTiles()
my_map

# add markers to the data
# we string together a set of map > my tiles > my markers etc.
# we pipe our dataset into the next line 
# we take it through the leaflet functions
# we add layers to the base layer by layer
library(leaflet)
my_map <- my_map %>%
        addMarkers(lat=39.2980803, lng=-76.5898801, 
                   popup="Jeff Leek's Office")
my_map

# clean piping
library(leaflet)
my_map <- my_map %>%
        addMarkers(lat=39.2980803, lng=-76.5898801, 
                   popup="Jeff Leek's Office")
my_map


# adding many different markers
set.seed(2016-04-25)
df <- data.frame(lat = runif(20, min = 39.2, max = 39.3),
                 lng = runif(20, min = -76.6, max = -76.5))
df %>% 
        leaflet() %>%
        addTiles() %>%
        addMarkers()


# custom markers
# we can add an icon as a label in leaflet!!
hopkinsIcon <- makeIcon(
        iconUrl = "http://brand.jhu.edu/content/uploads/2014/06/university.shield.small_.blue_.png",
        iconWidth = 31*215/230, iconHeight = 31,
        iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

hopkinsLatLong <- data.frame(
        lat = c(39.2973166, 39.3288851, 39.2906617),
        lng = c(-76.5929798, -76.6206598, -76.5469683))

hopkinsLatLong %>% 
        leaflet() %>%
        addTiles() %>%
        addMarkers(icon = hopkinsIcon)



# additional custom markers
hopkinsIcon <- makeIcon(
        iconUrl = "http://brand.jhu.edu/content/uploads/2014/06/university.shield.small_.blue_.png",
        iconWidth = 31*215/230, iconHeight = 31,
        iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

hopkinsLatLong <- data.frame(
        lat = c(39.2973166, 39.3288851, 39.2906617, 39.2970681, 39.2824806),
        lng = c(-76.5929798, -76.6206598, -76.5469683, -76.6150537, -76.6016766))

hopkinsLatLong %>% 
        leaflet() %>%
        addTiles() %>%
        addMarkers(icon = hopkinsIcon)



hopkinsSites <- c(
        "<a href='http://www.jhsph.edu/'>East Baltimore Campus</a>",
        "<a href='https://apply.jhu.edu/visit/homewood/'>Homewood Campus</a>",
        "<a href='http://www.hopkinsmedicine.org/johns_hopkins_bayview/'>Bayview Medical Center</a>",
        "<a href='http://www.peabody.jhu.edu/'>Peabody Institute</a>",
        "<a href='http://carey.jhu.edu/'>Carey Business School</a>"
)

hopkinsLatLong %>% 
        leaflet() %>%
        addTiles() %>%
        addMarkers(icon = hopkinsIcon, popup = hopkinsSites)



hopkinsSites <- c(
        "<a href='http://www.jhsph.edu/'>East Baltimore Campus</a>",
        "<a href='https://apply.jhu.edu/visit/homewood/'>Homewood Campus</a>",
        "<a href='http://www.hopkinsmedicine.org/johns_hopkins_bayview/'>Bayview Medical Center</a>",
        "<a href='http://www.peabody.jhu.edu/'>Peabody Institute</a>",
        "<a href='http://carey.jhu.edu/'>Carey Business School</a>"
)

hopkinsLatLong %>% 
        leaflet() %>%
        addTiles() %>%
        addMarkers(icon = hopkinsIcon, popup = hopkinsSites)



## adding clusters to our maps
df <- data.frame(lat = runif(500, min = 39.25, max = 39.35),
                 lng = runif(500, min = -76.65, max = -76.55))
df %>% 
        leaflet() %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions())


# clusters
df <- data.frame(lat = runif(500, min = 39.25, max = 39.35),
                 lng = runif(500, min = -76.65, max = -76.55))
df %>% 
        leaflet() %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions())


# mapping circle markers
df <- data.frame(lat = runif(20, min = 39.25, max = 39.35),
                 lng = runif(20, min = -76.65, max = -76.55))
df %>% 
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers()


df <- data.frame(lat = runif(20, min = 39.25, max = 39.35),
                 lng = runif(20, min = -76.65, max = -76.55))
df %>% 
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers()



# drawing circles
md_cities <- data.frame(name = c("Baltimore", "Frederick", "Rockville", "Gaithersburg", 
                                 "Bowie", "Hagerstown", "Annapolis", "College Park", "Salisbury", "Laurel"),
                        pop = c(619493, 66169, 62334, 61045, 55232,
                                39890, 38880, 30587, 30484, 25346),
                        lat = c(39.2920592, 39.4143921, 39.0840, 39.1434, 39.0068, 39.6418, 38.9784, 38.9897, 38.3607, 39.0993),
                        lng = c(-76.6077852, -77.4204875, -77.1528, -77.2014, -76.7791, -77.7200, -76.4922, -76.9378, -75.5994, -76.8483))
md_cities %>%
        leaflet() %>%
        addTiles() %>%
        addCircles(weight = 1, radius = sqrt(md_cities$pop) * 30)




md_cities <- data.frame(name = c("Baltimore", "Frederick", "Rockville", "Gaithersburg", 
                                 "Bowie", "Hagerstown", "Annapolis", "College Park", "Salisbury", "Laurel"),
                        pop = c(619493, 66169, 62334, 61045, 55232,
                                39890, 38880, 30587, 30484, 25346),
                        lat = c(39.2920592, 39.4143921, 39.0840, 39.1434, 39.0068, 39.6418, 38.9784, 38.9897, 38.3607, 39.0993),
                        lng = c(-76.6077852, -77.4204875, -77.1528, -77.2014, -76.7791, -77.7200, -76.4922, -76.9378, -75.5994, -76.8483))
md_cities %>%
        leaflet() %>%
        addTiles() %>%
        addCircles(weight = 1, radius = sqrt(md_cities$pop) * 30)

# drawing rectangles

leaflet() %>%
        addTiles() %>%
        addRectangles(lat1 = 37.3858, lng1 = -122.0595, 
                      lat2 = 37.3890, lng2 = -122.0625)


leaflet() %>%
        addTiles() %>%
        addRectangles(lat1 = 37.3858, lng1 = -122.0595, 
                      lat2 = 37.3890, lng2 = -122.0625)




# adding legend
df <- data.frame(lat = runif(20, min = 39.25, max = 39.35),
                 lng = runif(20, min = -76.65, max = -76.55),
                 col = sample(c("red", "blue", "green"), 20, replace = TRUE),
                 stringsAsFactors = FALSE)

df %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(color = df$col) %>%
        addLegend(labels = LETTERS[1:3], colors = c("blue", "red", "green"))




## Quiz Week 2: Rmarkdown and Leaflet
# https://rpubs.com/ryantillis/Data_Products_2

## Question 1:
# What is rmarkdown? (Check all that apply.)
# 
# A simplified XML format that can be interpreted into R.
# 
# A format that can be interpreted into markdown (which is a simplified markup language).
# 
# A form of LaTeX typesetting.
# 
# A simplified format that, when interpreted, incorporates your R analysis into your document.

## solution: 
# a format that can be interpreted into markdown (markdown is a simplified markup language)
# a simplified format that, when interpreted, incorporates your R analysis into your document

## Question 2:
# In rmarkdown presentations, 
# in the options for code chunks, 
# hat command prevents the code from being repeated before results in the final interpreted document?
#         
#         eval = FALSE
# 
#         cache = FALSE
# 
#         echo = FALSE
# 
#         comment = FALSE

# solution: echo = FALSE


## Question 3:
# In rmarkdown presentations, in the options for code chunks, what prevents the code from being interpreted?
#         
#         eval = FALSE
# 
#         run = FALSE
# 
#         cache = FALSE
# 
#         eval = NULL

## SOLUTION:  eval = FALSE


## Question 4:
# What is leaflet? (Check all that apply.)
# 
# An R package for creating 3D rendered isomaps
# 
# A tool for reproducible documents
# 
# An R package interface to the javascript library of the same name
# 
# A javascript library for creating interactive mapsv

## Solution: 
# an R package interface to the javascript library of the same name
# a javascript library fror creating interactive maps


## Question 5:
# run the R command:
df %>% leaflet() %>% addTiles()
# what is this equivalent to?
df(leaflet(addTiles()))
addTiles(leaflet(df))
addTiles(leaflet(df()))
leaflet(addTiles(df))
leaflet(df) %>% addTiles()

## Solution: 2,5

## Question 6:

# If I want to add popup icons to my leaflet map in R, I should use.
# 
# dplyr
# 
# addTiles
# 
# leaflet
# 
# addMarkers

## Solution: addMarkers



## R Packages
# an R package is a form of data product
# in creating an R package you've created polished, documented software for other users
# this is a big step over just distributing functions or code that do a task

# an R package is a mechanism for extending the basic functionality of R
# a collection of R function or other data objects
# organized in a systematic fashion to provide a minimal amount of consistenecy
# written by users / developers everywhere!!

# where are the packages?
# CRAN and Bioconductor
# also available from Github, Bitbucket etc.
# install.packages using install.packages()
# packages from Github can be installed using install_github() from the devtools package
# not required to put packages on central repository but it allows for greater sharing

# what is the purpose of an R package?
# documentation
# centralized resources
# minimal standards for reliability and robustness
# maintainability / extension
# API = APPLICATION PROGRAMMING INTERFACE = clear instructions of how to use the package
# users know that it will at least load properly

# Package DEvelopment Process
# write some code in a R script
# want to make the code available to others
# incorporate R script file into R package structure
# write documentation for other functions
# include some other material (examples, demos, datasets, tutorials)
# package it up!!

# submit package to CRAN or bioconductor
# push the source code to Githun or other code sharing website
# people find all kinds of problems
        # scenario 1 - tell you the problems and expect you to fix it
        # scenario 2 - other users will fix the problems for you
# you incorporate these changes into your new version of the same package

# R Package essentials
# create a directory with the name of the R package
# a descrtiption file which has info about the package
# R code 
# documentation is the main / sub-directory
# NAMESPACE file (optional)
# full requirements in Writing R Extensions

# Description File
# package name: name of package
# title: full name of package
# description: longer description
# version: version number
# authors: name of original authors
# maintainer: name + email of person who fixes the problems
# license: license type for the source code
# depends: list of other packages that your package depends on
# suggests: optional R packages that users might want to have installed
# date: release date
# url: package home page = github repository
# other: other fields can be added

# R Code
# copy R code into the R/ sub directory
# there can be many files in this directory
# usually seperate into logical groups
# code for all functions should be included here and not anywhere else in the package

# NAMESPACE file
# use to indicate which functions are exported
# exported functions can be called by the user and are considered the public API
# non-exported functions cannot be called directly by the user
# hides implementation details from users
# you can also indicate what functions you import from other packages
# this allows for your package to use other packages without making other packages visible to the user
# importing a function loads the package but does ont attach it to the search list

# key directives
        # export [function]
        # import [packages]
        # importFrom [package] [function]
        # exportClasses [class]
        # exportMethods [generic]

# namespace example:
# user can export mtvsplot
# import form Axis and import splines
export("mvtsplot")
importFrom(graphics, "Axis")
import(splines)

# Documentation
# write up documentation .Rd placed in main / sub-directory
# written is a specific markup language
# requried for every EXPORTED function!!
# limit your functions

# help file example
# user will have to complete this section to host package on CRAN!!
?line
# function (x, y = NULL) 
# {
#         xy <- xy.coords(x, y, setLab = FALSE)
#         ok <- complete.cases(xy$x, xy$y)
#         Call <- sys.call()
#         structure(.Call(C_tukeyline, as.double(xy$x[ok]), as.double(xy$y[ok]), 
#                         Call), class = "tukeyline")
# }
# <bytecode: 0x109506a08>
#         <environment: namespace:stats>


# building and checking
# R CMB build is a command line program that creates a package archive file (.tar . gz)
# R CMD check runs a number of tests on the package
# you can run R CMD build or R CMD check from the command-line using a terminal or shell
# you can also run them from within R using the system() function
system("R CMD build newpackage")
sytem("R CMD check newpackage")

# Checking
# R CMD check runs alot of testing
# documentation exists
# code can be loaded
# no major code problems
# runs examples in documentations
# check docs match code
# all tests must pass to put on CRAN

# Getting Started
# the package.skeleton() function in the utlis package creates a "skeleton" R package
# creates directory structure, description file, namespace file, documentation files
# if there are functions in your workspace it writes R code files to the R / directory
# documentation stubs are in the man page

# Summary
# R packages provide a systematic way to make R code available to others
# standard ensure that packages have a minimal amount of documentatoin and robustness
# obtained from CRAN, BIOCONDUCTOR, GITHUB

# Summary
# create a new directory with R and man / sub directories
# write a description file
# copy in R code
# write documentation files
# write a namespace file - contains imports and exports!!
# build and check your files!!


## building an R package demo




## Data Products Quiz Week 3
# https://rpubs.com/ryantillis/Data_Products_3

## Question 1:
# Which of the following items is required for an R package to pass R CMD check without any warnings or errors?

# example data sets
 
# An explicit software license
 
# unit tests
 
# a demo directory
 
# vignette

# Solution: an explicit software license



## Question 2:
# Which of the following is a generic function in a fresh installation of R, 
# with only the default packages loaded? (Select all that apply)

# mean

# lm

# colSums

# predict

# dgamma

# show

# Solution: show, mean, predict


## Question 3:
# What function is used to obtain the function body for an S4 method function?
#         
# getS3method()
# 
# getClass()
# 
# showMethods()
# 
# getMethod()


# Solution: getMethod()


## Question 4: 
# Please download the R package DDPQuiz3 from the course web site. 
# Examine the 𝚌𝚛𝚎𝚊𝚝𝚎𝚖𝚎𝚊𝚗 function implemented in the R/ sub-  
#         directory. 
# What is the appropriate text to place above the 𝚌𝚛𝚎𝚊𝚝𝚎𝚖𝚎𝚊𝚗 function for Roxygen2 to create a complete help file?


#' This function calculates the mean
#'
#' @param x is a numeric vector
#' @return the mean of x
#' @export
#' @examples
#' x <- 1:10
#' createmean(x)





## Classes and Methods in R
# represent new types of data that R does not support
# what if new data types emerge that we need to add into R?
# we need a new set of functions and data types to work with 
# the R system can adapt and support these new structures

# a system for doing object oriented programming
# R combines interaction with object orientation
# tradiitonal language C++ are not very interactive
# John Chambers is the creator of S and is documented in Programming with Data
# R allows people from being a user to a programmer

# start out as users and then slowly becomes PROGRAMMERS!!
# as our needs grow we can develop new things in R
# object oriented programming is a little different in R

# two styles of classes of methods:
# S3 classes / methods
        # included with version 3 of the S language
        # informal
        # old style classes / methods
# S4 classes / methods
        # more formal and rigorous
        # include with S-Plus 6 and R 1.4.0 
        # new style classes and methods
# use S4 if you are developing new classes in methods
# both with be used side by side for a long time
# both systems are seperate and independent of each other
# S3 is a little bit easier to get started with
# new developers are encouraged to develop in the S4 system

# the code for implementing S4 classes / methods in R is in the methods package
# a class is a description of a thing
# we can define a class as SetClass()
# an object is an instance of a class
# a method is a function that operates on a set of classes
# generic functions dispatch methods based on what your class is
# generic functions will call the correct method to do the job
# a method is the implementation of an function on an object of a specific class
# ?Classes, ?Methods

# all objects in R have a class which can be determined by the class function
# these are the atomic classes
class(1)
# [1] "numeric"
class(NA)
# [1] "logical"
class("foo")
# [1] "character"

# we have more classes
# the output from a regression fit is it's own class
# the output contains a lot of different information that determines it own class
# this will also have a set of unique functions that can be used on this unique class
x <- rnorm(100)
y <- x + rnorm(100)
fit <- lm(y ~ x)
class(fit)
# [1] "lm"

# S4 and S3 style generic functions look different but they play the same role
# when you program we can write new methods for an existing generic
# or create your own generic and associated methods
# if a data type does not exist in R that matches your needs you can always define a new class along with generics/methods that go with it

# generic functions
# find an appriopriate method based on what data is passed to them
mean
# function (x, ...) 
#         UseMethod("mean")
# <bytecode: 0x109c1f548>
#         <environment: namespace:base>

print
# function (x, ...) 
#         UseMethod("print")
# <bytecode: 0x1099ce1c0>
#         <environment: namespace:base>

# we have a list of methods than mean will go find depending on what data class is passed to it
methods("mean")
# [1] mean.Date     mean.default 
# [3] mean.difftime mean.POSIXct 
# [5] mean.POSIXlt 
# see '?methods' for accessing help and source code

# the show function is from the methods package and is the S4 equivalent of print
show
# standardGeneric for "show" defined from package "methods"
# 
# function (object) 
#         standardGeneric("show")
# <bytecode: 0x108369430>
#         <environment: 0x1022ba110>
#         Methods may be defined for arguments: object
# Use  showMethods("show")  for currently available ones.
# (This generic function excludes non-simple inheritance; see ?setIs)


# S4 Methods
# all these objects below have a special show function
showMethods("show")
# Function: show (package methods)
# object="ANY"
# object="C++Class"
# object="C++Function"
# object="C++Object"
# object="classGeneratorFunction"
# object="classRepresentation"
# object="envRefClass"
# object="externalRefMethod"
# object="function"
# (inherited from: object="ANY")
# object="genericFunction"
# object="genericFunctionWithTrace"
# object="MethodDefinition"
# object="MethodDefinitionWithTrace"
# object="MethodSelectionReport"
# object="MethodWithNext"
# object="MethodWithNextWithTrace"
# object="Module"
# object="namedList"
# object="ObjectsWithPackage"
# object="oldClass"
# object="refClassRepresentation"
# object="refMethodDef"
# object="refObjectGenerator"
# object="signature"
# object="sourceEnvironment"
# object="standardGeneric"
# (inherited from: object="genericFunction")
# object="traceable"


# first arguement is an object of particular class
# generic check the class of the object
# a search is done if there is an approprioate method for the class
# if there is a method for the class - the method is called and the loop ends
# if a method does not exist it will default to a default method for that method
# if a default method doesn't exist then we will get an error

# Examining Code for MEthods
# you cannot print the code for a method like other functions
# we need to use getS3method
# the call is getS3method(<generic>, <class>)
# the S4 methods you will use getMethod()
# the call is the same as the getS3method call

# S3 Class / Method Examples:
set.seed(2)
x <- rnorm(100)
mean(x)
# [1] -0.03069816

# the class of x is numeric
# but there is no mean method for "numeric" objects!!!
# so we then look for the default method and use it!!

# look at the generic mean method
head(getS3method("mean", "default"),10)
# 1  function (x, trim = 0, na.rm = FALSE, ...)                         
#         2  {                                                                  
#                 3      if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {      
#                         4          warning("argument is not numeric or logical: returning NA")
#                         5          return(NA_real_)                                           
#                         6      }                                                              
#                 7      if (na.rm)                                                     
#                         8          x <- x[!is.na(x)]                                          
#                 9      if (!is.numeric(trim) || length(trim) != 1L)                   
#                         10         stop("'trim' must be numeric of length one") 


# data frames can be many columns of different types!!
# the mean function will sort through the data frame by column calling the correct method for each column!!!
# mean will dispatch to the correct mean method based on the class of the data!!
set.seed(3)
df <- data.frame(x = norm(100), y = 1:100)
sapply(df, mean)
# a         b         c 
# 0.5038286 0.6183249 0.5243205 
# d 
# 0.4925005

# calling methods directly
# some S3 methods are visible to the user
# NEVER CALL METHODS DIRECTLY
# CALL THE GENERIC FUNCTION AND LET IT FIND THE CORRECT METHOD TO DISPATCH
# S4 we cannot call the methods directly at all!!!!

# plot function method
# here is a plot of a ts class!!!
# there is a special plotting method for ts objects!!
# the generic function will dispatch to the correct method
set.seed(10)
x <- rnorm(100)
x <- as.ts(x)
plot(x)

# write methods!
# print / show
# summary
# plot
# any new data classes will need functions to call methods on this new class


# S4 Classes
# why do we want to create a new class?
# to represent a new type of data
# new concepts that haven't been thought of yet
# to abstract / hide implementation details from the users
# "not known to R" is a new data type

# Creating a new class
# setClass()
# we need the name
# we need to specify data elements that are called slots
# we then defin methods for the class with the setMethod function 

# polygon class
# creating new classes / methods usaually not something done at console
# the "slots" for this new data class are x and y
# x and y must be numeric!!
library(methods)
setClass("polygon",representation(x = "numeric", y = "numeric"))

# a plot method can be created with the setMethod function
# we call setMEthod and specifiy the generic and the signature
# in our case generic is plot and the signature is polygon
# notice that the slots of the polygon (x and y coordinates) are accessed with the @ operator
# we did not have to create a new generic function because plot already exists
setMethod("plot", "polygon",
          function(x,y,...){
                  plot(x@x, x@y, type = "n",...)
                  xp <- c(x@x, x@x[1])
                  yp <- c(x@y, x@y[1])
                  lines(xp, yp)
          })


# the polygon method has now been added as a method to the plot function!!
# the signature for class polygon is now listed
# the method "ANY" is the default method
showMethods("plot")
# Function: plot (package graphics)
# x="ANY"
# x="polygon"


# let's use our new classes and methods
p <- new("polygon", x = c(1,2,3,4), y = c(1,2,3,4))
plot(p)


# summary
# developing classes and methods is a powerful way to extend the functionality of R
# classes define new data types
# methods extend generic functions to specify behavior of generic function and classes
# this allows new users to work with new data 



## Swirl
# swirl is an R package that turns R console into an interactive learning enviroment
# library(swirl)
# swirl()
# the R console will start talking to you!!
# you can interactively go through lessons in swirl!
# we can build are own with swirlify

## How to use swirlify
install.packages("swirlify")
library(swirlify)

# start a new lesson
# all swirl lessons are written in yaml
# yaml is a markdown language
# questions are developed by key value pairs
# all lessons and documents will be placed in our working directory
new_lesson("lesson 1", "my first course")

# use the wq() to add questions to your course
wq_message()

# question prompt
wq_command()

# add lesson to the manifiest
add_to_manifest()

# you can test your lesson for formatting
test_lesson()

# test lesson inside of swirl
demo_lesson()


# more complicated question types
get_current_lesson() # what course are we working on right now?

# start a new lesson
new_lesson("Lesson 2", "my first course")

# multiple choice question
wq_multiple()

# add to manifest to add our course
add_to_manifest()


# write a figure type question
# show a graph and ask a question about the figure
wq_figure()




































































 





















































































