# https://github.com/bcaffo/courses/tree/master/07_RegressionModels


## Introduction to Regression
# regression is the most fundamental topic for data science
# generalization of linear models
# correlation is tied to linear regression
# Francis Galton is the inventor of linear regression

# read the blog simply statistics = new reading material!
# "linear regression suggests a 1% increase in shots results in a 1.6 drop in win % (with standard error)


# what types of questions could we answer?
# use parents height to predict children heights
# we need parsimonious = "easy to explain"
# regression is the best at generating parsimonious data
# investigate the variation
# quantify what other features deeply effect children height
# connect this back to inference - what assumptions are needed to extrapolate this to the general population?
# regression to the mean 

## Basic Least Squares
# francis galton data invented regression and correlation
# loading the Using R for elementrary statistics = new book!
install.packages("UsingR")
library(UsingR)

data(galton); library(reshape2); long <- melt(galton)

# lets show the histogram of parent vs. child height
# this is the marginal distribution of the parents and child heights
ggplot(long, aes(x = value, fill = variable)) +
        geom_histogram(color = "black", binwidth = 1) +
        facet_grid(. ~ variable)


# consider the children's heights:
# how can we define the middle? the physical center of mass on a histogram
# the mean minimizes the square error of the child's heights
install.packages("manipulate")
library(manipulate)

# lets test this proof in real life
# as we get to the center of the historgram the MSE goes down!!!!
# to minimize MSE you need to minimize the sum of the squared distrances between observed and mu
# wow this is fucking amazing!!
# the least squares estimate is the empirical mean!!!
hist <- function (mu) {
        mse <- mean((galton$child - mu)^2)

        g <- ggplot(galton, aes(x = child)) +
                geom_histogram(fill = "salmon", color = "black", binwidth = 1) +
                geom_vline(xintercept = mu, size = 3) +
                ggtitle(paste("mu =",mu, "MSE = ", round(mse, 2), sep = " "))
        g
}

manipulate(hist(mu), mu = slider(62, 74, step = 0.5))


## Technical Details
# proof that Ybar is the minimizer of the mean squared error function
# our MSE function has to be greater than each yi - ybar
# therefore = our minimizing function has to be the ybar


# comparing children's heights and thier parents' heights
# scatter plot will show the relationship between parent and child
ggplot(galton, aes(x = parent, y = child)) +
        geom_jitter() +
        geom_smooth()

# regression through the origin
# we want to explain the relationship between parents and child with a line
# in order to find the best line - we need to find the slope Beta that minimizes the sum of the squared error
# this is similiar to the least squares mean!!!
# the line will minimize the sum of the squared vertical distances (errors)

# let's experiment with manipulate
# this is a toy example: regressiom through origin is not real regression
# regression fits the slope and the intercept of the line
# the best line will minimize the vertical distance between the observations and the line!!!!
# take all vertical distances between fitted lines and observations, square them and add them - find the best slope
# you can subtract the mean to re-center the origin for a new line

# manipulate example
# this is fucking sweet motherfucker!!!!
# you will fit the line with the slope that minimizes the distance from the line to the observations!!!
# note the slope of 1 is not good = you need to multiple the parents height by a factor of parents height
y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
        g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
        g <- g  + scale_size(range = c(2, 20), guide = "none" )
        g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean( (y - beta * x) ^2 )
        g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))


# the solution: lm fits the linear model exactly as above!!!
# this give us the best slope that minimizes the mean squared error from the observations to the line
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

# Call:
#         lm(formula = I(child - mean(child)) ~ I(parent - mean(parent)) - 
#                    1, data = galton)
# 
# Coefficients:
#         I(parent - mean(parent))  
# 0.6463 

# this code chunk fits the best line from lm onto the data!!!!
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
lm1 <- lm(galton$child ~ galton$parent)
g <- g + geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], size = 3, colour = grey(.5))
g


# swirl practice
install.packages("swirl")
library(swirl)


## Ordinary Least Squares
# this is a work horse of statistics
# it gives us a way of taking complicated outcomes and explaining behavior using linearity
# the simplest application of OLS is fitting a line through data

## Regression: Basic notation and background
# these are fundamental to regression
# we write X1, X2,...,Xn to describe n data points
# x1 = 1, x2 = 2, x3 = 5, and n = 3 (count of total observations)
# greek letters are estimates - american letters are observed!!!!

# empirical mean
# Xbar is sample mean of collection of xs
# sum of xi from 1 to n
# if we subtract the mean from each observation we get Xi~ = centering the random variables = mean will be 0
# the sample mean is the best least squares solution!!!

# emprical standard deviation
# variance is S^2 = the average squared deviation of the observations against the mean
# standard deviation is sqrt(variance)
# the data defined and each observation divided by the standrad deviation, we scale the data = sd = 1
# if we scale and center the data we get a dataset with mean = 0, std = 1 = NORMALIZING THE DATA!!!

## empircal covariance (correlation)
# most essential informatoni to regression
# we have pairs of data:
# the correlation is the covariance NORMALIZED = unit free quantity!!
# correlation needs to be between -1 and +1
# correlation measures the stregth of the linear relationship between variables X and Y
# stronger the relationship = closer to the extremes!!! -1 or 1
# correlation of 0 implies no relationship!!

## Least Squares estimation of regression lines
# explain child's height based on parents' height
# find the best line: CH = B0 + PH(B1)
# CH = intercept + slope of parents' height correlation
# how do we define best? = LEAST SQUARES
# we want to fit the line where the slope minimizes the squared distances between each obs and the fitted line
# the way the computer fits the line is by minimizing the total mean squared error!!! you fucking motherfucker!!
# each point contributes equally to the line fit!!
# B1 (slope) = the correlation between (y,x) * sd(y) / sd(x)
# B0 (intercept) = ybar - B1Xbar
# if you normalize the data = the slope is the correlation between Y and X

## Linear Least Squares Coding Example

## General least squares for linear equations
# Consider again the parent and child height data from Galton
library(UsingR)
data(galton)
library(dplyr); library(ggplot2)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")  
g

# show the best fitted line in R code
# this is an inside look at the built in lm function in R
# will be compare the "by hand" linear model to the lm function below!!
# we get the exact same numbers!!!
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

# (Intercept)         x
# [1,]    23.94153 0.6462906
# [2,]    23.94153 0.6462906

# lets flip the predictors = flip x and y
# results hold true
beta1 <- cor(y, x) *  sd(x) / sd(y)
beta0 <- mean(x) - beta1 * mean(y)
rbind(c(beta0, beta1), coef(lm(x ~ y)))

# (Intercept)         y
# [1,]    46.13535 0.3256475
# [2,]    46.13535 0.3256475

# Revisiting Galton's data
# Regression through the origin yields an equivalent slope if you center the data first
# test hand made formula vs. lm formula
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])
 
# x 
# 0.6462906 0.6462906 

# how to get regression through the origin = no intercept in lm call
lm(yc ~ xc -1)

# Call:
#         lm(formula = yc ~ xc - 1)
# 
# Coefficients:
#         xc  
# 0.6463 

lm(yc ~ xc + 0)

# Call:
#         lm(formula = yc ~ xc + 0)
# 
# Coefficients:
#         xc  
# 0.6463


# Normalizing variables results in the slope being the correlation
# if you normalize - the slope will be the correlation!!!!
yn <- (y - mean(y))/sd(y)
xn <- (x - mean(x))/sd(x)
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])

# xn 
# 0.4587624 0.4587624 0.4587624 


# how to add a regression line through a plot
# use the geom_smooth function and specifyy the method and the formula!!!
# the confidence interval is already included!!
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")  
g <- g + geom_smooth(method="lm", formula=y~x)
g


## Mathmatical Details: proofs of correlation formula


## swirl pratice
library(swirl)

#Here are the vectors of variations or tweaks
sltweak <- c(.01, .02, .03, -.01, -.02, -.03) #one for the slope
ictweak <- c(.1, .2, .3, -.1, -.2, -.3)  #one for the intercept
lhs <- numeric()
rhs <- numeric()
#left side of eqn is the sum of squares of residuals of the tweaked regression line
for (n in 1:6) lhs[n] <- sqe(ols.slope+sltweak[n],ols.ic+ictweak[n])
#right side of eqn is the sum of squares of original residuals + sum of squares of two tweaks
for (n in 1:6) rhs[n] <- sqe(ols.slope,ols.ic) + sum(est(sltweak[n],ictweak[n])^2)


# [1]  1.264198e-09  2.527486e-09  3.801688e-09 -1.261469e-09
# [5] -2.522938e-09 -3.767127e-09




## Regression to the Mean
# Question: why children of tall parents still tall but shorter than parents?
# these types of questions can be asked with anything related to measured error
# these are examples of regression to the mean
# invented by Galton = regression towards mediocrity in hereditary structure
# this idea serves as a foundation for linear regression

# 100% regression to the mean
# simulate pairs of standard normal distributions
# if you take the largest x, the corresponding y has a very high chance of being smaller
# in most cases there is some blend of the intrinsic component and some "noise"
# how much of sports is regression to the mean? can we try to quantify the regression to the mean?

# let's plot this idea with some R code
## Plot of the results

library(UsingR)
data(father.son)
# normalize the x and y variables = mean 0 and variance 1
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)

# rho is greek letter to represent correlation
rho <- cor(x, y)
# [1] 0.5013383

library(ggplot2)

g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
# indentity line
g = g + geom_abline(intercept = 0, slope = 1)

# axis for x and y
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)

# treat son's height as predictor and parents height as predictor
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)

# if no noise - predictions would fall exactly on the identity line
# however there is lots of noise - this is estimated by the regression line!
# the regression line will be how shrunken the line is to the x axis
# if all noise we would land exactly on the x axis = no predictive power!
# you can invert this and do the same for son = predictor, parent = outcome


g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")

g



## swirl practice Least Squares Estimates
# the method of choosing the "best" regression line is ordinary least squares
# this means we fit the line that minimizes the squared "error" from regression line to each obs
# the slope of a regressio line is the corrleation between the two variables
# you can normalize data by subtracting its mean and dividiing its standard deviation
# correlation of un-normalized data is correlation(x,y) * sd(x) / sd(y)




myPlot <- function(beta){
        y <- galton$child - mean(galton$child)
        x <- galton$parent - mean(galton$parent)
        freqData <- as.data.frame(table(x, y))
        names(freqData) <- c("child", "parent", "freq")
        plot(
                as.numeric(as.vector(freqData$parent)), 
                as.numeric(as.vector(freqData$child)),
                pch = 21, col = "black", bg = "lightblue",
                cex = .15 * freqData$freq, 
                xlab = "parent", 
                ylab = "child"
        )
        abline(0, beta, lwd = 3)
        points(0, 0, cex = 2, pch = 19)
        mse <- mean( (y - beta * x)^2 )
        title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = manipulate::slider(0.4, .8, step = 0.02))




## Quiz Week 1: Linear Regression
# https://rpubs.com/cheyu/reg-q1

# Question 1:
# consider the data listed below:
# what is the value of u that minimizes the least squares equation?

x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

# solution: the minimizer of the function is :
        # sum of w(i) * x(i) divided by sum of w(i)
sum((w*x)) / sum(w)

# [1] 0.1471429

# Question 2:
# consider the following dataset
# fit the regression through the origin and get the slope treating y as the outcome

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

# solution: use lm to fit a line through the data
# how to get regression through the origin = no intercept in lm call
lm(y ~ x -1)

# Call:
#         lm(formula = y ~ x - 1)
# 
# Coefficients:
#         x  
# 0.8263  

# Question 3:
# open data from mtcarts and fit a regression
# regression = model with mpg as outcome and weight as predictor
# what is the slope coeffiecnt??

data(mtcars)

# solution: simple fit of the lm function
lm(mpg ~ wt, data = mtcars)

# Call:
#         lm(formula = mpg ~ wt, data = mtcars)
# 
# Coefficients:
#         (Intercept)           wt  
#              37.285       -5.344  



# Question 4:
# consider data with an outcome (y) and a predictor (x)
# the standard deviation is one half of the outcome
# the correlation between the two variables is .5
# what value would the slope coefficient for the regression model with y as outcome be?

# solution:
# correlation formula:
        # B1 = corr(x,y) * sd(y) / sd(x)
        # corr = .5
        # standard deviation of x = sd(x) = .5 * sd(y)
        # rewrite formula = (.5) * sd(y) / (.5)sd*y
        # .5 cancels out of correlation term and SD(X) term!!!
        # final result = sd(y) / sd(y) = 1



# Question 5:
# students were given tests and scores were normalized to mean 0 and variance 1
# the correlation between the scores on the two tests was 0.4
# what would be the expected score on Quiz 2 for student who scored 1.5 on Quiz 1?

# solution:
        # correlation equation = corr(q1, q2) * sd(q1) / sd(q2) = .4
        # quiz 1 score = 1.5
        # quiz 2 score will be q1 * the correlation
        # (1.5) * .4 = .6
cor <- .4
q1 <- 1.5

q2 <- cor *q1
# [1] 0.6



# Question 6: 
# consider the data below
# what is the value of the first measurement if x were normalized?
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)

# solution: subtract the mean and divide by the sd
m <- mean(x)
sd <- sd(x)

obs1 = x[1]

solution <- (obs1 - m) / sd
solution
# [1] -0.9718658


# Question 7
# consider the data below
# what is the intercept for fitting the model with x as predictor?
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

# solution: call lm
lm1 <- lm(y ~ x)
lm1

# Call:
#         lm(formula = y ~ x)
# 
# Coefficients:
#         (Intercept)            x  
#               1.567       -1.713  




# Question 8
# both the predictor and response have a mean 0
# what can be said about the intercept when you fit a regression line?

# solution: data is normalized
# to normalize data fit the lm line subtracting 1 from the call
# will plot a line without an intercept
# intercept will be 0



# Question 9
# consider the data below
# what value minimizes the sum of the squared distances between these points and itself?
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)

# solution: the mean will minimize the squared error
solution <- mean(x)
solution
# [1] 0.573





# Question 10
# the slope is b1 of y ~ x
# the slope y1 of x ~ y
# divide b1 by y1 = b1 / y1
# what is this ratio always equal to?

# solution: 
        # b1 = corr(y,x) * sd(y) / sd(x)
        # y1 = corr(x,y) * sd(x) / sd(y)
        # therefore: b1 / y1 = sd(y) / sd(x) * sd(y)  / sd(x) = sd(y)^2 / sd(x)^2
        # sd()^ = VARIANCE!!!
        # b1 / y1 = var(y) / var(x)





## Linear Regression Week 2


## Statistical Linear Regression Models
# basic regression with least squares is an estimation tool
# we would like to do inference!
# how can we take our model an make it "generalize" to a larger population

# a statistical model will have independent error
# error might be the error you get from not having certain models
# or error might just be the noise of the natural dataset

# Interpret the Intercept
# expected value and variance are population quantities - the estimates that we would like to know
# intercept = expected value Y given that the regressor is 0
# regression model = Y = intercept + B1x1 + error
# if we shift the regression variable it does not change the slope of the LINE

# Interpret the Coeffiecnts (SLOPES)
# interpretating the regression coeffiencts = THE SLOPE
# becuase of a slope and change in x will have a corresponding change in y
# b1 is the expected change in reponse per change in the regression variable (x variable)




## Linear Regression for Prediction
# we can get a prediction for Y by plugging in a new "X"
# we will push through a value of x through the slope and the intercept and error
# this will give us a new prediction of Y based on a "new" value of x!!

# let's do some code for regression models
library(UsingR); data(diamond)
library(ggplot2)

# plot carat vs. price and plot a regression model through the relationship
# method = "lm" plots the regression line assumes y is outcome and x is predictor
# the lm line will minimizes the sum of squared vertical distances between points and the line
(g = ggplot(diamond, aes(x = carat, y = price)) +
        xlab("Mass (carats)") + 
        ylab("Price (SIN $)") +
        geom_point(size = 6, color = "black", alpha = .2) +
        geom_point(size = 5, color = "blue", alpha = .2) +
        geom_smooth(method = "lm", color = "black"))

# let's fit the actual regression model
fit <- lm(price ~ carat, data = diamond)

# grab coefficients as a vector
# coefficient interpretation:
# coefficient: we estimate an expected $3721.02 increase in price for every carat increase in a diamond
# intercept: value of a 0 carat diamond
coef(fit)
# (Intercept)       carat 
# -259.6259   3721.0249 

# detailed regression summary
summary(fit)
# Call:
#         lm(formula = price ~ carat, data = diamond)
# 
# Residuals:
#         Min      1Q  Median      3Q 
# -85.159 -21.448  -0.869  18.972 
# Max 
# 79.370 
# 
# Coefficients:
#         Estimate Std. Error
# (Intercept)  -259.63      17.32
# carat        3721.02      81.79
# t value Pr(>|t|)    
# (Intercept)  -14.99   <2e-16 ***
#         carat         45.50   <2e-16 ***
#         ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
# ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 31.84 on 46 degrees of freedom
# Multiple R-squared:  0.9783,	Adjusted R-squared:  0.9778 
# F-statistic:  2070 on 1 and 46 DF,  p-value: < 2.2e-16



# let's scale the model to build more interpretation
# we scale the regression by the mean of carat = MEAN CENTER THE PREDICTOR VARIABLE!
# use the I function to do math statements within the lm call
# this will change the intercept interpretation to be the price for an average carat diamond
# re-scaling the variable x will not change the slope of B1 - it will only re-scale the intercept
fit2 <- lm(price ~I(carat - mean(carat)), data = diamond)

# $500 dollars is the expected price of an average carat diamond!!
coef(fit2)
# (Intercept) 
# 500.0833 
# I(carat - mean(carat)) 
# 3721.0249 

summary(fit2)
# Call:
#         lm(formula = price ~ I(carat - mean(carat)), data = diamond)
# 
# Residuals:
#         Min      1Q  Median      3Q 
# -85.159 -21.448  -0.869  18.972 
# Max 
# 79.370 
# 
# Coefficients:
#         Estimate
# (Intercept)             500.083
# I(carat - mean(carat)) 3721.025
# Std. Error
# (Intercept)                 4.596
# I(carat - mean(carat))     81.786
# t value
# (Intercept)              108.8
# I(carat - mean(carat))    45.5
# Pr(>|t|)    
# (Intercept)              <2e-16 ***
#         I(carat - mean(carat))   <2e-16 ***
#         ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
# ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 31.84 on 46 degrees of freedom
# Multiple R-squared:  0.9783,	Adjusted R-squared:  0.9778 
# F-statistic:  2070 on 1 and 46 DF,  p-value: < 2.2e-16


# a 1 carat jump is very large - we can do more rescaling
# lets change the units to 1/10th of a carat
# this can me more interpretable
fit3 <- lm(price ~ I(carat *10), data = diamond)

# price increases $372 dollars for every 1/10th increase in carat size!!!!
coef(fit3)
# (Intercept) I(carat * 10) 
# -259.6259      372.1025 

summary(fit3)
# Call:
#         lm(formula = price ~ I(carat * 10), data = diamond)
# 
# Residuals:
#         Min      1Q  Median      3Q 
# -85.159 -21.448  -0.869  18.972 
# Max 
# 79.370 
# 
# Coefficients:
#         Estimate Std. Error
# (Intercept)   -259.626     17.319
# I(carat * 10)  372.102      8.179
# t value Pr(>|t|)    
# (Intercept)    -14.99   <2e-16 ***
#         I(carat * 10)   45.50   <2e-16 ***
#         ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
# ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 31.84 on 46 degrees of freedom
# Multiple R-squared:  0.9783,	Adjusted R-squared:  0.9778 
# F-statistic:  2070 on 1 and 46 DF,  p-value: < 2.2e-16



## let's predict on new diamonds!! THIS IS WHY WE DATA!!! YOU MOTHERFUCKER LEARN THIS!
# what is the price of these new diamonds???

# carat sizes of new diamonds we want to predict price on
new.d <- c(.16,.28,.34)

# plugging in diamonds to the model 
# coef(fit)[1] is the value of the intercept!!!
# coef(fit)[2] is the value of the coefficient slope of carat!!!
# predicted prices are below!!!
coef(fit)[1] + coef(fit)[2] * new.d
# [1]  335.7381  782.2611 1005.5225


# general method for predicting = predict function!!
# same answer as above - you need to feed it newdata to show the prediction
# giving no "newdata" will predict back onto the observed data
predict(fit, newdata = data.frame(carat = new.d))
# 1         2         3 
# 335.7381  782.2611 1005.5225 

# linear regression works by taking in the new variable and "finding" it on the line
# this allows us to trace the line to the y axis to give our predicted outcome
# THIS SHIT IS THE FUNDAMENTAL FUNDAMENTAL IF YOU DON'T GET THIS YOU ARE A FUCKING WORTHLESS STATISTICAN



## Residuals and Residual Variation
# motivating example: diamond dataset
# how can we example variation in prices based on carat size?
library(UsingR)
data(diamond)
library(ggplot2)

# fits regression line through the data
# we want to explain price by mass
# there is variation - not all points fit exactly to the plotted line
# the variation around the regression line is the RESIUDAL VARIATION
# this is the variation that is not explained by the regressor variable MASS
# we have explained some variation but not ALL OF IT!!!
# the distance of the residuals to the line is the residuals!!
(g = ggplot(diamond, aes(x = carat, y = price)) +
                xlab("Mass") +
                ylab("Price")+
                geom_point(size = 7, color = "black", alpha = .5) +
                geom_point(size = 5, color = "blue", alpha = .2) +
                geom_smooth(method = "lm", color = "black"))


# model: Price = intercept + ceof(MASS1) + error_term(0, sigma^2)
# Y is a price, x is a price
# residual is the distance between observed value and the fitted line
# the least squares tries to minimize the distance between the squared error distances
# properties of residuals
        # expected value = 0
        # if the intercept is included the sum of the residuals is also 0
        # if a regressor variable is included in the model the sum of error times the regressor is also 0
        # RESIDUALS ARE USEFUL FOR INVESTIGATING POOR MODEL FIT
        # positive residuals are above the regression line, negative are below
        # systematic variation = variation explained by the regression model
        # residual variation = variation "left over" after fitting the model
        # residual plots highlight poor model fit

## Residual Coding Examples
data(diamond)

# hand calculate the residuals
y <- diamond$price; x <- diamond$carat; n <- length(y)

# fit a model on price to carat
fit <- lm(y~x)
# Call:
#         lm(formula = y ~ x)
# 
# Coefficients:
#         (Intercept)            x  
# -259.6       3721.0 

# get the residuals of fit
e <- resid(fit)
# 1           2           3 
# -17.9483176  -7.7380691 -22.9483176 
# 4           5           6 
# -85.1585661 -28.6303057   6.2619309 
# 7           8           9 
# 23.4721795  37.6311854 -38.7893116 
# 10          11          12 
# 24.4721795  51.8414339  40.7389488 
# 13          14          15 
# 0.2619309  13.4209369  -1.2098087 
# 16          17          18 
# 40.5287002  36.1029250 -44.8405542 
# 19          20          21 
# 79.3696943 -25.0508027  57.8414339 
# 22          23          24 
# 9.2619309 -20.9483176  -3.7380691 
# 25          26          27 
# -19.9483176  27.8414339 -54.9483176 
# 28          29          30 
# 8.8414339 -26.9483176  16.4721795 
# 31          32          33 
# -22.9483176 -13.1020453 -12.1020453 
# 34          35          36 
# -0.5278205   3.2619309   2.2619309 
# 37          38          39 
# -1.2098087 -43.2098087 -27.9483176 
# 40          41          42 
# -23.3122938 -15.6303057  43.2672091 
# 43          44          45 
# 32.8414339   7.3696943   4.3696943 
# 46          47          48 
# -11.5278205 -14.8405542  17.4721795

# another way to get residuals
# with no specifications, predict will fit the model back onto the observed data
yhat <- predict(fit)
# 1         2         3 
# 372.9483  335.7381  372.9483 
# 4         5         6 
# 410.1586  670.6303  335.7381 
# 7         8         9 
# 298.5278  447.3688  521.7893 
# 10        11        12 
# 298.5278  410.1586  782.2611 
# 13        14        15 
# 335.7381  484.5791  596.2098 
# 16        17        18 
# 819.4713  186.8971  707.8406 
# 19        20        21 
# 670.6303  745.0508  410.1586 
# 22        23        24 
# 335.7381  372.9483  335.7381 
# 25        26        27 
# 372.9483  410.1586  372.9483 
# 28        29        30 
# 410.1586  372.9483  298.5278 
# 31        32        33 
# 372.9483  931.1020  931.1020 
# 34        35        36 
# 298.5278  335.7381  335.7381 
# 37        38        39 
# 596.2098  596.2098  372.9483 
# 40        41        42 
# 968.3123  670.6303 1042.7328 
# 43        44        45 
# 410.1586  670.6303  670.6303 
# 46        47        48 
# 298.5278  707.8406  298.5278 


# check these two methods against each other
# both methods are the same!
max(abs(e - (y - yhat)))
# [1] 9.485746e-13
max(abs(e- (y - coef(fit)[1] - coef(fit)[2] * x)))
# [1] 9.485746e-13


# the sum of residuals is zero
sum(e)
# [1] -1.865175e-14

# sum of the residuals times the regressor variable is 0
sum(e *x)
# [1] 6.959711e-15



## plotting residuals for motivation
# residuals will be the "red lines" between the observed points and the fitted line
# plot the values
# plots the regression line
# plot the distances between values and the regression line

# values plot
plot(diamond$carat, diamond$price,
     xlab = "Mass",
     ylab = "Price",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = F)
# regression line added to the plot
abline(fit, lwd = 2)
# residual distances added to the plot
for (i in  1:n)
        lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red", lwd = 2)


# how do we asses redisual variation?
# plot residuals vs. our regressor variable!!
# residuals vs. x plot!!!

# plot the mass variable and the residuals as points
# residuals on y axis, regressor variable on the x
# when you look at residual plot you are looking for any pattern = residuals should be patternless!!
# residuals need to be randomly distributed above and below 0
plot(x, e,
     xlab = "MASS",
     ylab = "Residuals",
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21, frame = F)
# plot a horizontal line at 0 to determine positive or negtive residuals
abline(h = 0, lwd = 2)
# loop through residuals and calculate red line distance for each
for (i in  1:n)
        lines(c(x[i], x[i]), c(e[i], 0), col = "red", lwd = 2)


## other residual plots

# non-linear residual plot data
# uniform x value
# use the x value plus some sin noise and normal noise
x = runif(100, -3, 3); y <- x + sin(x) + rnorm(100, sd = .2);

# plot the results of the regression line
(ggplot(data.frame(x = x, y= y), aes(x = x, y = y)) +
         geom_smooth(method = "lm", color = "black")+
         geom_point(size = 7, color = "black", alpha = .4) +
         geom_point(size = 5, color = "red", alpha = .4))

# now let's plot the residuals
# there is a pattern in the residuals...hmm...
# the sine term is now extremely apparent = the model inadequacy if perfectly shown out in the open
(ggplot(data.frame(x = x, y = resid(lm(y~x))),aes(x = x, y = y)) +
                geom_hline(yintercept = 0, size = 2) +
                geom_point(size = 7, color = "black", alpha = .4) +
                geom_point(size = 5, color = "red", alpha = .4) +
                xlab("X") + ylab("Residual"))


# example of a "perfect" fit that is not so perfect when looking at the residuals plot
x <- runif(100, 0 , 6); y <- x + rnorm(100, mean = 0, sd = .001 * x)

# plot the fitted line through this data
# data looks like an exact fit!!
(ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
                geom_smooth(method = "lm", color = "black") +
                geom_point(size = 7, color = "black", alpha = .4) +
                geom_point(size = 5, color = "blue", alpha = .4))
# let's plot the residuals to investigate
# what we notice is the variability gradually increases as x gets larger!!
# this is a pattern and residuals should have no pattern! ~ randomly distributed
# this is called heteroskedasticity!!
(ggplot(data.frame(x = x, y = resid(lm(y~x))), aes(x = x, y = y)) +
                geom_hline(yintercept = 0, size = 2) +
                geom_point(size = 7, color = "black", alpha = .4) +
                geom_point(size = 5, color = "green", alpha = .4) +
                xlab("x") + ylab("Residual"))




# residual plot on the diamond dataset
# add a column to the original diamond dataset with the lm fit residuals "column e"
# model is price modeled by carat
diamond$e <- resid(lm(price ~ carat, data = diamond))

# plot the residual plots
# no pattern in the residuals - indicates a reasonable good fit
(ggplot(diamond, aes(x = carat, y = e)) +
                xlab("Carats") +
                ylab("Residual Price") +
                geom_hline(yintercept = 0, size = 2) +
                geom_point(size = 7, color = "black", alpha = .5) +
                geom_point(size = 5, color = "blue", alpha = .2))


# systemaic residuals vs. regression residuals
# systematic = "natural" variance or devation 
# regression residuals = variance around the newly fitted regression line!!

# residual vector around the intercept = "natural" residuals
# residuals with a regression line = residuals around the fitted regression line
# variation around average price
# variation around the price ~ carat regression line
e = c(resid(lm(price ~ 1, data = diamond)), resid(lm(price ~ carat, data = diamond)))

# create factor of natural vs. regression residuals
fit <- factor(c(rep("ITC", nrow(diamond)),
                rep("ITC, slope", nrow(diamond))))

# plot the residuals based on the type of "fit" - natural or over the regression
# we have explained a lot of the variation in the "natural" variance with the model
# there is still variation after accounting for the residual model
# subtracting the "left over" regression variation vs. the "natural" variation gives us the amount of variation "explained" by the model 
(ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit)) +
                geom_dotplot(binaxis = "y", size = 2, stackdir = "center") +
                xlab("fitting approach") +
                ylab("residual price"))



## Residual Variation
#  residual variation is the variation around the the regression line
# residuals are the distances between the outcomes and the fitted regression line
# the sum of residuals are 0 if an intercept is included
# the average squared residual is an estimate of the actual regression


# diamond example - how to grab the residual variation
y <- diamond$price; x <- diamond$carat; n <- length(y)

# fit the model
fit <- lm(y ~ x)

# print the residual variation to the console
summary(fit)$sigma
# [1] 31.84052

# hand-written formula:
sqrt(sum(resid(fit)^2) / (n-2))
# [1] 31.84052


# total variation = total variation around the mean 
# regression will explain away some of the total variation = regression variability
# the "left over" is the residuals around the regressio line = RESIDUAL variability = variability not explained by the MODEL!!!
## RESID VARIATION + REGRESS VARIATION = TOTAL VARIATION!!!!!
## TOTAL VARIATION = EXPLAINED VARIATION + LEFTOVER VARIATION!!!!
## R^2 = % of total variance that is represented by the MODEL!!!!
## R^2 = REGRESSION VARIATION / TOTAL VARIATION!!!! = SHARE OF VARIATION EXPLAINED BY MODEL!!!
## % of the variantion in price that is explained by mass

## R^2 facts
# has to be between 0 and 1
# is the sample correlation squared
# this can be misleading summary model of fit
        # deleting data can inflate R^2
        # adding terms to a regression model will increase R^2 = OVERFITTING


## anscombe example
# all these plots have the same R^2 value = BUT THE FIT IS DIFFERENT FOR EACH ONE!
# be careful for interpretting R^2
# when you look at the scatterplots the fit has very different meaning!!!
# our summary of the variation has thrown out a lot of the information!!
# plot 1 = nice fit 
# plot 2 = curved data has missing term?
# plot 3 = large outlier
# plot 4 = "stacked" data with one "outside" point
data("anscombe"); example("anscombe")

anscmb> require(stats); require(graphics)

# anscmb> summary(anscombe)
# x1             x2             x3      
# Min.   : 4.0   Min.   : 4.0   Min.   : 4.0  
# 1st Qu.: 6.5   1st Qu.: 6.5   1st Qu.: 6.5  
# Median : 9.0   Median : 9.0   Median : 9.0  
# Mean   : 9.0   Mean   : 9.0   Mean   : 9.0  
# 3rd Qu.:11.5   3rd Qu.:11.5   3rd Qu.:11.5  
# Max.   :14.0   Max.   :14.0   Max.   :14.0  
# x4           y1               y2       
# Min.   : 8   Min.   : 4.260   Min.   :3.100  
# 1st Qu.: 8   1st Qu.: 6.315   1st Qu.:6.695  
# Median : 8   Median : 7.580   Median :8.140  
# Mean   : 9   Mean   : 7.501   Mean   :7.501  
# 3rd Qu.: 8   3rd Qu.: 8.570   3rd Qu.:8.950  
# Max.   :19   Max.   :10.840   Max.   :9.260  
# y3              y4        
# Min.   : 5.39   Min.   : 5.250  
# 1st Qu.: 6.25   1st Qu.: 6.170  
# Median : 7.11   Median : 7.040  
# Mean   : 7.50   Mean   : 7.501  
# 3rd Qu.: 7.98   3rd Qu.: 8.190  
# Max.   :12.74   Max.   :12.500  
# 
# anscmb> ##-- now some "magic" to do the 4 regressions in a loop:
#         anscmb> ff <- y ~ x
# 
# anscmb> mods <- setNames(as.list(1:4), paste0("lm", 1:4))
# 
# anscmb> for(i in 1:4) {
#         anscmb+   ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
#         anscmb+   ## or   ff[[2]] <- as.name(paste0("y", i))
#                 anscmb+   ##      ff[[3]] <- as.name(paste0("x", i))
#                 anscmb+   mods[[i]] <- lmi <- lm(ff, data = anscombe)
#         anscmb+   print(anova(lmi))
#         anscmb+ }
# Analysis of Variance Table
# 
# Response: y1
# Df Sum Sq Mean Sq F value  Pr(>F)   
# x1         1 27.510 27.5100   17.99 0.00217 **
#         Residuals  9 13.763  1.5292                   
# ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Analysis of Variance Table
# 
# Response: y2
# Df Sum Sq Mean Sq F value   Pr(>F)   
# x2         1 27.500 27.5000  17.966 0.002179 **
#         Residuals  9 13.776  1.5307                    
# ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Analysis of Variance Table
# 
# Response: y3
# Df Sum Sq Mean Sq F value   Pr(>F)   
# x3         1 27.470 27.4700  17.972 0.002176 **
#         Residuals  9 13.756  1.5285                    
# ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Analysis of Variance Table
# 
# Response: y4
# Df Sum Sq Mean Sq F value   Pr(>F)   
# x4         1 27.490 27.4900  18.003 0.002165 **
#         Residuals  9 13.742  1.5269                    
# ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# anscmb> ## See how close they are (numerically!)
#         anscmb> sapply(mods, coef)
# lm1      lm2       lm3       lm4
# (Intercept) 3.0000909 3.000909 3.0024545 3.0017273
# x1          0.5000909 0.500000 0.4997273 0.4999091
# 
# anscmb> lapply(mods, function(fm) coef(summary(fm)))
# $lm1
# Estimate Std. Error  t value    Pr(>|t|)
# (Intercept) 3.0000909  1.1247468 2.667348 0.025734051
# x1          0.5000909  0.1179055 4.241455 0.002169629
# 
# $lm2
# Estimate Std. Error  t value    Pr(>|t|)
# (Intercept) 3.000909  1.1253024 2.666758 0.025758941
# x2          0.500000  0.1179637 4.238590 0.002178816
# 
# $lm3
# Estimate Std. Error  t value    Pr(>|t|)
# (Intercept) 3.0024545  1.1244812 2.670080 0.025619109
# x3          0.4997273  0.1178777 4.239372 0.002176305
# 
# $lm4
# Estimate Std. Error  t value    Pr(>|t|)
# (Intercept) 3.0017273  1.1239211 2.670763 0.025590425
# x4          0.4999091  0.1178189 4.243028 0.002164602
# 
# 
# anscmb> ## Now, do what you should have done in the first place: PLOTS
#         anscmb> op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
# 
# anscmb> for(i in 1:4) {
#         anscmb+   ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
#         anscmb+   plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
#                        anscmb+        xlim = c(3, 19), ylim = c(3, 13))
#         anscmb+   abline(mods[[i]], col = "blue")
#         anscmb+ }



## residual swirl practice! you bitch
sqrt(sum(fit$residuals^2) / (n - 2))

summary(fit)$sigma









## Inference in Regression
# let's recall our model = Y = intercept + slope(x) + iid error term
# intercept = Ybar - slope*Xbar
# beta1 = the SLOPE = correlation(Y, X) * SD(Y) / SD(X)

# Statistical Inference Review
# theta (hat) - theta / standard error have the following properties
        # normally distributed
        # finite samples with estimated variance = T distribution
        # we can use this to test hypothesis tests = test = to the null or not equal to the null
        # we can make confidence intervals: theta hat +/- Q1 - 2*error 
# inference in regression will follow the same ideas
# errors may not have to follow a normal distribution - law of high sample size
# Variance of slope = variance around the line / variance around the X value
# we need more variance in X to get more information gain on how to fit the best line!!!
# the variance of the coefficient is very informative!!!


## coding example:
library(UsingR); data(diamond)


# let's do these calculations manually to build inference motivation
# define your variables
y <- diamond$price; x <- diamond$carat; n <- length(y)

# coefficient slope definition
beta1 <- cor(y,x) * sd(y) / sd(x)

# intercept definition
beta0 <- mean(y) - beta1 * mean(x)

# error term definition = residuals!! 
e <-  y - beta0 - beta1 * x

# sigma calculation = estimate of the standard deviation around the regression line!!!
sigma <- sqrt(sum(e^2))

# sums of my squares of x
ssx <- sum((x - mean(x))^2)

# standard error of Beta0 = standard error of the intercept
seBeta0 <- (1 / n + mean(x) ^2 / ssx)

# standard error of Beta1 = standard error of the coefficient value = the slope
seBeta1 <- sigma / sqrt(ssx)

# t stat for beta0
tbeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1

# calculate your pvalues
pBeta0 <- 2 * pt(abs(tbeta0), df = n - 2, lower.tail = F)
pBeta1 <- 2 * pt(abs(tBeta1), df = n -2, lower.tail = F)

# build the coefficient table
coefTable <-  rbind(c(beta0, seBeta0, tbeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
# build row and column names for your coefficient table
colnames(coefTable) <- c("Estimate", "STD Error", "t value", "P value")
rownames(coefTable) <- c("Intercept", "x")

# print the results
coefTable

# Estimate   STD Error     t value
# Intercept -259.6259   0.2958544 -877.546181
# x         3721.0249 554.6988286    6.708189
# P value
# Intercept 8.329671e-99
# x         2.499167e-08

# R way to quickly build all of above
# this output is exactly the same as the results above!!!
fit <- lm(y ~ x)
summary(fit)$coefficients

# Estimate Std. Error   t value
# (Intercept) -259.6259   17.31886 -14.99094
# x           3721.0249   81.78588  45.49715
# Pr(>|t|)
# (Intercept) 2.523271e-19
# x           6.751260e-40


# let's work through getting the confidence intervals
sumCoef <-  summary(fit)$coefficients

# getting the actual confidence interval of the residuals = INTERCEPT
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df.residual) * sumCoef[1,2]
# [1] -294.4870 -224.7649

# getting the actual confidence interval of the residuals = SLOPE
sumCoef[2,1] + c(-1,1) * qt(.975, df = fit$df.residual * sumCoef[2,2])
# [1] 3719.064 3722.985

# scale interval to get smaller jump in diamond size
# for a .1 carat increase in size - we have 95% confidence that a the price will be between this interval!!!
(sumCoef[2,1] + c(-1,1) * qt(.975, df = fit$df.residual * sumCoef[2,2])) /10
# [1] 371.9064 372.2985





## Prediction with Linear Regression
# consider predicting Y at the value of X
        # predicting the price of a diamond given the size
        # predicting the height of a child given the height of the parents
# estimation point = intercept + slope*(value of X)
# a standard error is needed to create a prediction interval
# you can get inference in regression easier i.e. confidence intervals / prediction intervals
# we need to evaluate uncertainty in our linear models
# our prediction error is going to be the smallest when predicting at the average X
# the more variable our Xs are the lower out prediction error is!!

# coding example:
library(ggplot2)
x = diamond$carat; y = diamond$price

# define new x variables
newx = data.frame(x = seq(min(x), max(x), length = 100))

# get the prediction and confidence interval from the predict function
p1 = data.frame(predict(fit, newdata = newx, interval = "confidence"))
p2 = data.frame(predict(fit, newdata = newx, interval = "prediction"))

# create row names in a new column for confidence and prediction
p1$interval = "confidence"
p2$interval = "prediction"

# re-write the new x values in a column
p1$x = newx$x
p2$x = newx$x

# combine the CI and predictions into one data frame and name it y
dat = rbind(p1, p2)
names(dat)[1] = "y"

# plot the results
# blue is the prediction interval
# pink is the confidence interval 
# confidence interval is "narrower" than the prediction interval!!!
# more data = more narrow CI = predictions closer to the line
# the prediction interval represents the variability in the Y
# there remains residual variation no matter how good your model is
# inherent error is captured in the prediction interval!!
# both will get narrower towards the center of the data set = the "mean" of the x's
(ggplot(dat, aes(x = x, y = y)) +
                geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = .2) +
                geom_line() +
                geom_point(data = data.frame(x = x, y = y), aes(x = x, y = y), size = 4))










## QUIZ 2: 
# https://rpubs.com/cheyu/reg-q2


## Question 1:
# consider the following data with x as the predictor and y as the outcome:
# give a p-value for the two sided hypothesis test of wheter B1 from a linear regression model:
# is 0 or not 0
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

## Solution:
# fit a linear model, and obtain the p-value:
fit <- lm(y ~ x)
summary(fit)
# Call:
#         lm(formula = y ~ x)
# 
# Residuals:
#         Min       1Q   Median       3Q 
# -0.27636 -0.18807  0.01364  0.16595 
# Max 
# 0.27143 
# 
# Coefficients:
#         Estimate Std. Error
# (Intercept)   0.1885     0.2061
# x             0.7224     0.3107
# t value Pr(>|t|)  
# (Intercept)   0.914    0.391  
# x             2.325    0.053 .
# ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
# ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.223 on 7 degrees of freedom
# Multiple R-squared:  0.4358,	Adjusted R-squared:  0.3552 
# F-statistic: 5.408 on 1 and 7 DF,  p-value: 0.05296

# p-value noted in the summary but we can also extract it
# coef of summary fit gives table of key metrics by intercept and variables
coef.table <- coef(summary(fit))
# navigate the table to the pvalue we want: row 2, column 4
coef.table[2,4]
# [1] 0.05296439





## Question 2:
# consider the data from the previous problem:
# give the estimate of the residual standard deviation
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

## Solution: 
# fit the linear model then extract the residual standard error from the summary
# https://stackoverflow.com/questions/11099272/r-standard-error-output-from-lm-object
# the output from the summary function is just and R list: you can apply R operations to a list to find RSE
fit <- lm(y ~ x)
(m <- summary(fit))
# Call:
#         lm(formula = y ~ x)
# 
# Residuals:
#         Min       1Q   Median       3Q      Max 
# -0.27636 -0.18807  0.01364  0.16595  0.27143 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.1885     0.2061   0.914    0.391  
# x             0.7224     0.3107   2.325    0.053 .
# ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.223 on 7 degrees of freedom
# Multiple R-squared:  0.4358,	Adjusted R-squared:  0.3552 
# F-statistic: 5.408 on 1 and 7 DF,  p-value: 0.05296

# see where residual standard error falls in the list == "sigma"
names(m)
# what number place is it in the list? == 6
# output should match the Residual Standard Error listed in the summary of the lm model
(sigma <- m[[6]])
# [1] 0.2229981







## Question 3:
# in the mtcars dataset:
# fit a regressio model of weight on mpg
# obtain the 95% confidence interval at the average weight
# what is the lower endpoint of this interval?

##Solution:
# load the data; fit the line; use predict to find the confidence interval
data("mtcars")

# define variables
y <- mtcars$mpg; x <- mtcars$wt

# fit the model
fit <- lm(y ~ x)

# predict back onto the mean of the weight, specifiy the interval option to confidence interval
pred <- predict(fit, newdata = data.frame(x = mean(x)), interval = "confidence")
pred
# fit      lwr      upr
# 1 20.09062 18.99098 21.19027

# practice extracting R lists
(lower <- pred[[2]])
# [1] 18.99098







## Question 4:
# refer to the previous question:
# what is the weight coefficient interpretted as?

# Solution: 
# fit the model and interpret the slope
data("mtcars")

# define variables
y <- mtcars$mpg; x <- mtcars$wt

# fit the model
fit <- lm(y ~ x)

# take a look at summary and coefficients
summary(fit)
coeff <- fit$coefficients

# practice extracting from R lists:
(slope.wt <- coeff[[2]])
# [1] -5.344472

## interpretation of weight coefficient: -5.344472
# for each 1,000 lb increase in weight we expect a decrease in mpg at a factor of the slope
# therefore: the slope interpretation is the estimated expected change in mpg per 1,000 lb increase in wt





## Question 5:
# refer to the mtcars data:
# a new car weighs 3,000 pounds...
# what is the 95% prediction interval for is mpg?
# what is the upper endpoint?

## Solution: load the data, fit the line, and predict on a x == 3,0000 with interval = "prediction"
data("mtcars")

# define variables
y <- mtcars$mpg; x <- mtcars$wt

# fit the model
fit <- lm(y ~ x)

# predictions = predict using fit model, new data is a 3000 lb car, with prediction as interval
# remember to scale your interval to 3,000 / 1,000 as x moves in 1,000 increments!!!!
# extract the upper confidence interval from the new predict list
(pred <- predict(fit, newdata = data.frame(x = 3), interval = "prediction"))
pred[[3]]
# [1] 27.57355




## Question 6:
# consider again the mtcars dataset:
# construct a 95% confidence interval of mpg for a 1 short ton increase increase in weight
# give the lower CI endpoint
# a short ton is defined as 2,000 lbs

## Solution: 
# load the data
# fit the line 
# predict on a "short ton" with interval = "prediction"
        # will need to re-scale the x value to be the increase in one "short ton"
data("mtcars")

# define variables
y <- mtcars$mpg; x <- mtcars$wt

# fit the model with "short ton" scaled x
# to scale x we need to divide x by 2 == change predictor slope interpretation from 1,000 to 2,000 pounds increments
fit2 <- lm(y ~ I(x/2))
summary(fit2)

# coefficient table
coef.table <- coef(summary(fit2))
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)  37.28513   1.877627 19.857575 8.241799e-19
# I(x/2)      -10.68894   1.118202 -9.559044 1.293959e-10

# calculate confidence interval...
(coef.table[2,1] + c(-1, 1) * qt(.975, df = fit2$df) * coef.table[2, 2])
# [1] -12.97262  -8.40527









## Question 7:
# if my x from a linear regression is measured in centimeters:
# to conver to meters what would happen to the slope coefficient?


## Solution: test case prediction
# define data
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

# fit the model - no scaling
fit <- lm(y ~ x)

# extract the x coefficient value
fit$coef[2]
# x 
# 0.7224211


# define data
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

# fit the model - with scaling
fit <- lm(y ~ I(x/100))

# extract the x coefficient value
fit$coef[2]
# I(x/100) 
# 72.24211 

# the slope coefficient will be multiplied by 100!!!







## Question 8: 
# if outcome Y, and predictor X fit a regressio with Y = intercept + slope(x1) + error
# we obtain the intercept and the slope coefficient
# what would happen to the slope and intercept if we refit the model with a new regressor = X + constant?

## Solution: test case prediction
# define data
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

# fit the model - no scaling
fit <- lm(y ~ x)

# extract the x coefficient value
fit$coef
# (Intercept)           x 
# 0.1884572   0.7224211 


# define data
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

# fit the model - with scaling
# note the slope of the line will not change with the added constant!!
# the intercept will be scaled to the value of average value of the slop plus the constant
fit <- lm(y ~ I(x + 10))

# extract the x coefficient value
fit$coef
# (Intercept)   I(x + 10) 
# -7.0357536   0.7224211 

# the coefficient  of the model will be equal to: intercept - 10*slope
fit$coef[1] - 10 * fit$coef[2] 
# (Intercept) 
# -7.035754 







## Question 9:
# refer back to the mtcars data set with mpg as outcome and weight as predictor
# what is the ratio of the sum of squared errors:
        # when comparing model with intercept only vs. model with slope and intercept

## Solution: fit intercept only model and compare against model with slope and intercept
# key point: a model with the just the intercept will be the mean of the predictor!!!!
data(mtcars)

# define variables
y <- mtcars$mpg; x <- mtcars$wt

# fit the slope and intercept model
fit.car <- lm(y ~ x)

# get the residuals
sr <-  sum(resid(fit.car)^2)

# get the residuals for the intercept only = sum(difference between y and mean(y) ^2)
sri <- sum((y - mean(y))^2)

# proportion of the sum of square errors between the two "fits"
(SSE.diff <- sr / sri)
# [1] 0.2471672





## Question 10:
# do the residuals always have to sum to 0 in linear regression?

# Solution: yes when the intercept is included
data(mtcars)

# define data
y <- mtcars$mpg
x <- mtcars$wt

# fit with an intercept
fit_car <- lm(y ~ x)

# extract residuals and see if they are zero? yes
sum(resid(fit_car))
# [1] -1.637579e-15

# fit without an intercept
fit_car_noic <- lm(y ~ x - 1)

# extract residuals and see if they are zero? NO!!! = NO INTERCEPT, residuals DO NOT HAVE TO SUM TO 0!!!
sum(resid(fit_car_noic))
# [1] 98.11672





## swirl practice
# 5 and 6 of the regression models course


## Week 3: Multivariate Regression

# linear regression with many predictors not just one
# one predictor can be two simple a explainer of the relationship
# other relationships we know or do not know may be more explanatory of the relationship
# what other variables explain relationships?
# multiplicity = many variables and just settled down on one that is the "best"
# is it breath mints or smoking as the best variable? - is there a mint effect beyond smoking?
# split grouping into smoker and not smoker to see if there is a breath mint effect
# linear regression tries to do this! hold both variables accountable in the relationship!

# you can use multivariate linear regression for inference and prediction!
# i.e predicting time in the hospital from given variables
# we need more than just simple linear regression = multivariate
# what features do we include in the data?
# how do we avoid overfitting?
# consequences for throwing out important variables and for including not good variables
# we want parsimonious models that are easy to explain!
# multivariate regression is a great start for any prediction problem!!
# the key is a well thought-out linear model!

# linear model: more variables than the simple model
# Y = B0 + B1X1 + .... BnXn + error term
# Bo = intercept; B1...BN are coefficient slopes based on predictors!; also need the error term
# least squares: differences between actuals and predicited observations: this is the same equation we wanted to minimize in simple linear regression!
# NOTE: the important linearity is linearity in the COEFFICIENTS (slopes)

# how do we get the estimates?
# we push the least squares estimates for one variable and extrapolate it to multiple variables
# least squares: (yi - x1iB1 - x2iB2)^2
# we do this by putting together multiple "origin regressions" for each predictor!
# we can substitute the value of B2 to be a equation with just B1!!
# B1 is the coefficient after removing x1 out of response and the second predictor!!
# any coefficient is the effect after any other predictor has been removed from repsonse and other predictors!!
# the fitted coefficient for B1 is regressio through the origin after removing x2 from repsone Y and the first regression variable X1
# X1 has been adjusted for the effect of X2
# this is the same as you "go down the line" = X2 is the linear effect of removing x1 from response and X2

# you get rid of the other variable from repsonse and from x1!!!
# this is the same for x2!!
# B1 = cor(X,Y) * sd(Y) / sd(X)
# we are trying to minimize the least squares estiamte from each variable
# we still want to minimize the distances from actual - observed
# for each variable in the model - we adjust Y and the x1, then keep going down the line!!

# R examples:

# define the data
# three predictors 
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)

# fit models
# y = intercept plus all coefficients defined above with error term
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)

# residuals for x2 and x3
ey = resid(lm(y~x2 + x3))
# residuals for x on x2 and x3
ex = resid(lm(x ~ x2 + x3))

# observe results of the residuals
sum(ey*ex) / sum(ex^2)
# [1] 1.000173
coef(lm(ey ~ ex - 1))
# ex 
# 1.000173 

# observe the coefficients
# notice the x coefficient is the same as the residuals on y removing and and x on x2, x3
# this is how linear models adjust by "removing" the effect of the other coefficients from y and x
coef(lm(y ~ x + x2 + x3))
# (Intercept)           x          x2          x3 
# 1.002080    1.000173    1.010692    1.001398

# the interpretation of a multivariate regression coefficient is the expected change in repsonse:
# per unit change in regressor, holding all of the other regressors fixed!!!!
# expected change in repsonse while holding all other regressors fixed!!!!!!!
# LEARN THIS YOU SON OF A BITCH!!! YOU WHORE!!! YOU CAN'T FUCKING DO STATS IF YOU DON'T KNOW THIS SHIT!!
# hold the other predictors constant = ADJUSTING FOR THE OTHER VARIABLES INCLUDED IN THE MODELS!!!


# properties of the multivar regression are the same as simple just more added to them
# model = Y = sum(XikBk) + ei where ei ~ N (0, sigma^2)
# fitted responsed = Yihat = sum(Xik * Bk)
# residuals ei = Yi (observed) - Yihat (predicted)
# Variance estimate (sigma squared) = 1 / (n - p) *sum(ei^2) = average squared residuals!!
# to get predicted responses at new values of x = simply plug them into the linear model!
# each coeffiecnt will have standard errors
# we can test coefficients are 0 with t tests - coefficients will follow a t distribution
# prediction repsonses have standard errors and we can calculate predicted and expected repsonse intervals

# linear models are the single most important applied statistical machine learning
# they should always be our starting point!!
# decompose a signal into its harmonics using linear regression
# flexibly fit complicated functions using linear functions (curves)
# fit factor variables as predictors = ANOVA, ANCOVA are cases of linear regresion
# uncover complex multivariate relationships with the response by using linear regression
# build accurate prediction models with linear regression


## multivariate regression examples
require(datasets)
data(swiss)
?swiss

# fertility measures with socio-economic indicators for french provinces
# what explains fertility in this province?

# exploratory pairs plot
install.packages("GGally")
library(GGally)

# ggpairs plot in ggally
# https://stackoverflow.com/questions/35085261/how-to-use-loess-method-in-ggallyggpairs-using-wrap-function/35088740
(g = ggpairs(data = swiss,
            lower = list(continuous = "smooth")))
# correlation between variables given in the top upper triangle
# scatterplots and lm relationships are shown on each y axis
# you can see correlation and plot vs the repsonse y and all other variables
# how we read a pairs plot?
# correlation between ferility and argiculture is .35

# lets fit some models...
# all predictors...
# grab just the coefficient table - you can split this table to use in other code
# estimate = our model estiamtes an expected .17 decrease in fertility for every 1% increase in males involved in agricultre
        # THIS IS HOLDING ALL OTHER VARIABLES CONSTANT = KEY COMPONENT OF MULTIVAR REGRESSION!
# standard error = how precise our estimate of the coefficient is! variability of the coefficient
        # we can perform a hypothesis test - t stat = estimate divided by standard error
# t value = estimate divided by standard error
# P value = getting value as extreme as the value given = p value tells you the coefficient is statistically signfiicant!
summary(lm(Fertility ~ ., data = swiss))$coefficients
# Estimate  Std. Error
# (Intercept)      66.9151817 10.70603759
# Agriculture      -0.1721140  0.07030392
# Examination      -0.2580082  0.25387820
# Education        -0.8709401  0.18302860
# Catholic          0.1041153  0.03525785
# Infant.Mortality  1.0770481  0.38171965
# t value     Pr(>|t|)
# (Intercept)       6.250229 1.906051e-07
# Agriculture      -2.448142 1.872715e-02
# Examination      -1.016268 3.154617e-01
# Education        -4.758492 2.430605e-05
# Catholic          2.952969 5.190079e-03
# Infant.Mortality  2.821568 7.335715e-03




# let's use only one predictor vs. fertility
# we notice the estimate has changed signs!! how can this be?
# now it has a positive effect on fertility now - this is simpson's paradox!
# adjusting for other variables changes the direction of agriculture on fertility!!
# accounting for other variables can change the nature of the relationship between predictors and repsonse!!
# agriculutre coefficient is still statistically significant
# regression is a dynamic process that you have to select the accurate features!
# feature selection is important - put in the right cofounders!
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients
# Estimate Std. Error
# (Intercept) 60.3043752 4.25125562
# Agriculture  0.1942017 0.07671176
# t value     Pr(>|t|)
# (Intercept) 14.185074 3.216304e-18
# Agriculture  2.531577 1.491720e-02


# simulation example:
# how can adjustment reverse the sign of an effect?
# happiness is negatively on x1 but positively on x2 with random noise

# time
n <- 100; 
x2 <- 1:n; 
# money
x1 <- .01*x2 + runif(n,-.1,.1);
# happiness
y = -x1 + x2 + rnorm(n,sd = .01)

# random noise around money line
plot(x1)


# plot the models - happiness on money
# extremely large coefficent no where near the "true" model defined above
# we do not "see" x2 and that relationship is not known!!
summary(lm(y ~ x1))$coefficients
# Estimate Std. Error
# (Intercept)  1.730082   1.221910
# x1          97.121318   2.127758
# t value     Pr(>|t|)
# (Intercept)  1.415884 1.599800e-01
# x1          45.644916 7.699705e-68


# plot the models - time on money
summary(lm(y ~ x2))$coefficients
# Estimate   Std. Error
# (Intercept) -0.004720147 0.0126065983
# x2           0.990264439 0.0002167279
# t value      Pr(>|t|)
# (Intercept)   -0.3744187  7.089017e-01
# x2          4569.1599363 6.474924e-263

# with the correct adjustments!!
# happiness on time AND money!!!
# we adjusted and selected the right features and our model picked out the "true" relationships
# money is now negatively associated with happiness at the "true" model estimate -1
# time is now positively associated with happiness at the "true" model estimate 1
summary(lm(y ~ x1 + x2))$coefficients
# Estimate   Std. Error
# (Intercept)  0.0003770888 0.0021563072
# x1          -1.0110204120 0.0177115605
# x2           1.0001144234 0.0001764873
# t value      Pr(>|t|)
# (Intercept)    0.1748771  8.615407e-01
# x1           -57.0825146  1.876810e-76
# x2          5666.7790355 1.556618e-269


# let's plot the results
# data frame of y, x1, x2 and the residuals
dat <- data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1~x2)))

# ggplot
# result is how money relates to happiness based on the value of time!!
# lower time "color" = relationship is negative between money and happiness
# higher time "color" = relationship is positive between money and happiness
# the positive relationship is confounding between the two variables!
library(ggplot2)
ggplot(data = dat, aes(y = y, x = x1, color = x2)) +
        geom_point(color = "grey50", size = 5) +
        geom_smooth(method = "lm", se = F, color = "black") +
        geom_point(size = 4)

# plotting the residuals of the test case above
# plots the slope of coefficient including both time and money
# now we see the strong negative relationship of money on happiness
# for residual y and resiudal x1 the slope should be around -1
# the x2 variable is clearly not related to the x1 residual variable
# multivar regression removes the effect of the other variables! KEY CONCEPT!!
# x2's effect has been removed from x1 and y!!!!!!!!!!!! FUCK YOU YOU PEICE OF SHIT!
# throwing every variable is not the key - there are consequences of throwing to many spurious variables in the model
ggplot(data = dat, aes(y = ey, x = ex1,color = x2)) +
        geom_point(color = "grey50", size = 5) +
        geom_smooth(method = "lm", se = F, color = "black") +
        geom_point(size = 4)

# back to the original dataset
# what did we learn?
# the sign reverses itself with inclusion of Examination and Education
# the percent of males in the province working in agriculture is negatively related to education attainment (-.6395)
# Education and Test Examination (correlation of .6984) are measuring similiar things!
        # is the positive marginal an artificat between agriculture and feritlity - for not accounting Education Level?
# anyone claiming the provinces with more agriculture see higher fertility rates is immediatley open to criticism!!!
# even just claiming an association between agricultre and fertility is also suspect! 
        # you can easily "alter" this relationship by including other "well thought out" variables!!!




# what happens when we put in "bad" variables into our model?
z <- swiss$Agriculture + swiss$Education

# fit model with "bad" varaible: education and agricultre are already included!
# if we include this variable - R can detect it is an exact combination of other variables already included!
# you will get an NA for the z variable in this case
lm(Fertility ~ . + z, data = swiss)
# Call:
#         lm(formula = Fertility ~ . + z, data = swiss)
# 
# Coefficients:
#         (Intercept)       Agriculture  
# 66.9152           -0.1721  
# Examination         Education  
# -0.2580           -0.8709  
# Catholic  Infant.Mortality  
# 0.1041            1.0770  
# z  
# NA 



## Multivariate Regression Examples: Part 2
# Dummy variables: linear regression is very flexible
# you can fix factor variables and then use ANOVA on the factors

# model with one predictor = Y = B0 + Xi1B1 + error
# with Xi takes on the value of 1 or 0 control or not control group
# E[Yi] = B0 + B1 in the control
# E[Yi] = B0 for people not in the control group (i.e x = 0)

# the mean of the treated is the mean of the treated, b0 hat is the mean of the control group
# b1 is interpretted as the increase or decrease in the mean comparing those in the group to those not in the group
# this allows you to fit a two level facotr variable of the two groups: you get inference between the two groups
# you also get the means for each group: t test for b1 is the same as a two group t test 

# we can extend this to 3 or more variables!
# model: Y = B0 + x1(repub or not) + x2(demo or not) + error
# repub = B0 + B1
# demo = B0 + B2
# independent = B0 the intercept both x1 and x2 are 0!!!
# b1 compares repub to ind
# b2 compares demo to ind
# b1 - b2 compares Repub to Demo!!
# the intercept becomes the value for the independents - all other variables are based off comparisions to independents!
# we can switch around the omitted variable to switch around the interpretability!
# R picks a "reference" level for you!
# factor variables depends on the reference level = coefficient interpretation will change depending on the the refence factor level!

# data example
data("InsectSprays")
require(stats)

# violin plot!!
# histogram type - shows where and the volume of each type of spray
ggplot(data = InsectSprays, aes(y = count, x = spray, fill = spray)) +
        geom_violin(color = "black", size = 2) +
        xlab("Type of Spray") + ylab("Insect Count")

# fit a model of count on spray
# spray is already a factor variable!
# all factors of the model are given a coefficent except spray A - use the intercept as the spray A coefficient
# all other factors are relative to spray A!! = spray A is the reference factor level!
# coefficient is the difference from spray A and the coefficient spray!
# to interpret other coeffiicents we will have to substract one factor level from the other (ex. Spray B - Spray D)
summary(lm(count~spray, data = InsectSprays))$coefficients
# Estimate Std. Error
# (Intercept)  14.5000000   1.132156
# sprayB        0.8333333   1.601110
# sprayC      -12.4166667   1.601110
# sprayD       -9.5833333   1.601110
# sprayE      -11.0000000   1.601110
# sprayF        2.1666667   1.601110
# t value     Pr(>|t|)
# (Intercept) 12.8074279 1.470512e-19
# sprayB       0.5204724 6.044761e-01
# sprayC      -7.7550382 7.266893e-11
# sprayD      -5.9854322 9.816910e-08
# sprayE      -6.8702352 2.753922e-09
# sprayF       1.3532281 1.805998e-01



# hard code the same example as above
# first model: count was outcome and spray was factor with spray A as reference factor level
# use I function to hard code spray A as the reference level
# this will gives us the same results as above!
summary(lm(count ~
                   I(1*(spray =="B")) + I(1 * (spray == "C")) +
                   I(1*(spray == "D")) +I(1 * (spray == "E")) +
                   I(1*(spray == "F")), data = InsectSprays))$coef
# Estimate
# (Intercept)            14.5000000
# I(1 * (spray == "B"))   0.8333333
# I(1 * (spray == "C")) -12.4166667
# I(1 * (spray == "D"))  -9.5833333
# I(1 * (spray == "E")) -11.0000000
# I(1 * (spray == "F"))   2.1666667
# Std. Error
# (Intercept)             1.132156
# I(1 * (spray == "B"))   1.601110
# I(1 * (spray == "C"))   1.601110
# I(1 * (spray == "D"))   1.601110
# I(1 * (spray == "E"))   1.601110
# I(1 * (spray == "F"))   1.601110
# t value
# (Intercept)           12.8074279
# I(1 * (spray == "B"))  0.5204724
# I(1 * (spray == "C")) -7.7550382
# I(1 * (spray == "D")) -5.9854322
# I(1 * (spray == "E")) -6.8702352
# I(1 * (spray == "F"))  1.3532281
# Pr(>|t|)
# (Intercept)           1.470512e-19
# I(1 * (spray == "B")) 6.044761e-01
# I(1 * (spray == "C")) 7.266893e-11
# I(1 * (spray == "D")) 9.816910e-08
# I(1 * (spray == "E")) 2.753922e-09
# I(1 * (spray == "F")) 1.805998e-01



# what happens when we include spary A in the hard coded model?

# original model:
summary(lm(count~., data = InsectSprays))$coef

# hard coded model now with spray A included:
# spray A will now populate as NA
# spray A is redundant - 6 means and 7 parameters - will drop one - we already have the intercept!
summary(lm(count ~
                   I(1*(spray =="B")) + I(1 * (spray == "C")) +
                   I(1*(spray == "D")) +I(1 * (spray == "E")) +
                   I(1*(spray == "F")) + I(1*(spray == "A")), data = InsectSprays))

# Call:
#         lm(formula = count ~ I(1 * (spray == "B")) + I(1 * (spray == 
#                                                                     "C")) + I(1 * (spray == "D")) + I(1 * (spray == "E")) + I(1 * 
#                                                                                                                                       (spray == "F")) + I(1 * (spray == "A")), data = InsectSprays)
# 
# Residuals:
#         Min     1Q Median     3Q    Max 
# -8.333 -1.958 -0.500  1.667  9.333 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate
# (Intercept)            14.5000
# I(1 * (spray == "B"))   0.8333
# I(1 * (spray == "C")) -12.4167
# I(1 * (spray == "D"))  -9.5833
# I(1 * (spray == "E")) -11.0000
# I(1 * (spray == "F"))   2.1667
# I(1 * (spray == "A"))       NA
# Std. Error
# (Intercept)               1.1322
# I(1 * (spray == "B"))     1.6011
# I(1 * (spray == "C"))     1.6011
# I(1 * (spray == "D"))     1.6011
# I(1 * (spray == "E"))     1.6011
# I(1 * (spray == "F"))     1.6011
# I(1 * (spray == "A"))         NA
# t value Pr(>|t|)
# (Intercept)            12.807  < 2e-16
# I(1 * (spray == "B"))   0.520    0.604
# I(1 * (spray == "C"))  -7.755 7.27e-11
# I(1 * (spray == "D"))  -5.985 9.82e-08
# I(1 * (spray == "E"))  -6.870 2.75e-09
# I(1 * (spray == "F"))   1.353    0.181
# I(1 * (spray == "A"))      NA       NA
# 
# (Intercept)           ***
#         I(1 * (spray == "B"))    
# I(1 * (spray == "C")) ***
#         I(1 * (spray == "D")) ***
#         I(1 * (spray == "E")) ***
#         I(1 * (spray == "F"))    
# I(1 * (spray == "A"))    
# ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
# ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.922 on 66 degrees of freedom
# Multiple R-squared:  0.7244,	Adjusted R-squared:  0.7036 
# F-statistic:  34.7 on 5 and 66 DF,  p-value: < 2.2e-16



# can we give mean for each of the groups? we need to remove the intercept!!
# now we have associated mean estimates for each factor level!
# there is no intercept term and no comparision back to a reference factor level!
# we have six parameters and 6 means to work with!!
# this model is not different than the model with the intercept - the coefficients just have a different interpretation!!
# with intercept the t test tests against the reference level!!!
# without intercept the t test test the coefficient is or is not zero!!!

summary(lm(count~spray -1, data = InsectSprays))$coef
# Estimate Std. Error   t value
# sprayA 14.500000   1.132156 12.807428
# sprayB 15.333333   1.132156 13.543487
# sprayC  2.083333   1.132156  1.840148
# sprayD  4.916667   1.132156  4.342749
# sprayE  3.500000   1.132156  3.091448
# sprayF 16.666667   1.132156 14.721181
# Pr(>|t|)
# sprayA 1.470512e-19
# sprayB 1.001994e-20
# sprayC 7.024334e-02
# sprayD 4.953047e-05
# sprayE 2.916794e-03
# sprayF 1.573471e-22


# reording the levels
# spray C is now the refence level
# all  other factors are interpreted as the value different from spray C now!
spray2 <-  relevel(InsectSprays$spray, "C")
summary(lm(count~spray2, data = InsectSprays))$coef
# Estimate Std. Error
# (Intercept)  2.083333   1.132156
# spray2A     12.416667   1.601110
# spray2B     13.250000   1.601110
# spray2D      2.833333   1.601110
# spray2E      1.416667   1.601110
# spray2F     14.583333   1.601110
# t value     Pr(>|t|)
# (Intercept) 1.840148 7.024334e-02
# spray2A     7.755038 7.266893e-11
# spray2B     8.275511 8.509776e-12
# spray2D     1.769606 8.141205e-02
# spray2E     0.884803 3.794750e-01
# spray2F     9.108266 2.794343e-13



## Summary: 
# if we treat Spray as a factor: R includes an intercept and omits the first level of the factor
# all t-tests for comparisions of Spray are vs. the reference factor "Spray A"
# all means of Spray are also vs. the reference factor
# if we omit an intercept - means are of each factor and t test are comparisions against 0
# revel allows you to choose the reference level of the model

## Thoughts on the data
# counts are bounded from below by 0 - may be more natural to bound by Possion = "GLM"
# variance does not look constant - means look correct but inference does not look correct
# inequal variances = heterskedasticity


## Multivariate Regression: Part 3
# ANCOVA
# fitting multiple lines with different lines and different slopes
library(datasets); data(swiss)
head(swiss)
# Fertility Agriculture
# Courtelary        80.2        17.0
# Delemont          83.1        45.1
# Franches-Mnt      92.5        39.7
# Moutier           85.8        36.5
# Neuveville        76.9        43.5
# Porrentruy        76.1        35.3
# Examination Education
# Courtelary            15        12
# Delemont               6         9
# Franches-Mnt           5         5
# Moutier               12         7
# Neuveville            17        15
# Porrentruy             9         7
# Catholic Infant.Mortality
# Courtelary       9.96             22.2
# Delemont        84.84             22.2
# Franches-Mnt    93.40             20.2
# Moutier         33.77             20.3
# Neuveville       5.16             20.6
# Porrentruy      90.57             26.6

library(dplyr)
# data is very bimodal
hist(swiss$Catholic)

# create a binary catholic variables
# we will now have a column Catholic Bin that is 1 when the Catholic proportion is greater than 50
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))
head(swiss)
# Fertility Agriculture Examination
# 1      80.2        17.0          15
# 2      83.1        45.1           6
# 3      92.5        39.7           5
# 4      85.8        36.5          12
# 5      76.9        43.5          17
# 6      76.1        35.3           9
# Education Catholic Infant.Mortality
# 1        12     9.96             22.2
# 2         9    84.84             22.2
# 3         5    93.40             20.2
# 4         7    33.77             20.3
# 5        15     5.16             20.6
# 6         7    90.57             26.6
# CatholicBin
# 1           0
# 2           1
# 3           1
# 4           0
# 5           0
# 6           1

# plot the data
# we can plot the data by the dummy variable for catholic provinces
g = ggplot(swiss, aes(x = Agriculture, y = Fertility, color = as.factor(CatholicBin))) +
        geom_point(size = 6, color = "black") +
        geom_point(size = 4) +
        xlab("% in Agriculture") +
        ylab("Fertility")

# let's try to fit a model for each factor level of the CatholicBin dummy variable
# model1 = E[Y | x1, x2] = B0 + B1x1 = this would diregard religion
# model2 = E[Y | x1, x2] = B0 + B1x1 + B2x2 = this includes religion because x1 and x2 will rotate and 1 and 0!!
# model3 = E[Y | x1, x2] = B0 + B1x1 + B2x2 + b3x1*x2 = interactions between the two xs
        # the third model will fit two lines with different slopes and intercepts!

# examples with code:

# fit a model - does not include religion at all
# our initial model will disregard the religion variable entirely
fit = lm(Fertility ~ Agriculture, data = swiss)
# plot the data
# fit the line over the previous graph
g1 = g 
g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)

# coefficient summary
summary(fit)$coef
# Estimate Std. Error
# (Intercept) 60.3043752 4.25125562
# Agriculture  0.1942017 0.07671176
# t value     Pr(>|t|)
# (Intercept) 14.185074 3.216304e-18
# Agriculture  2.531577 1.491720e-02



# fit a model including the Catholic variable as a factor
fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
summary(fit)$coef
# Estimate
# (Intercept)          60.8322366
# Agriculture           0.1241776
# factor(CatholicBin)1  7.8843292
# Std. Error
# (Intercept)           4.1058630
# Agriculture           0.0810977
# factor(CatholicBin)1  3.7483622
# t value
# (Intercept)          14.815944
# Agriculture           1.531210
# factor(CatholicBin)1  2.103406
# Pr(>|t|)
# (Intercept)          1.032493e-18
# Agriculture          1.328763e-01
# factor(CatholicBin)1 4.118221e-02

# plot the new line over the previous plot
# this plots two lines one for catholic =1 and one for catholic = 2 of the factor variable Catholic Bins
# we hard coded the correct slope and intercept for each level of the Catholic Bins variable
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)



# fit a model including all interaction terms of the factor variable
# all interactions are included as estimates of the model
fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
summary(fit)$coef
# Estimate  Std. Error
# (Intercept)                      62.04993019  4.78915566
# Agriculture                       0.09611572  0.09881204
# factor(CatholicBin)1              2.85770359 10.62644275
# Agriculture:factor(CatholicBin)1  0.08913512  0.17610660
# t value     Pr(>|t|)
# (Intercept)                      12.9563402 1.919379e-16
# Agriculture                       0.9727127 3.361364e-01
# factor(CatholicBin)1              0.2689238 7.892745e-01
# Agriculture:factor(CatholicBin)1  0.5061430 6.153416e-01


# let's plot these two interaction fits
# we have a line for mostly catholic
# we have a line for mostly non-catholic
# correct intercepts and slopes are defined below from the interaction lm fit!!
# we have two different intercepts and two different slopes for each religion bin
# the positive line will be the catholic line
# the "flatter" line will be the non-catholic line
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3],
                      slope = coef(fit)[2] + coef(fit)[4], size = 2)



## Adjustment:
# what is adjustment?
# adjustment is adding in other variables to examine the relationship between the other variables
# a new variable will distort or change the relationships from the current model
# new variables can confound with other variables - "take up" some or more the explained variance
# the relationship between response and predictor will change when new variables are added
# this is a key point of feature selection: finding the right features but also strategically adding features to investigate the relationship changes in other features!
# adjusting for one variable can change the relationship between other variables

n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# simulation 1
# in this case the horizontal lines show marginal effect of group status between red and blue
# the mariginal effect is disregarding x
# clear linear relationship between outcome and regressor
# change in intercept it the change between the y axis of line red and line blue
# we have pretty even observations across each line - resulting in a good experiement
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


# simulation 2
# adjustment making a big difference!
# straight horizontal lines are the mariginal difference not accounting for X
# we have a massive difference without x in the model
# including x we take the mariginal difference way down - including x significantly changes the relationship
# this data looks like we did not randomize - clear difference between blue and red
# treatments are clearly seperated
# strong marginal effect without x but very small effect by including x!!
# no instance of overlap between red and blue groups to compare to directly
# we are relying heavily on the model to compare groups
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


# simulation 3
# some group overlap between the two groups red and blue
# horizontal lines show means for each group = mariginal effect
# but adjusting we find the estimate flips!
# before adjustment blue greater than red
# after adjustment (including x) red effect greater than blue!
# SIMPSON'S PARADOX - NOT A PARADOX
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


# simulation 4
# not marginal effect between red and blue
# there is now a large effect when we adjust for X!!!
# any permutation between with and without adjustment can occur!
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# simulation 5
# slopes might not always be constant
# there is no such thing as treatment effect
# one side shows blue higher than red (right side)
# the other side shows red higher than blue (left side)
# there is no such thing as a treatment effect = it depends on the level of x you are out
# be careful when using interaction terms s
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1));
beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


# simulation 6
# nothing is specific to a binary treatment; x2 is color
# not alot of relationship between y and x1, but x2 (color) may show a different relatioship!
# most of the variation of y is explained by the relationship in x2!!
# we need to look at the residuals to see the relationship between x1 and x2
p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

# resiudal relationship
# plot is the residuals of x2 removing x1 versus the residuals of x1 modeled on x2
# we see a strong positive relationship = x2 explains almost all of the residuals in the full model
# there is a strong relationship left over for y on x1 after removing the effect of x2!!
plot(resid(lm(x1 ~ x2)), resid(lm(y ~ x2)), frame = FALSE, col = "black", bg = "lightblue", pch = 21, cex = 2)
abline(lm(I(resid(lm(x1 ~ x2))) ~ I(resid(lm(y ~ x2)))), lwd = 2)

# modeling multivariate relationships is difficult
# we have not shown how to pick the "Right" model only that models can change extremely when adding features!!
# we still need an answer to find the "right" model
# this is where you need to have SME knowledge of the problem to determine the correct features!!
# we need to determine if certain features are related to each other - if they both explain the same thing why are they in the model?
# need to constantly simulate features in and out of the model to get a sense of the relationships
# GET YOUR FUCKING HANDS DIRTY IN THIS SHIT!! COMP, BUSINESS, STATS, together as one to choose the right model!!




## swirl practice




## Residuals and Diagnostics and Variation
# we start with linear model with iid error
# estimate of resiudal variation
# very easy to get plot of residuals and plots that judge model fit

# plot examples
data(swiss)

#fit model
fit <- lm(Fertility ~ ., data = swiss)
summary(fit)$coef
# Estimate  Std. Error   t value
# (Intercept)      66.9151817 10.70603759  6.250229
# Agriculture      -0.1721140  0.07030392 -2.448142
# Examination      -0.2580082  0.25387820 -1.016268
# Education        -0.8709401  0.18302860 -4.758492
# Catholic          0.1041153  0.03525785  2.952969
# Infant.Mortality  1.0770481  0.38171965  2.821568
# Pr(>|t|)
# (Intercept)      1.906051e-07
# Agriculture      1.872715e-02
# Examination      3.154617e-01
# Education        2.430605e-05
# Catholic         5.190079e-03
# Infant.Mortality 7.335715e-03

# residuals and diagnostic plots
# you will get four plots:
# residuals vs. fitted, Normal Q-Q, Scale-Location, Resiudals vs. Leverage
plot(fit)

# influential  high leverage points vs. outlier points
# there is a distinction between these two types of points 
# middle of the cloud of data = little influence or leverage
# on the same line but far away from the center density = HIGH LEVERAGE 
# futher away but on the line = MORE LEVERAGE!! - think of an actual flucrum!!
# leverage = how far away from the center of the data
# influcence = if the high leverage point "uses" its leverage
# far away point leverage point has influcence if it does not adhere to the natural relationship of the data 
# it is not using it leverage = NOT INFLUENTIAL
# INFLUENTIAL = HIGH LEVERAGE and OUTSIDE FOR THE NORMAL FIT OF THE DATA!!
# how can we diagnose these points?
# OUTLIERS can exert influence and leverage - 
# LEVERAGE = AWAY FROM THE CENTER OF THE DATA CLOUD
# INFLUENCE = OUTSIDE OF THE NORMAL DATA TREND

## Residuals, Diagnostics and Variation Part 2
# plot summary
# outlier is very vague
# outliers can be the result of a real or spurios processes
# remove if spurious, keep if generated from the actual process
# outliers can conform to the regression relationship i.e. outlying X or Y but not outlying the regression relationship
# there are a lot of diagnostic measures for outliers = influence.measures
# rstandard and rstudent are themes on residuals
# these are attempts to standarize the residuals so you can compare to baseline and determine if outlier
# basically calculate with and without point and see results differ
# hatvalues = measure of influence - far away from the x values (does not have to use the leverage)
# hatvalues is great for finding data entry errors
# how do we measure actual influence? = dffits, dfbetas
# take out data point - re fit model - figure out the change with point in or not in the data
# dfbetas - how much to slope coefficients change with and without the point included in the data
# cook's distance = overall change in slope coefficients
# resid = returns the ordinary resiudals
# cross validated residuals = resid(fit) / (1 - hatvalues(fit)) = "press residuals" = leave one out cross validated error
# all of these measures help detect outliers, influence, leverage without re-fitting the model - run the functions as is
# all these measures probe the data to diagnose the data and the model
# residual plots = resiudals vs. fitted values = any systematic pattern is BAD!
# Q-Q plot = look for normality
# plot of leverage values = which points have high leverage
# influence measures = bottom line - if point removed how is everything affected?



## Resiudals, Diagnostics, Variation Part 3
# Case 1: very far away point in top right
# the fit is developed based highly on this one influential point 
# all other values are grouped non-linearly in a cloud below the high leverage point

# code example: model fit without the high leverage point (10,10)
# the point has createed a strong regression relationship "by itself"
n <- 100; x = c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x,y, frame = F, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y~x))

# let's look at the diagnostic measures for this point
fit <- lm(y ~ x)

# dfbetas example: point 10 10 is point #1 with a high slope coefficient change: 5.613!!
round(dfbetas(fit)[1:10, 2],3)
# 1      2      3      4      5      6      7      8      9     10 
# 5.613 -0.102 -0.008  0.028  0.030 -0.034  0.009  0.002  0.000 -0.202 

# hatvalues example: again point 10 10 is large compared to other values
# hat values need to be between 0 and 1
# we should single this point out
round(hatvalues(fit)[1:10],3)
# 1     2     3     4     5     6     7     8     9    10 
# 0.505 0.023 0.010 0.028 0.021 0.012 0.029 0.010 0.019 0.017 


# now let's look at another point where the outlier "fits" into the regression line
# new example fit will have an outlier far away from x but still on the regression line
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)

# dfbetas diagnostics
# outlying point is still large but not as large
# some influence but not much
round(dfbetas(fit2)[1:10, 2],3)
# 1      2      3      4      5      6      7      8      9     10 
# 0.313 -0.007  0.005 -0.200 -0.032  0.074  0.010  0.121 -0.081  0.127 

# hatvalues diagnostics
# our new outlier is very large under hatvalues
# outlier is outside of x value range but it does adhere to the regression line
# will have high LEVERAGE VALUE but not LARGE INFLUENCE
round(hatvalues(fit2)[1:10],3)
# 1     2     3     4     5     6     7     8     9    10 
# 0.217 0.010 0.013 0.068 0.014 0.023 0.010 0.022 0.014 0.014 

# example by Stefanski TAS 2007 Vol 61
# residual plots room in to potential problems with our data!!
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)

# fitted the example model
# hmmm...all P values are highly significant...what does this mean? should we use this?
summary(lm(V1 ~ . -1, data = dat))$coef
# Estimate Std. Error   t value     Pr(>|t|)
# V2 0.9856157 0.12798121  7.701253 1.989126e-14
# V3 0.9714707 0.12663829  7.671225 2.500259e-14
# V4 0.8606368 0.11958267  7.197003 8.301184e-13
# V5 0.9266981 0.08328434 11.126919 4.778110e-28

# OH REALLY? YOU MOTHERFUCKER THINKS THIS IS A GOOD MODEL - LOOK AT THE FUCKING RESIDUALS!!
# without looking at the residuals we would not have seen anything - look at the clear systematic pattern - this is a shit model!
# a linear relationshuip will not work - we want to model this systematic variation
# NEED TO ALWAYS CHECK RESIDUALS YOU GODDAMN PEICE OF SHIT!
fit <- lm(V1 ~ . - 1, data = dat); 
plot(predict(fit), resid(fit), pch = '.')


## DIAGNOSTIC PLOTS EXPLAINED

# PLOT 1: RESIDUALS VS. FITTED:
# we do not want to see a systematic pattern!!
# we want variance to be constant!!
# patterns mean we are not modelling the systematic variance

# PLOT 2: NORMAL Q - Q PLOT:
# plot to test the normality of the error terms
# results on a straight line represent residuals that are normally distributed

# PLOT 3: SCALE - LOCATION PLOT:
# plots standardized residuals against the fitted values "like a t-stat"
# same as residuals vs. fitted but with a different residual scale
# great for looking at residuals through different models = they will be "scaled" to be comparable between models

# PLOT 4: RESIDUALS VS. LEVERAGE:
# plots standardized resiudals vs. leverage
# leverage is distance away from the x values = CAN BE INFLUENTIAL!
# looking for any systematic pattern or values that may be outliers!
# point may have a small resiudal but large leverage = one the line but far away from the X cloud





## MODEL and FEATURE SELECTION:
# how do we choose what variables to include in a regressio model?
# no single answer "it depends" but we should develop techniques to help us 
# there are consequences to under and overfitting a model!!
# in regression we want simple interpretable models - parsamonious!!
# the simplest solution is probably the best - linear regression
# if a variable explains a little bit more but hurts interpretability we will exclude that variable
# a model is a lens you are looking at your data - no "right" or "wrong"
# you can learn things from simple models - different lenses can give you different information
# what happens when we include bad predictors or exclude good predictors
# unknown unknown - things we don't know that we should include
# known unknowns - things we would like to include but don't have
# known knowns - regressors we should check to include in the regression model

# general rules:
# omitting a variable we should include = BIAS = not an accurate estimate = increase stand error! 
# including variables we shouldn't have = reduction of bias but INCREASE in standard error!!
# R squared will increase as you include more regressors - even ones that don't matter

## MODEL and Feature Selection Part 2:

# simulation example:
# define the n and the three random normal regressors
# investigate how x1 "moves" as we add more variables into the variance of x1
# first simulation - variables are not related to each other
n = 100; nosim = 1000
x1 = rnorm(n); x2 = rnorm(n); x3 = rnorm(n);
betas = sapply(1:nosim, function(i){
        y = x1 + rnorm(n, sd =.3)
        c(coef(lm(y ~x1))[2],
          coef(lm(y ~ x1 + x2))[2],
          coef(lm(y ~ x1 + x2 + x3))[2])
})

round(apply(betas, 1, sd), 5)
# x1      x1      x1 
# 0.03280 0.03284 0.03287 

# simulation example: x2 and x3 will now depend heavily on x1
# now the variablilty increases a lot - standard error will increase alot
# if the variable you include is highly correlated with the measure you are aiming for - you will add standard error
# the more correlated the covariates are to the regressors the more increase in standard deviation
# NO FREE LUNCH!
# if we omit variables that are important - BIAS; include variables that are correlated = STANDARD ERROR
n = 100; nosim = 1000
x1 = rnorm(n); x2 = x1/sqrt(2) + rnorm(n); x3 = x1 + .95 + rnorm(n) * sqrt(1 - .95^2);
betas = sapply(1:nosim, function(i){
        y = x1 + rnorm(n, sd =.3)
        c(coef(lm(y ~x1))[2],
          coef(lm(y ~ x1 + x2))[2],
          coef(lm(y ~ x1 + x2 + x3))[2])
})

round(apply(betas, 1, sd), 5)
# x1      x1      x1 
# 0.03012 0.03508 0.09609 


# variance inflation factors
# variance increases when we included a variable that was highly related to x1
# we can only estimate the increase in variance
# the variance inflation factor is the increase in the variance compared to the ideal setting when unrelated variables
# we may want to include certain variables even if they dramatically inflate our variance
# case we are in vs ideal case (all variables are uncorrelated)

# simulation example: 
# variance inflation factor - need to install package "car"
# vif = the standard error is multiplied by the vif value for included the variable as opposed to have all orthogonal variables (uncorrelated)
# this is what can happen if you have highly correlated variables to a model: VIF can help you investigate
install.packages("car")
library("car")
fit = lm(Fertility ~ ., data = swiss)
vif(fit)
sqrt(vif(fit))


# resiudal variance estimation
# if we underfit a model the variance estimate is biased
# if we overfit the model than the variance is unbiased, however the variance of the variance is larger if we include bad variables
# principal component or factor analytic models on covariates are often useful for reducing complex covariate spaces
# these uses decrease interpretability
# good design can eliminate the need for complex model searches = RANDOMIZATION - experiment design and cross validation
# nested models = different subsets of models and using them to compare against each other - add more variables as you go

# nested model testing
# build multiple models with increasing number of predictors
fit1 = lm(Fertility ~ Agriculture, data = swiss)
fit3 = update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 = update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)

# view the effect of each model as we added variables
# F stat gives you the yes inclusion of these variables is needed over just choosing the previous level
# significant is included the newest variable "matters" to the model than just including the previous set of variables
anova(fit1, fit2, fit3)
# Analysis of Variance Table
# 
# Model 1: Fertility ~ Agriculture
# Model 2: Fertility ~ Agriculture + Examination + Education
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     45 6283.1                                  
# 2     43 3180.9  2    3102.2 20.968 4.407e-07 ***
#         ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ALL MODELS ARE WRONG SOME ARE USEFUL!!! LENS TO TEACH US SOMETHING USEFUL ABOUT OUR DATA!!!




## Week 3 Regression Models: Quiz
# https://rpubs.com/cheyu/reg-q3
# https://rpubs.com/bjyenis/242552


## Question 1:
# consider the mtcars dataset - fit a model with mpg on cylinders and weight
# give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4

# solution: fit model and interpret based on cylinder factor level
# load data
data("mtcars")
str(mtcars)
# fit model
fit <- lm(mpg ~ as.factor(cyl) + wt, data = mtcars)
# intrepret coefficients
summary(fit)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)     33.990794  1.8877934 18.005569 6.257246e-17
# as.factor(cyl)6 -4.255582  1.3860728 -3.070244 4.717834e-03
# as.factor(cyl)8 -6.070860  1.6522878 -3.674214 9.991893e-04
# wt              -3.205613  0.7538957 -4.252065 2.130435e-04

# solution = -6.071


## Question 2:
# fit a model with mpg with cylinder with weight in and out of the model
# what can be said about the effect comparing 8 cylinders to 4?

# solution: fit two models and compare the results
fit.with <- lm(mpg ~ as.factor(cyl) + wt, data = mtcars)
fit.witho <- lm(mpg ~ as.factor(cyl), data = mtcars)

summary(fit.with)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)     33.990794  1.8877934 18.005569 6.257246e-17
# as.factor(cyl)6 -4.255582  1.3860728 -3.070244 4.717834e-03
# as.factor(cyl)8 -6.070860  1.6522878 -3.674214 9.991893e-04
# wt              -3.205613  0.7538957 -4.252065 2.130435e-04

summary(fit.witho)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)      26.663636  0.9718008 27.437347 2.688358e-22
# as.factor(cyl)6  -6.920779  1.5583482 -4.441099 1.194696e-04
# as.factor(cyl)8 -11.563636  1.2986235 -8.904534 8.568209e-10

# holding weight constant - cylinders have a less of an impact on mpg (see model 1)
# without weight the impact of cylinders on mpg is large (check the coefficients in both models)


## Question 3:
# fit a model with mpg vs. factor cylinders and weight
# then fit a model including interaction terms between cylinders and weight
# give the p value for the likelihood ratio test comparing the two models (@ 0.05)

# solution: fit models and compare p values between the two using lr test function in R
mod1 <- lm(mpg ~ as.factor(cyl) + wt, data = mtcars)
modi <- lm(mpg ~ as.factor(cyl) + wt + as.factor(cyl)*wt, data = mtcars)

summary(mod1)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)     33.990794  1.8877934 18.005569 6.257246e-17
# as.factor(cyl)6 -4.255582  1.3860728 -3.070244 4.717834e-03
# as.factor(cyl)8 -6.070860  1.6522878 -3.674214 9.991893e-04
# wt              -3.205613  0.7538957 -4.252065 2.130435e-04

summary(modi)$coef
# Estimate Std. Error    t value     Pr(>|t|)
# (Intercept)         39.571196   3.193940 12.3894599 2.058359e-12
# as.factor(cyl)6    -11.162351   9.355346 -1.1931522 2.435843e-01
# as.factor(cyl)8    -15.703167   4.839464 -3.2448150 3.223216e-03
# wt                  -5.647025   1.359498 -4.1537586 3.127578e-04
# as.factor(cyl)6:wt   2.866919   3.117330  0.9196716 3.661987e-01
# as.factor(cyl)8:wt   3.454587   1.627261  2.1229458 4.344037e-02

# run the lr test function in R between the two models
library("lmtest")
lrtest(mod1, modi)

# Likelihood ratio test
# Model 1: mpg ~ as.factor(cyl) + wt
# Model 2: mpg ~ as.factor(cyl) + wt + as.factor(cyl) * wt
#    #Df  LogLik Df  Chisq Pr(>Chisq)  
# 1   5 -73.311                       
# 2   7 -70.741  2 5.1412    0.07649 .
# ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# based on the .05 p value criteria - we would actually reject that the interactio model added informatoin
# .07649 vs. .05 criteria = interaction model may not be needed



## Question 4:
# fit a model mpg on cylinders and weight included as a shifted weight variable: see code below
# how if the weight coefficient in this model with a scaled weight x value?
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# Call:
#         lm(formula = mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# 
# Coefficients:
#         (Intercept)   I(wt * 0.5)  factor(cyl)6  factor(cyl)8  
# 33.991        -6.411        -4.256        -6.071

# solution: the weight variable has shifted but the coefficient value is still interpreted in the same way
# weight is now the estimated change in MPG per ONE TON increase in weight for A SPECIFIC NUMBER OF CYLINDERS!!
# NOTE: original weight x is in 2 TONS - multiplying by .5 gives us 1 TON



## Question 5:
# consider the following dataset: give the hat diagnol of the most influential point
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

# solution: regress the data and use hatvalues to give the hat value of the most influential point
fit <- lm(y ~ x)
summary(fit)$coef
# Estimate Std. Error   t value   Pr(>|t|)
# (Intercept) -0.1067207  0.2354337 -0.453294 0.68111587
# x            0.1288595  0.0447965  2.876552 0.06370816

# use hatvalues to find the highest hat value for most influential point
hatvalues(fit)
# 1         2         3         4         5 
# 0.2286650 0.2438146 0.2525027 0.2804443 0.9945734

# highest hat value = value with most influence is observation 5 i.e 11.72 !!!


## Question 6:
# consider the following dataset: give the slope of dfbeta for the point with highest hat value
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

# solution: fit model, calculate hatvalue, then calculate dfbetas value
fit <- lm(y ~ x)
summary(fit)$coef
# Estimate Std. Error   t value   Pr(>|t|)
# (Intercept) -0.1067207  0.2354337 -0.453294 0.68111587
# x            0.1288595  0.0447965  2.876552 0.06370816

# hatvalues
hatvalues(fit)
# 1         2         3         4         5 
# 0.2286650 0.2438146 0.2525027 0.2804443 0.9945734 

# dfbetas values
dfbetas(fit)
# (Intercept)             x
# 1  1.06212391   -0.37811633
# 2  0.06748037   -0.02861769
# 3 -0.01735756    0.00791512
# 4 -1.24958248    0.67253246
# 5  0.20432010 -133.82261293

# the 5th observation (11.72) has the highest hatvalue, and a dfbetas value of -133
# check out influence measures function!!
influence.measures(fit)
# Influence measures of
# lm(formula = y ~ x) :
#         
#         dfb.1_     dfb.x     dffit cov.r   cook.d   hat inf
# 1  1.0621 -3.78e-01    1.0679 0.341 2.93e-01 0.229   *
# 2  0.0675 -2.86e-02    0.0675 2.934 3.39e-03 0.244    
# 3 -0.0174  7.92e-03   -0.0174 3.007 2.26e-04 0.253   *
# 4 -1.2496  6.73e-01   -1.2557 0.342 3.91e-01 0.280   *
# 5  0.2043 -1.34e+02 -149.7204 0.107 2.70e+02 0.995   *



## Question 7:
# consider a regression between y and x with and without adjustment for z
# which is true about comparing the coefficient between Y and X with / without adjustment for z?

# For the the coefficient to change sign, there must be a significant interaction term.

# The coefficient can't change sign after adjustment, 
# except for slight numerical pathological cases.

# It is possible for the coefficient to reverse sign after adjustment. 
# For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment.

# Adjusting for another variable can only attenuate the coefficient toward zero.
# It can't materially change sign.


## solution: obviously adjustment can change the sign of the variables




## swirl practice
# Selection: 9
# 
# | Attempting to load lesson dependencies...
# 
# | Package ‘datasets’ loaded correctly!
#         
#         |                                                           |   0%
# 
# | Residuals, Diagnostics, and Variation. (Slides for this and other
#                                           | Data Science courses may be found at github
#                                           | https://github.com/DataScienceSpecialization/courses. If you care
#                                           | to use them, they must be downloaded as a zip file and viewed
#                                           | locally. This lesson corresponds to
#                                           | Regression_Models/02_04_residuals_variation_diagnostics.)
# 
# ...
# 
# |==                                                         |   3%
# | In the accompanying figure there is a fairly obvious outlier.
# | However obvious, it does not affect the fit very much as can be
# | seen by comparing the orange line with the black. The orange line
# | represents a fit in which the outlier is included in the data set,
# | and the black line represents a fit in which the outlier is
# | excluded. Including this outlier does not change the fit very
# | much, so it is said to lack influence.
# 
# ...
# 
# |====                                                       |   6%
# | This next figure also has a fairly obvious outlier, but in this
# | case including the outlier changes the fit a great deal. The slope
# | and the residuals of the orange line are very different than those
# | of the black line. This outlier is said to be influential.
# 
# ...
# 
# |=====                                                      |   9%
# | Outliers may or may not belong in the data. They may represent
# | real events or they may be spurious. In any case, they should be
# | examined. In order to spot them, R provides various diagnostic
# | plots and measures of influence. In this lesson we'll illustrate
# | their meanings and use. The basic technique is to examine the
# | effects of leaving one sample out, as we did in comparing the
# | black and orange lines above. We'll use the influential outlier to
# | illustrate, since leaving it out has clear effects.
# 
# ...
# 
# |=======                                                    |  12%
# | The influential outlier is in a data frame named out2. It has two
# | columns, labeled y and x, respectively. To begin, create a model
# | named fit using fit <- lm(y ~ x, out2) or an equivalent
# | expression.
# 
# > fit <- lm(y ~ x, out2)
# 
# | You are doing so well!
#         
#         |=========                                                  |  15%
# | The simplest diagnostic plot displays residuals versus fitted
# | values. Residuals should be uncorrelated with the fit, independent
# | and (almost) identically distributed with mean zero. Enter
# | plot(fit, which=1) at the R prompt to see if this is the case.
# 
# > plot(fit, which =1)
# 
# | That's a job well done!
# 
# |===========                                                |  18%
# | Do the residuals appear uncorrelated with the fit?
# 
# 1: Yes
# 2: No. There is a linear pattern involving all but one residual and the fit.
# 
# Selection: 2
# 
# | You got it right!
# 
# |=============                                              |  21%
# | The Residuals vs Fitted plot labels certain points with their row
# | names or numbers, numbers in our case. Which of the three labeled
# | points would you guess is our influential outlier?
# 
# 1: 13
# 2: 50
# 3: 1
# 
# Selection: 3
# 
# | Excellent job!
# 
# |==============                                             |  24%
# | Our influential outlier is in row 1 of the data. To exclude it is
# | just a matter using out2[-1, ] rather than out2 as data. Create a
# | second model, named fitno for 'fit with no outlier', which
# | excludes the outlier.
# 
# > fit <- lm(y ~ x, out2[-1,])
# 
# | You seem to have misspelled the model's name. I was expecting
# | fitno but you apparently typed fit.
# 
# | Almost! Try again. Or, type info() for more options.
# 
# | Enter fitno <- lm(y ~ x, out2[-1, ]) or something equivalent at
# | the R prompt.
# 
# > fitno <- lm(y ~ x, out2[-1, ])
# 
# | You're the best!
# 
# |================                                           |  27%
# | Display a Residuals vs Fitted plot for fitno. Remember to use
# | which=1.
# 
# > plot(fitno,which =1)
# 
# | That's the answer I was looking for.
# 
# |==================                                         |  30%
# | This plot has none of the patterned appearance of the first. It
# | looks as we would expect if residuals were independently and
# | (almost) identically distributed with zero mean, and were
# | uncorrelated with the fit.
# 
# ...
# 
# |====================                                       |  33%
# | The change which inclusion or exclusion of a sample induces in
# | coefficents is a simple measure of its influence. Subtract
# | coef(fitno) from coef(fit) to see the change induced by including
# | the influential first sample.
# 
# > coef(fitno) - coef(fit)
# (Intercept)           x 
# 0.01167866  0.53363019 
# 
# | Not quite, but you're learning! Try again. Or, type info() for
# | more options.
# 
# | Just enter coef(fit)-coef(fitno) at the R prompt.
# 
# > coef(fit)-coef(fitno)
# (Intercept)           x 
# -0.01167866 -0.53363019 
# 
# | You are doing so well!
# 
# |=====================                                      |  36%
# | dfbeta: The function, dfbeta, does the equivalent calculation for
# | every sample in the data. The first row of dfbeta(fit) should
# | match the difference we've just calculated. The second row is a
# | similar calculation for the second sample, and so on. Since dfbeta
# | returns a large matrix, use either head(dfbeta(fit)) or
# | View(dfbeta(fit)) to examine the result.
# 
# > head(dfbeta(fit))
# (Intercept)             x
# 1 -0.011678662 -0.5336301857
# 2  0.008636967  0.0045759242
# 3  0.010323864 -0.0003509441
# 4  0.003122096 -0.0033664451
# 5  0.001975966 -0.0008297575
# 6  0.002230518 -0.0005867041
# 
# | Excellent work!
#         
#         |=======================                                    |  39%
# | Comparing the first row with those below it, we see that the first
# | sample has a much larger effect on the slope (the x column) than
# | other samples. In fact, the magnitude of its effect is about 100
# | times that of any other point. Its effect on the intercept is not
# | very distinctive essentially because its y coordinate is 0, the
# | mean of the other samples.
# 
# ...
# 
# |=========================                                  |  42%
# | When a sample is included in a model, it pulls the regression line
# | closer to itself (orange line) than that of the model which
# | excludes it (black line.) Its residual, the difference between its
# | actual y value and that of a regression line, is thus smaller in
# | magnitude when it is included (orange dots) than when it is
# | omitted (black dots.) The ratio of these two residuals, orange to
# | black, is therefore small in magnitude for an influential sample.
# | For a sample which is not influential the ratio would be close to
# | 1. Hence, 1 minus the ratio is a measure of influence, near 0 for
# | points which are not influential, and near 1 for points which are.
# 
# ...
# 
# |===========================                                |  45%
# | This measure is sometimes called influence, sometimes leverage,
# | and sometimes hat value. Since it is 1 minus the ratio of two
# | residuals, to calculate it from scratch we must first obtain the
# | two residuals. The ratio's numerator (orange dots) is the residual
# | of the first sample of the model we called fit. The model fitno,
# | which excludes this sample, also excludes its residual, so we will
# | have to calculate its value. This is easily done. We use R's
# | predict function to calculate fitno's predicted value of y and
# | subtract it from the actual value. Use the expression resno <-
# | out2[1, "y"] - predict(fitno, out2[1,]) to do the calculation.
# 
# > resno <-
# +     | out2[1, "y"] - predict(fitno, out2[1,])
# Error: unexpected '|' in:
# "resno <-
# |"
# > resno <-out2[1, "y"] - predict(fitno, out2[1,])
# 
# | Perseverance, that's the answer.
# 
# |=============================                              |  48%
# | Now calculate the influence of our outlier using
# | 1-resid(fit)[1]/resno or an equivalent expression.
# 
# > 1-resid(fit)[1]/resno
# 1 
# 0.6311547 
# 
# | Your dedication is inspiring!
#         
#         |==============================                             |  52%
# | hatvalues: The function, hatvalues, performs for every sample a
# | calculation equivalent to the one you've just done. Thus the first
# | entry of hatvalues(fit) should match the value which you have just
# | calculated. Since there are quite a few samples, use
# | head(hatvalues(fit)) or View(hatvalues(fit)) to compare the
# | influence measure of our outlier to that of some other samples.
# 
# > head(hatvalues(fit))
# 1          2          3          4          5          6 
# 0.63115474 0.02324999 0.01962520 0.04326099 0.02255531 0.02071441 
# 
# | All that hard work is paying off!
# 
# |================================                           |  55%
# | Residuals of individual samples are sometimes treated as having
# | the same variance, which is estimated as the sample variance of
# | the entire set of residuals. Theoretically, however, residuals of
# | individual samples have different variances and these differences
# | can become large in the presence of outliers. Standardized and
# | Studentized residuals attempt to compensate for this effect in two
# | slightly different ways. Both use hat values.
# 
# ...
# 
# |==================================                         |  58%
# | We'll consider standardized residuals first. To begin, calculate
# | the sample standard deviation of fit's residual by dividing fit's
# | deviance, i.e., its residual sum of squares, by the residual
# | degrees of freedom and taking the square root. Store the result in
# | a variable called sigma.
# 
# > simga
# Error: object 'simga' not found
# > sigma <- sqrt
# 
# | One more time. You can do it! Or, type info() for more options.
# 
# | Enter sigma <- sqrt(deviance(fit)/df.residual(fit)) or an
# | equivalent expression at the R prompt.
# 
# > sigma <- sqrt(deviance(fit)/df.residual(fit))
# 
# | Perseverance, that's the answer.
# 
# |====================================                       |  61%
# | Ordinarily we would just divide fit's residual (which has mean 0)
# | by sigma. In the present case we multiply sigma times
# | sqrt(1-hatvalues(fit)) to estimate standard deviations of
# | individual samples. Thus, instead of dividing resid(fit) by sigma,
# | we divide by sigma*sqrt(1-hatvalues(fit)). The result is called
# | the standardized residual. Compute fit's standardized residual and
# | store it in a variable named rstd.
# 
# > sigma*sqrt(1-hatvalues(fit))
# 1         2         3         4         5         6 
# 0.3015870 0.4907748 0.4916846 0.4857214 0.4909493 0.4914114 
# 7         8         9        10        11        12 
# 0.4915701 0.4883341 0.4905705 0.4910237 0.4899486 0.4915171 
# 13        14        15        16        17        18 
# 0.4905071 0.4913258 0.4868200 0.4913989 0.4913706 0.4905405 
# 19        20        21        22        23        24 
# 0.4776628 0.4890140 0.4915267 0.4896276 0.4895598 0.4899490 
# 25        26        27        28        29        30 
# 0.4876623 0.4916776 0.4896277 0.4908021 0.4911304 0.4883057 
# 31        32        33        34        35        36 
# 0.4909860 0.4916833 0.4911820 0.4860069 0.4916512 0.4879225 
# 37        38        39        40        41        42 
# 0.4860309 0.4892811 0.4911778 0.4886486 0.4910759 0.4852949 
# 43        44        45        46        47        48 
# 0.4916861 0.4916094 0.4916631 0.4910861 0.4916007 0.4906225 
# 49        50        51 
# 0.4902028 0.4837825 0.4912899 
# 
# | You're close...I can feel it! Try it again. Or, type info() for
# | more options.
# 
# | Enter rstd <- resid(fit)/(sigma * sqrt(1-hatvalues(fit))) or an
# | equivalent expression at the R prompt.
# 
# > rstd <- resid(fit)/(sigma * sqrt(1-hatvalues(fit)))
# 
# | You are doing so well!
#         
#         |======================================                     |  64%
# | rstandard: The function, rstandard, computes the standardized
# | residual which we have just computed step by step. Use
# | head(cbind(rstd, rstandard(fit))) or View(cbind(rstd,
#                                                   | rstandard(fit))) to compare the two calculations.
# 
# > head(cbind(rstd, rstandard(fit)))
# rstd           
# 1 -5.1928156 -5.1928156
# 2  0.9389601  0.9389601
# 3  1.0450409  1.0450409
# 4  0.2682743  0.2682743
# 5  0.1893339  0.1893339
# 6  0.2186961  0.2186961
# 
# | You are really on a roll!
#         
#         |=======================================                    |  67%
# | A Scale-Location plot shows the square root of standardized
# | residuals against fitted values. Use plot(fit, which=3) to display
# | it.
# 
# > plot(fit, which = 3)
# 
# | You got it right!
#         
#         |=========================================                  |  70%
# | Most of the diagnostic statistics under discussion were developed
# | because of perceived shortcomings of other diagnostics and because
# | their distributions under a null hypothesis could be
# | characterized. The assumption that residuals are approximately
# | normal is implicit in such characterizations. Since standardized
# | residuals adjust for individual residual variances, a QQ plot of
# | standardized residuals against normal with constant variance is of
# | interest. Use plot(fit, which=2) to display this diagnostic plot.
# 
# > plot(fit, which = 2)
# 
# | Perseverance, that's the answer.
# 
# |===========================================                |  73%
# | Look at the outlier's standardized residual, labeled 1 on the
# | Normal QQ plot. About how many standard deviations from the mean
# | is it?
#         
#         1: About -5
# 2: About -2
# 
# Selection: 2
# 
# | Not quite! Try again.
# 
# | This would be its position on the vertical axis.
# 
# 1: About -2
# 2: About -5
# 
# Selection: 2
# 
# | All that practice is paying off!
#         
#         |=============================================              |  76%
# | Studentized residuals, (sometimes called externally Studentized
#                           | residuals,) estimate the standard deviations of individual
# | residuals using, in addition to individual hat values, the
# | deviance of a model which leaves the associated sample out. We'll
# | illustrate using the outlier. Recalling that the model we called
# | fitno omits the outlier sample, calculate the sample standard
# | deviation of fitno's residual by dividing its deviance, by its
# | residual degrees of freedom and taking the square root. Store the
# | result in a variable called sigma1.
# 
# > rstd <- resid(fit)/(sigma * sqrt(1-hatvalues(fit)))
# 
# | Not quite, but you're learning! Try again. Or, type info() for
# | more options.
# 
# | Enter sigma1 <- sqrt(deviance(fitno)/df.residual(fitno)) or an
# | equivalent expression at the R prompt.
# 
# > sigma1 <- sqrt(deviance(fitno)/df.residual(fitno))
# 
# | Great job!
# 
# |==============================================             |  79%
# | Calculate the Studentized residual for the outlier sample by
# | dividing resid(fit)[1] by the product of sigma1 and
# | sqrt(1-hatvalues(fit)[1]). There is no need to store this in a
# | variable.
# 
# > resid(fit)[1] / (sigma1*sqrt(1-hatvalues(fit)[1]))
# 1 
# -7.664261 
# 
# | That's correct!
#         
#         |================================================           |  82%
# | rstudent: The function, rstudent, calculates Studentized residuals
# | for each sample using a procedure equivalent to that which we just
# | used for the outlier. Thus rstudent(fit)[1] should match the value
# | we calculated in the previous question. Use head(rstudent(fit)) or
# | View(rstudent(fit)) to verify this and to compare the Studentized
# | residual of the outlier with those of other samples.
# 
# > head(rstudent(fit))
# 1          2          3          4          5          6 
# -7.6642608  0.9378046  1.0460451  0.2657179  0.1874606  0.2165588 
# 
# | You got it right!
#         
#         |==================================================         |  85%
# | Cook's distance is the last influence measure we will consider. It
# | is essentially the sum of squared differences between values
# | fitted with and without a particular sample. It is normalized
# | (divided by) residual sample variance times the number of
# | predictors which is 2 in our case (the intercept and x.) It
# | essentially tells how much a given sample changes a model. We'll
# | illustrate once again by calculating Cook's distance for the
# | outlier.
# 
# ...
# 
# |====================================================       |  88%
# | We'll begin by calculating the difference in predicted values
# | between fit and fitno, the models which respectively include and
# | omit the outlier. This is most easily done by subtracting
# | predict(fit, out2) from predict(fitno, out2). Store the difference
# | in a variable named dy.
# 
# > predict(fitno, out2) - predict(fit, out2) 
# 1            2            3            4            5 
# 2.679829590  0.283638110  0.069419418 -0.427405401 -0.097011327 
# 6            7            8            9           10 
# -0.027198916  0.155533433  0.466649359 -0.138366375 -0.087713317 
# 11           12           13           14           15 
# -0.193118350 -0.003654442  0.311064759  0.209578057 -0.378270935 
# 16           17           18           19           20 
# 0.196167237 -0.035036318  0.307822702 -0.696406846 -0.259216332 
# 21           22           23           24           25 
# 0.167713315 -0.217474574  0.388877954 -0.193083754 -0.336635794 
# 26           27           28           29           30 
# 0.105632034  0.383972292  0.280620422  0.239921104  0.468263041 
# 31           32           33           34           35 
# -0.092486603  0.098944865 -0.066000665 -0.415112689  0.124016894 
# 36           37           38           39           40 
# 0.489405767  0.580571476 -0.241710530 -0.066614597 -0.281788590 
# 41           42           43           44           45 
# 0.247378594 -0.445224294  0.094487758  0.024112700  0.049564298 
# 46           47           48           49           50 
# -0.079503882  0.020949454 -0.133164616 -0.172166314  0.670464336 
# 51 
# 0.215681812 
# 
# | That's not exactly what I'm looking for. Try again. Or, type
# | info() for more options.
# 
# | Enter dy <- predict(fitno, out2)-predict(fit, out2) or an
# | equivalent expression at the R prompt.
# 
# > dy <- predict(fitno, out2)-predict(fit, out2)
# 
# | Excellent work!
#         
#         |======================================================     |  91%
# | Recall that we calculated the sample standard deviation of fit's
# | residual, sigma, earlier. Divide the summed squares of dy by
# | 2*sigma^2 to calculate the outlier's Cook's distance. There is no
# | need to store the result in a variable.
# 
# > dy <- predict(fitno, out2)-predict(fit, out2)
# 
# | Keep trying! Or, type info() for more options.
# 
# | Enter sum(dy^2)/(2*sigma^2) or an equivalent expression at the R
# | prompt.
# 
# > sum(dy^2)/(2*sigma^2)
# [1] 23.07105
# 
# | All that practice is paying off!
# 
# |=======================================================    |  94%
# | cooks.distance: The function, cooks.distance, will calculate
# | Cook's distance for each sample. Rather than verify that
# | cooks.distance(fit)[1] is equal to the value just calculated,
# | because that sort of thing must be getting tedious by now, display
# | a diagnostic plot which uses Cook's distance using plot(fit,
# | which=5).
# 
# > plot(fit, which = 5)
# 
# | That's correct!
#         
#         |=========================================================  |  97%
# | That concludes swirl's coverage of Residuals, Diagnostics, and
# | Variation. The HTML5 slides for this as well as other units in the
# | Johns Hopkins Data Science Specialization can be found here:
# | https://github.com/DataScienceSpecialization/courses. They must be
# | downloaded and viewed locally.
# 
# ...



## DATA ANALYSIS PRACTICE QUIZ






## Linear Regression Week 4: Logistic Regression and Poisson Regression
# linear regression is not without its set of limitations
# what is the response is strictly positive? discrete?
# transformations are hard to interpret - there is value modeling on the repsonse the "way it came in"
# generalized linear models is a family of model that includes linear models
# by extending the family - we can handle many of the issues with simple linear models
# with these models we gain more complexity and lose some interpretation
# GLM involves three components:
        # an exponential family model for the response
        # a systematic component via a linear predictor
        # a link function that connects the means of the response to the linear predictor
# the most common GLMs: linear, binomial, binary regression, Poisson regression

# switching out of linear additive response models and move into GLMs
# we will get into more "messy" mathmatical procedures
# linear model still is the single most applied statistical learning method
# linear model requires "additive" responses!
# transformations are hard to interpret - natural log may be the easiest transformation - arc-sin-sqrt not so much
# strictly positive values are hard for linear models
# negative and zero values are also hard for linear models
# nice to have a model on the scale of the response that is how it was presented
# natural log does not work for negative or zero values

# Generalized Linear Models - Nelder and Wesserman
# an exponential family model for the response - random component
# a systematic component via a linear predictor - the systematic error we want to model
# we need to connect the random and the systematic error - connect the means of the response to the linear predictor

# Example: Linear Models are GLMs
# assume Y ~ normal with normal distribution (normal distribution is an expoential family distribution!)
# define the linear predictor Bi is the collection of xs times the coefficients
# the link function is the IDENTITY function - Yi is equal to the coefficients + the error

# Example: Logistic Regression
# we assume that Y is binary or following the Bernoulli distribution
# Y the response can only be 0 or 1!!!
# we have a probability of getting a 1 or 0
# the linear predictor is still the same - xs times the coefficients
# the log odds - logistic link function - is the link function - WE TAKE THE LOG of THE ODDS to connect Y to the predictors x * coefficients
# we transform the mean of our distribution not the Y values itself!!!
# we can transform the distribution of the Y to have it relate to the linear predictor!!
# you can also go "backwards" by converting from log odds back to the normal mean
# using the link function we have a liklihood function that we wish to optimize to obatin our paramenter estimates!!

# Poisson Regression
# we assume that Y follows a Poisson distribution!
# all Y responses must be larger than zero!!
# best for unbounded counts !
# the linear predictor is the same - covariates times thier coefficients!!
# the link function is the log link - we log the mean for the data that Y comes from!!
# we can go backwards from the log link back to the mean!
# this gives us another optimization function that we want to maximize to get us our coefficients!

# our maximization function are similiar to the least squares idea
# GLM try to solve the liklihood function - a function we want to solve for zero!
# at 0 we maximize our optimization function!!

# GLM Variances: poisson and binomial
# linear models need a constant variance...
# bernoulli - the variance depends on which observation - variance is not constant across all is
# poisson - the variance also depends on i - not constant like a linear model
# the modeling assumptions often but a restriction on the mean and variance
# you need to check this if you are trying to model using GLMs!!
# quasi - models relax the mean - variance relationships requirements - slightly more flexible variance model in case we don't strictly adhere to the strict variance structure
# quasi models are standard in all languages
# GLMs HAVE TO BE OPTIMIZED - not minimized as in linear regression
# predicted repsonse is the same - take our Xs and multiply by the coefficients!
# you will need to convert to and from the "transformed" glm model "by the link function"
# coefficents are interpreted the same - EXPECTED CHANGE IN RESPONSE BASED ON CHANGE IN X HOLDING ALL OTHER VARIABLES CONSTANT
# THE COEFFICIENTS ARE ALSO ON THE SCALE OF OR GLM LINEAR PREDICTOR!!
# we lose some of the inference...no longer have t values .. but we can compare correct distributions to get "back" to p values
# all results are based require a large sample size to convert new coefficient distriution to a t distribution or p value


## Logistic Regression for Binary Outcomes
# binary GLMs are trying to model outcomes that can only take two values
# coin flip, winning or losing, etc. 
# THESE WIN / LOSS OUTCOMES ARE OFTEN CALLED BERNOULLI DISTRIBUTIONS!
# if we happen to have serveral exchangeable binary outcomes for the same level of covariate we can aggregate into 1's and 0s
# BINOMIAL: for each covariates we can count how many wins and losses are assigned to that variable
# different levels of wins and losses across covariates lead us to model in the binomial distribution

# response is only 0 or 1
# survival analysis, win/loss, success / failures
# model the data like a set of "coin flips"
# binary and binomial will be covered below
# example: RAVENS FOOTBALL DATA

# load the data
load("~/Downloads/ravensData.rda")

# does ravens score predict wins?
# we have wins coded as 1 and 0!
head(ravensData)
# ravenWinNum ravenWin ravenScore opponentScore
# 1           1        W         24             9
# 2           1        W         38            35
# 3           1        W         28            13
# 4           1        W         34            31
# 5           1        W         44            13
# 6           0        L         23            24

# no really a linear regression problem...
# we assume errors are normal...with 1 and 0 data we will not have constant error terms
# constant variance of the error term is a requirement of linear regression...
# in this case - out variance will depend on each i = non-constant variance

# linear regression check...
# look at how fucking shitty this fit is...
# only explains .14 percent of the total variance...
# maybe we need a better approach to modeling this data...
# you can get estimates above or below 1 or 0...we want to model the odds!!!!!
lmRaven <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
# Call:
#         lm(formula = ravensData$ravenWinNum ~ ravensData$ravenScore)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
# -0.7302 -0.5076  0.1824  0.3215  0.5719 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)  
# (Intercept)           0.285032   0.256643   1.111   0.2814  
# ravensData$ravenScore 0.015899   0.009059   1.755   0.0963 .
# ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4464 on 18 degrees of freedom
# Multiple R-squared:  0.1461,	Adjusted R-squared:  0.09868 
# F-statistic:  3.08 on 1 and 18 DF,  p-value: 0.09625

# view coefficents
summary(lmRaven)$coef
#                       Estimate  Std. Error  t value   Pr(>|t|)
# (Intercept)           0.28503172 0.256643165 1.110615 0.28135043
# ravensData$ravenScore 0.01589917 0.009058997 1.755069 0.09625261


# modeling the odds - we want to model the probability the RAVENS WIN!
# our probability game to game will DIFFER depending on the RAVENS SCORE!! 
# this probability is what we want to model!!
# you can go forward and backawards from probability to odds
# the log of the odds is the LOGIT - WE PUT THE MODEL ON THE LOG OF THE ODDS!!!
# we relate our coin flip probability to the regressors by using the log of the odds!!
# this allows us to convert our bernoulii back into a linear model
# we are not scaling the the repsonse or parameters - we scale the mean to a probability distribution!

# interpreting the logistic regression:
# b0 = log odds of a ravens win if they score zero points!
# b1 = log odds ratio of win probability for each point scored (compare to zero points)
# exp(b1) = odds ratio of win probability for each point scored (compared to zero points)
# when we exp our probability we go back to the probability scale and we get a ratio of the odds of winning!
# the extension to multiple logistic regression - all other regressors are held fixed! same log odds and then ratio of probability with with exp the results

# history of odds
# how do we set up a fair flip game to be fair?
# we can calculate expected earnings
# E[earnings] = Xp - Y(1-p) = 0
# amount you win if you win the flip - the amount you lose if you lose the flip = solve this equation to 0
# this implies = Y/X = p / 1 - p
# odds can be: "How much should you be willing to pay for a p probability of winning a dollar?"
# if p > .05 you have to pay more if oyu lose than you get if you win
# if p < .5 you have to pay less if you lose than you get if you win
# the house always wins - they set up the odds to "win" no matter who wins!

# how to visualize fitting logistic regression curves
library(manipulate)

# what is R doing when we are fitting logistic regressions?
# we only have data with 1 and 0
# as we go towards large x values = more likely to "win"
# R will try a bunch on S curves that best matches up with different probabilities
# it does this by the maximum likelihood!!
# we manipulate B0 and B1 to fin the best curve that fits our 1 and 0 data the best!
# the final curve is our regression function: e^b0 + B1X / 1 + e^B0 + B1X
x <- seq(-10, 10, length = 1000)
manipulate(
        plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)), 
             type = "l", lwd = 3, frame = FALSE),
        beta1 = slider(-2, 2, step = .1, initial = 2),
        beta0 = slider(-2, 2, step = .1, initial = 0)
)




# fitting the ravens data with a glm function = LOGTISTIC REGRESSION
# using family binomial will assume the correct link function - logit link function
# on the logit scale we want to be as close to 0 are possible
# when we scale it to the odds - we want to as close to one as possible
logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)
# Call:
#         glm(formula = ravensData$ravenWinNum ~ ravensData$ravenScore, 
#             family = "binomial")
# 
# Deviance Residuals: 
#         Min       1Q   Median       3Q      Max  
# -1.7575  -1.0999   0.5305   0.8060   1.4947  
# 
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)
# (Intercept)           -1.68001    1.55412  -1.081     0.28
# ravensData$ravenScore  0.10658    0.06674   1.597     0.11
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 24.435  on 19  degrees of freedom
# Residual deviance: 20.895  on 18  degrees of freedom
# AIC: 24.895
# 
# Number of Fisher Scoring iterations: 5

# here is the fitted line - scaled back to probability odds!
# this is the "fitted" S Curve based on the log odds porbability
plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",
     xlab="Score",ylab="Prob Ravens Win")

# convert log odds back to probabilities
# suggests an 11% increase for every 1 point increase in Ravens score
exp(logRegRavens$coeff)
# (Intercept) ravensData$ravenScore 
# 0.1863724             1.1124694 

# confidence intervals for the expontential form (converted back)
# confidence interval for the coefficient values
# how every this is not sufficient - point scored is not significant
exp(confint(logRegRavens))
# Waiting for profiling to be done...
#                               2.5 %   97.5 %
#         (Intercept)           0.005674966 3.106384
# ravensData$ravenScore         0.996229662 1.303304

# anova of logistic regression
# plot model with score and without score...
# is the addition of score significant?
anova(logRegRavens,test="Chisq")
# Analysis of Deviance Table
# 
# Model: binomial, link: logit
# 
# Response: ravensData$ravenWinNum
# 
# Terms added sequentially (first to last)
# 
# 
# Df Deviance Resid. Df Resid. Dev
# NULL                                     19     24.435
# ravensData$ravenScore  1   3.5398        18     20.895
# Pr(>Chi)  
# NULL                            
# ravensData$ravenScore  0.05991 .
# ---
#         Signif. codes:  
#         0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



## interpretting odds ratios
# odds ratios are not probabilities!!
# odds ratios of 1 = no difference in odds
# log odds ratio of 0 = no difference in odds = LOGIT!!
# odds ration of < .5 or >2 = commonly a "moderate effect"
# for small probabilities RR is approximately equal to OR but THEY ARE NOT THE SAME!!
# Relative Risk approximates the Odds Ratios but they are not the same!!
# use knowledge from linear regression to bring us into Generalized Linear Models!!
        # a repsonse with a unique distribution
        # set of predictors
        # A LINK FUNCTION (LOGIT etc.) TO RELATE THE REPSONSE BACK TO THE PREDICTORS!!




## swirl practice
# Selection: 10
# 
# | Attempting to load lesson dependencies...
# 
# | This lesson requires the ‘car’ package. Would you like me
# | to install it for you now?
#         
#         1: Yes
# 2: No
# 
# Selection: 1
# 
# | Trying to install package ‘car’ now...
# also installing the dependencies ‘minqa’, ‘nloptr’, ‘RcppEigen’, ‘lme4’, ‘SparseM’, ‘MatrixModels’, ‘pbkrtest’, ‘quantreg’
# 
# 
# | Package ‘car’ loaded correctly!
#         
#         |                                                                                                  |   0%
# 
# | Variance Inflation Factors. (Slides for this and other Data Science courses may be found at github
#                                | https://github.com/DataScienceSpecialization/courses. If you care to use them, they must be downloaded as
#                                | a zip file and viewed locally. This lesson corresponds to
#                                | Regression_Models/02_04_residuals_variation_diagnostics.)
# 
# ...
# 
# |====                                                                                              |   4%
# | In modeling, our interest lies in parsimonious, interpretable representations of the data that enhance
# | our understanding of the phenomena under study. Omitting variables results in bias in the coefficients of
# | interest - unless their regressors are uncorrelated with the omitted ones. On the other hand, including
# | any new variables increases (actual, not estimated) standard errors of other regressors. So we don't want
# | to idly throw variables into the model. This lesson is about the second of these two issues, which is
# | known as variance inflation.
# 
# ...
# 
# |========                                                                                          |   8%
# | We shall use simulations to illustrate variance inflation. The source code for these simulations is in a
# | file named vifSims.R which I have copied into your working directory and tried to display in your source
# | code editor. If I've failed to display it, you should open it manually.
# 
# ...
# 
# |============                                                                                      |  12%
# | Find the function, makelms, at the top of vifSims.R. The final expression in makelms creates 3 linear
# | models. The first, lm(y ~ x1), predicts y in terms of x1, the second predicts y in terms of x1 and x2,
# | the third in terms of all three regressors. The second coefficient of each model, for instance coef(lm(y
#                                                                                                          | ~ x1))[2], is extracted and returned in a 3-long vector. What does this second coefficient represent?
#         
#         1: The coefficient of the intercept.
# 2: The coefficient of x1.
# 3: The coefficient of x2.
# 
# Selection: 2
# 
# | You're the best!
# 
# |================                                                                                  |  17%
# | In makelms, the simulated dependent variable, y, depends on which of the regressors?
# 
# 1: x1 and x2
# 2: x1, x2, and x3
# 3: x1
# 
# Selection: 2
# 
# | Almost! Try again.
# 
# | The dependent variable, y, is formed by the expression, y <- x1 + rnorm(length(x1), sd = .3). Which of
# | the regressors, x1, x2, x3, appears in this expression?
# 
# 1: x1
# 2: x1 and x2
# 3: x1, x2, and x3
# 
# Selection: 1
# 
# | You got it right!
# 
# |====================                                                                              |  21%
# | In vifSims.R, find the functions, rgp1() and rgp2(). Both functions generate 3 regressors, x1, x2, and
# | x3. Compare the lines following the comment Point A in rgp1() with those following Point C in rgp2().
# | Which of the following statements about x1, x2, and x3 is true?
# 
# 1: x1, x2, and x3 are uncorrelated in both rgp1() and rgp2().
# 2: x1, x2, and x3 are correlated in both rgp1() and rgp2().
# 3: x1, x2, and x3 are correlated in rgp1(), but not in rgp2().
# 4: x1, x2, and x3 are uncorrelated in rgp1(), but not in rgp2().
# 
# Selection: 4
# 
# | Nice work!
# 
# |========================                                                                          |  25%
# | In the line following Point B in rgp1(), the function maklms(x1, x2, x3) is applied 1000 times. Each time
# | it is applied, it simulates a new dependent variable, y, and returns estimates of the coefficient of x1
# | for each of the 3 models, y ~ x1, y ~ x1 + x2, and y ~ x1 + x2 + x3. It thus computes 1000 estimates of
# | the 3 coefficients, collecting the results in 3x1000 array, beta. In the next line, the expression,
# | apply(betas, 1, var), does which of the following?
# 
# 1: Computes the variance of each row.
# 2: Computes the variance of each column.
# 
# Selection: 2
# 
# | Not quite, but you're learning! Try again.
# 
# | We hope to illustrate the effect of extra variables on the variance of x1's coefficient. For this purpose
# | we have 3 models, y ~ x1, y ~ x1 + x2, and y ~ x1 + x2 + x3. The three rows of beta correspond to the
# | three models. The columns correspond to the 1000 simulated situations in which we estimate the
# | coefficients of x1 for each of the three models. We are interested in the variance of the x1 coefficient
# | for each of those three models.
# 
# 1: Computes the variance of each row.
# 2: Computes the variance of each column.
# 
# Selection: 1
# 
# | You are quite good my friend!
# 
# |=============================                                                                     |  29%
# | The function rgp1() computes the variance in estimates of the coefficient of x1 in each of the three
# | models, y ~ x1, y ~ x1 + x2, and y ~ x1 + x2 + x3. (The results are rounded to 5 decimal places for
# | convenient viewing.) This simulation approximates the variance (i.e., squared standard error) of x1's
# | coefficient in each of these three models. Recall that variance inflation is due to correlated regressors
# | and that in rgp1() the regressors are uncorrelated. Run the simulation rgp1() now. Be patient. It takes a
# | while.
# 
# > # Regressor generation process 1.
#         > rgp1 <- function(){
#                 +   print("Processing. Please wait.")
#                 +   # number of samples per simulation
#                         +   n <- 100
#                         +   # number of simulations
#                                 +   nosim <- 1000
#                                 +   # set seed for reproducibility
#                                         +   set.seed(4321)
#                                 +   # Point A
#                                         +   x1 <- rnorm(n)
#                                         +   x2 <- rnorm(n)
#                                         +   x3 <- rnorm(n)
#                                         +   # Point B
#                                                 +   betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
#                                                 +   round(apply(betas, 1, var), 5)
#                                                 + }
# 
# | You almost had it, but not quite. Try again. Or, type info() for more options.
# 
# | Just enter rgp1() at the R prompt.
# 
# > rgp1
# function(){
#         print("Processing. Please wait.")
#         # number of samples per simulation
#         n <- 100
#         # number of simulations
#         nosim <- 1000
#         # set seed for reproducibility
#         set.seed(4321)
#         # Point A
#         x1 <- rnorm(n)
#         x2 <- rnorm(n)
#         x3 <- rnorm(n)
#         # Point B
#         betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
#         round(apply(betas, 1, var), 5)
# }
# 
# | That's not the answer I was looking for, but try again. Or, type info() for more options.
# 
# | Just enter rgp1() at the R prompt.
# 
# > rpg1()
# Error in rpg1() : could not find function "rpg1"
# > makelms <- function(x1, x2, x3){
# +   # Simulate a dependent variable, y, as x1
# +   # plus a normally distributed error of mean 0 and 
# +   # standard deviation .3.
# +   y <- x1 + rnorm(length(x1), sd = .3)
# +   # Find the coefficient of x1 in 3 nested linear
# +   # models, the first including only the predictor x1,
# +   # the second x1 and x2, the third x1, x2, and x3.
# +   c(coef(lm(y ~ x1))[2], 
# +     coef(lm(y ~ x1 + x2))[2], 
# +     coef(lm(y ~ x1 + x2 + x3))[2])
# + }
# 
# | Not quite, but you're learning! Try again. Or, type info() for more options.
# 
# | Just enter rgp1() at the R prompt.
# 
# > 
#         > # Regressor generation process 1.
#         > rgp1 <- function(){
#                 +   print("Processing. Please wait.")
#                 +   # number of samples per simulation
#                         +   n <- 100
#                         +   # number of simulations
#                                 +   nosim <- 1000
#                                 +   # set seed for reproducibility
#                                         +   set.seed(4321)
#                                 +   # Point A
#                                         +   x1 <- rnorm(n)
#                                         +   x2 <- rnorm(n)
#                                         +   x3 <- rnorm(n)
#                                         +   # Point B
#                                                 +   betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
#                                                 +   round(apply(betas, 1, var), 5)
#                                                 + }
# 
# | One more time. You can do it! Or, type info() for more options.
# 
# | Just enter rgp1() at the R prompt.
# 
# > 
#         > # Regressor generation process 2.
#         > rgp2 <- function(){
#                 +   print("Processing. Please wait.")
#                 +   # number of samples per simulation
#                         +   n <- 100
#                         +   # number of simulations
#                                 +   nosim <- 1000
#                                 +   # set seed for reproducibility
#                                         +   set.seed(4321)
#                                 +   # Point C
#                                         +   x1 <- rnorm(n)
#                                         +   x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
#                                         +   x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
#                                         +   # Point D
#                                                 +   betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
#                                                 +   round(apply(betas, 1, var), 5)
#                                                 + }
# 
# | Not quite! Try again. Or, type info() for more options.
# 
# | Just enter rgp1() at the R prompt.
# 
# > rpg1()
# Error in rpg1() : could not find function "rpg1"
# > rpg1
# Error: object 'rpg1' not found
# > gp1 <- function(){
#         +   print("Processing. Please wait.")
#         +   # number of samples per simulation
#                 +   n <- 100
#                 +   # number of simulations
#                         +   nosim <- 1000
#                         +   # set seed for reproducibility
#                                 +   set.seed(4321)
#                         +   # Point A
#                                 +   x1 <- rnorm(n)
#                                 +   x2 <- rnorm(n)
#                                 +   x3 <- rnorm(n)
#                                 +   # Point B
#                                         +   betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
#                                         +   round(apply(betas, 1, var), 5)
#                                         + }
# 
# | Give it another try. Or, type info() for more options.
# 
# | Just enter rgp1() at the R prompt.
# 
# > # Regressor generation process 1.
#         > rgp1 <- function(){
#                 +   print("Processing. Please wait.")
#                 +   # number of samples per simulation
#                         +   n <- 100
#                         +   # number of simulations
#                                 +   nosim <- 1000
#                                 +   # set seed for reproducibility
#                                         +   set.seed(4321)
#                                 +   # Point A
#                                         +   x1 <- rnorm(n)
#                                         +   x2 <- rnorm(n)
#                                         +   x3 <- rnorm(n)
#                                         +   # Point B
#                                                 +   betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
#                                                 +   round(apply(betas, 1, var), 5)
#                                                 + }
# 
# | That's not exactly what I'm looking for. Try again. Or, type info() for more options.
# 
# | Just enter rgp1() at the R prompt.
# 
# > rgp1()
# [1] "Processing. Please wait."
# x1      x1      x1 
# 0.00110 0.00111 0.00112 
# 
# | Nice work!
#         
#         |=================================                                                                 |  33%
# | The variances in each of the three models are approximately equal, as expected, since the other
# | regressors, x2 and x3, are uncorrelated with the regressor of interest, x1. However, in rgp2(), x2 and x3
# | both depend on x1, so we should expect an effect. From the expressions assigning x2 and x3 which follow
# | Point C, which is more strongly correlated with x1?
#         
#         1: x3
# 2: x2
# 
# Selection: 1
# 
# | That's correct!
# 
# |=====================================                                                             |  38%
# | Run rgp2() to simulate standard errors in the coefficient of x1 for cases in which x1 is correlated with
# | the other regressors
# 
# > rgp2()
# [1] "Processing. Please wait."
# x1      x1      x1 
# 0.00110 0.00240 0.00981 
# 
# | Nice work!
# 
# |=========================================                                                         |  42%
# | In this case, variance inflation due to correlated regressors is clear, and is most pronounced in the
# | third model, y ~ x1 + x2 + x3, since x3 is the regressor most strongly correlated with x1.
# 
# ...
# 
# |=============================================                                                     |  46%
# | In these two simulations we had 1000 samples of estimated coefficients, hence could calculate sample
# | variance in order to illustrate the effect. In a real case, we have only one set of coefficients and we
# | depend on theoretical estimates. However, theoretical estimates contain an unknown constant of
# | proportionality. We therefore depend on ratios of theoretical estimates called Variance Inflation
# | Factors, or VIFs.
# 
# ...
# 
# |=================================================                                                 |  50%
# | A variance inflation factor (VIF) is a ratio of estimated variances, the variance due to including the
# | ith regressor, divided by that due to including a corresponding ideal regressor which is uncorrelated
# | with the others. VIF's can be calculated directly, but the car package provides a convenient method for
# | the purpose as we will illustrate using the Swiss data from the datasets package.
# 
# ...
# 
# |=====================================================                                             |  54%
# | According to its documentation, the Swiss data set consists of a standardized fertility measure and
# | socioeconomic indicators for each of 47 French-speaking provinces of Switzerland in about 1888 when Swiss
# | fertility rates began to fall. Type head(swiss) or View(swiss) to examine the data.
# 
# > head(swiss)
# Fertility Agriculture Examination Education Catholic Infant.Mortality
# Courtelary        80.2        17.0          15        12     9.96             22.2
# Delemont          83.1        45.1           6         9    84.84             22.2
# Franches-Mnt      92.5        39.7           5         5    93.40             20.2
# Moutier           85.8        36.5          12         7    33.77             20.3
# Neuveville        76.9        43.5          17        15     5.16             20.6
# Porrentruy        76.1        35.3           9         7    90.57             26.6
# 
# | Nice work!
#         
#         |=========================================================                                         |  58%
# | Fertility was thought to depend on five socioeconomic factors: the percent of males working in
# | Agriculture, the percent of draftees receiving the highest grade on the army's Examination, the percent
# | of draftees with Education beyond primary school, the percent of the population which was Roman Catholic,
# | and the rate of Infant Mortality in the province. Use linear regression to model Fertility in terms of
# | these five regressors and an intercept. Store the model in a variable named mdl.
# 
# > md1 <- lm(Fertility ~ ., data = swiss)
# 
# | You seem to have misspelled the model's name. I was expecting mdl but you apparently typed md1.
# 
# | Nice try, but that's not exactly what I was hoping for. Try again. Or, type info() for more options.
# 
# | Entering mdl <- lm(Fertility ~ ., swiss) is the easiest way to model Fertility as a function of all five
# | regressors. The dot after the ~ means to include all (including an intercept.)
# 
# > mdl <- lm(Fertility ~ ., data = swiss)
# 
# | That's correct!
#         
#         |=============================================================                                     |  62%
# | Calculate the VIF's for each of the regressors using vif(mdl).
# 
# > vif(mdl)
# Agriculture      Examination        Education         Catholic Infant.Mortality 
# 2.284129         3.675420         2.774943         1.937160         1.107542 
# 
# | Your dedication is inspiring!
# 
# |=================================================================                                 |  67%
# | These VIF's show, for each regression coefficient, the variance inflation due to including all the
# | others. For instance, the variance in the estimated coefficient of Education is 2.774943 times what it
# | might have been if Education were not correlated with the other regressors. Since Education and score on
# | an Examination are likely to be correlated, we might guess that most of the variance inflation for
# | Education is due to including Examination.
# 
# ...
# 
# |=====================================================================                             |  71%
# | Make a second linear model of Fertility in which Examination is omitted, but the other four regressors
# | are included. Store the result in a variable named mdl2.
# 
# > mdl2 <- lm(Fertility ~ . -Examination, data = swiss)
# 
# | You are quite good my friend!
#         
#         |==========================================================================                        |  75%
# | Calculate the VIF's for this model using vif(mdl2).
# 
# > vif(mdl2)
# Agriculture        Education         Catholic Infant.Mortality 
# 2.147153         1.816361         1.299916         1.107528 
# 
# | Great job!
# 
# |==============================================================================                    |  79%
# | As expected, omitting Examination has markedly decreased the VIF for Education, from 2.774943 to
# | 1.816361. Note that omitting Examination has had almost no effect the VIF for Infant Mortality. Chances
# | are Examination and Infant Mortality are not strongly correlated. Now, before finishing this lesson,
# | let's review several significant points.
# 
# ...
# 
# |==================================================================================                |  83%
# | A VIF describes the increase in the variance of a coefficient due to the correlation of its regressor
# | with the other regressors. What is the relationship of a VIF to the standard error of its coefficient?
#         
#         1: There is no relationship.
# 2: They are the same.
# 3: VIF is the square of standard error inflation.
# 
# Selection: 2
# 
# | You're close...I can feel it! Try it again.
# 
# | Variance is the square of standard deviation, and standard error is the standard deviation of an
# | estimated coefficient.
# 
# 1: There is no relationship.
# 2: They are the same.
# 3: VIF is the square of standard error inflation.
# 
# Selection: 3
# 
# | That's the answer I was looking for.
# 
# |======================================================================================            |  88%
# | If a regressor is strongly correlated with others, hence will increase their VIF's, why shouldn't we just
# | exclude it?
#         
#         1: Excluding it might bias coefficient estimates of regressors with which it is correlated.
# 2: We should never exclude anything.
# 3: We should always exclude it.
# 
# Selection: 1
# 
# | Nice work!
#         
#         |==========================================================================================        |  92%
# | The problems of variance inflation and bias due to excluded regressors both involve correlated
# | regressors. However there are methods, such as factor analysis or principal componenent analysis, which
# | can convert regressors to an equivalent uncorrelated set. Why then, when modeling, should we not just use
# | uncorrelated regressors and avoid all the trouble?
#         
#         1: We should always use uncorrelated regressors.
# 2: Factor analysis takes too much computation.
# 3: Using converted regressors may make interpretation difficult.
# 
# Selection: 3
# 
# | Excellent work!
#         
#         |==============================================================================================    |  96%
# | That completes the exercise in variance inflation. The issue of omitting regressors is discussed in
# | another lesson.
# 
# ...
# 
# |==================================================================================================| 100%
# | Would you like to receive credit for completing this course on Coursera.org?
#         
#         1: No
# 2: Yes
# 
# Selection: 2
# What is your email address? olivierzach618@gmail.com
# What is your assignment token? Kvo97TehrE7TuIMw
# Grade submission succeeded!
#         
#         | You got it!
#         
#         | You've reached the end of this lesson! Returning to the main menu...
# 
# | Please choose a course, or type 0 to exit swirl.













## Poisson Regression: Generalized Linear Model
# many data take the form of unbounded count data
# calls to call center, sales, flu cases etc. 
# in some cases counts are clearly bounded
# however modeling counts as unbounded is often done when the upper limit is not known or very large relative to the number of events
# if the upper bound is known - we can use these techniques to model proportion or rate
# the starting point for most count analysis is the Poisson distribution
# linear regression with transformation is an option - but Poisson may model count data better
# the poissson is a useful distribution for counts and rates
# modeling web traffic, incidence rates, approximating binomial probabilities with small p and large n, analyzing contingency table data
# if x is poisson then mean is T * lambda
# expected value of estimate of rate it x / t = lambda
# possion tends to a normal distribution t * lambda is large = t or lambda can get large to take us to an approximate normal distribution
# the variance of x is t * lambda
# poisson mean and variance EQUAL!! in the poisson distribution

## Poisson example: web traffic 
# our unit of time is t = 1 = web hits per one day!

install.packages("devtools")
library(devtools)

install_github("rga", "skardhamar")
library(rga)

# load the data
load("~/Downloads/gaData.rda")

gaData$julian <- julian(gaData$date)
head(gaData)
# date visits simplystats julian
# 1 2011-01-01      0           0  14975
# 2 2011-01-02      0           0  14976
# 3 2011-01-03      0           0  14977
# 4 2011-01-04      0           0  14978
# 5 2011-01-05      0           0  14979
# 6 2011-01-06      0           0  14980

# plot the data
plot(gaData$julian, gaData$visits, pch = 19, col = "darkgrey", xlab = "Julian", ylab = "Visits")

# linear regression has issues with count data
# however - as the mean of the counts get larger and larger we trend to the normal dsitribution!
# let's plot the regression line through the website count data
# we cannot model the curvature of the web traffic data
# consider taking the log of the outcome for better inpretability
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)

# when you take the natural log of the outcome of a linear regression - your exponteatiaed coefficients can relate back to the geometeric mean
# we are estimating things about geometric means
# this is how we get an interpretable intercept! = e^BO = exponentiated scale!
# e^B1 is the estaimated relative increase or decrease in geometric mean hits per day

# after converting intercept to geometric form (natural log) - we now have interpretable intercepts
# our coefficient for $julian is the expected increase in web traffic on the EXPOENTIATED SCALE
# take this through the natural log to get the normal units!
round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)
# (Intercept) gaData$julian 
# 0.00000       1.00231 

## Poisson vs. Linear
# in glms we do not take a transformatoin of the repsonse itself - we transform the mean of the means!
# in a poisson log / linear we have the log of the expected web hits per day
# mean web hits per day depends on e^ linear regression data
# assume data is poisson data - we have exp^THE NORMAL LINEAR REGRESSION MODEL

# differences are now multiplicative when transferred back to the NORMAL scale
# we have our normal regression just exponentiated
# slope coefficient: increase / decrease in mean per one unit change in regressor
# exponentiate = close to 1
# log scale = close to 0
# these easily scale to multiple regression = interpretation will be the increase / decrease holding other variables constant
# think of it as adding and exp to all terms in our normal linear regression model

# fitting the poisson model
# blue line is the poisson fit that "curves" better than the strict linear regression
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
glm1 <- glm(gaData$visits ~ gaData$julian,family="poisson")
abline(lm1,col="red",lwd=3); lines(gaData$julian,glm1$fitted,col="blue",lwd=3)

# mean variance relationship?
# in poisson the mean and variance must be equal...as mean goes up variance should also go up
# this mean variance relationship is critical to using the Poisson distribution
# examine the relationship in this plot - do we notice a increasing pattern? NOT REALLY...
# variance is actually higher for low fitted observations...
# possible use of a quasi-poisson - variance will be a constant multiple of the mean
plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")


# model agnostic standard errors?
# we can estimate confidence intervals and coefficients using a model agnostic error
# https://stackoverflow.com/questions/3817182/vcovhc-and-confidence-interval
library(sandwich)
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
        cf <- coef(object); pnames <- names(cf)
        if (missing(parm))
                parm <- pnames
        else if (is.numeric(parm))
                parm <- pnames[parm]
        a <- (1 - level)/2; a <- c(a, 1 - a)
        pct <- stats:::format.perc(a, 3)
        fac <- qnorm(a)
        ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                                   pct))
        ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
        ci[] <- cf[parm] + ses %o% fac
        ci
}

# non-agnostic confidence interval generator
confint(glm1)
# Waiting for profiling to be done...
#                       2.5 %        97.5 %
# (Intercept)   -34.346577587 -31.159715656
# gaData$julian   0.002190043   0.002396461

# agnostic confidence interval generator
confint.agnostic(glm1)
#                               2.5 %        97.5 %
#         (Intercept)   -36.362674594 -29.136997254
#         gaData$julian   0.002058147   0.002527955



# how do we handle rates and proportions?
# we have a count and an offset telling me how large the counts should be...
# if our counts have a term we want to interpret as we can model the rate or the proportion
# share of simply statistics over total web hits as the repsonse model
# we can do this in a log linear model!
# log of the outcome = the log terms + the regular regression term
# add your denominator into your  linear model as an OFFSET log term

# fit a rate or proportion model
# notice the offset term that is added to the glm call
# specify the family as Poission - R will give you the correct link function - LOGIT link function
# plotted here is the total website hits versus modeling the proportion of simply statistics web traffic
# the blue points are adjusted for the red points
glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
points(julian(gaData$date),glm1$fitted,col="red",pch=19)


# fitted model to the actual simply statistics data
glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),gaData$simplystats/(gaData$visits+1),col="grey",xlab="Date",
     ylab="Fitted Rates",pch=19)
lines(julian(gaData$date),glm2$fitted/(gaData$visits+1),col="blue",lwd=3)


# watch out for zero inflation - lots of data with zeros...
# pscl package helps assist with zero inflation modeling...
# we can use our linear regression framework to "apply" them the binary / logistic / poission cases



## swirl practice: overfitting and underfitting
# | Please choose a lesson, or type 0 to return to course menu.
# 
# 1: Introduction
# 2: Residuals
# 3: Least Squares Estimation
# 4: Residual Variation
# 5: Introduction to Multivariable Regression
# 6: MultiVar Examples
# 7: MultiVar Examples2
# 8: MultiVar Examples3
# 9: Residuals Diagnostics and Variation
# 10: Variance Inflation Factors
# 11: Overfitting and Underfitting
# 12: Binary Outcomes
# 13: Count Outcomes
# 
# Selection: 11
# 
# | Attempting to load lesson dependencies...
# 
# | Package ‘car’ loaded correctly!
#         
#         |                                                    |   0%
# 
# | Overfitting and Underfitting. (Slides for this and other
#                                  | Data Science courses may be found at github
#                                  | https://github.com/DataScienceSpecialization/courses. If
#                                  | you care to use them, they must be downloaded as a zip file
#                                  | and viewed locally. This lesson corresponds to
#                                  | Regression_Models/02_04_residuals_variation_diagnostics.)
# 
# ...
# 
# |==                                                  |   4%
# | The Variance Inflation Factors lesson demonstrated that including new variables will increase standard
# | errors of coefficient estimates of other, correlated regressors. Hence, we don't want to idly throw
# | variables into the model. On the other hand, omitting variables results in bias in coefficients of
# | regressors which are correlated with the omitted ones. In this lesson we demonstrate the effect of omitted
# | variables and discuss the use of ANOVA to construct parsimonious, interpretable representations of the data.
# 
# ...
# 
# |====                                                |   7%
# | First, I would like to illustrate how omitting a correlated regressor can bias estimates of a coefficient.
# | The relevant source code is in a file named fitting.R which I have copied into your working directory and
# | tried to display in your source code editor. If I've failed to display it, you should open it manually.
# 
# ...
# 
# |======                                              |  11%
# | Find the function simbias() at the top of fitting.R. Below the comment labeled Point A three regressors, x1,
# | x2, and x3, are defined. Which of these two are correlated?
#         
#         1: x1 and x2
# 2: x2 and x3
# 3: x1 and x3
# 
# Selection: 1
# 
# | That's a job well done!
# 
# |========                                            |  15%
# | Within simbias() another function, f(n), is defined. It forms a dependent variable, y, and at Point C
# | returns the coefficient of x1 as estimated by two models, y ~ x1 + x2, and y ~ x1 + x3. One regressor is
# | missing in each model. In the expression for y (Point B,) what is the actual coefficient of x1?
# 
# 1: 1/sqrt(2)
# 2: 1
# 3: 0.3
# 
# Selection: 3
# 
# | Keep trying!
# 
# | What is the coefficient of x1 in the sum, x1 + x2 + x3?
# 
# 1: 1
# 2: 0.3
# 3: 1/sqrt(2)
# 
# Selection: 1
# 
# | Your dedication is inspiring!
# 
# |==========                                          |  19%
# | At Point D in simbias() the internal function, f(), is applied 150 times and the results returned as a 2x150
# | matrix. The first row of this matrix contains independent estimates of x1's coefficient in the case that x3,
# | the regressor uncorrelated with x1, is omitted. The second row contains estimates of x1's coefficient when
# | the correlated regressor, x2, is omitted. Use simbias(), accepting the default argument, to form these
# | estimates and store the result in a variable called x1c. (The default argument just guarantees a nice
# | histogram, in a figure to follow.)
# 
# > x1c <- simbias()
# 
# | You got it right!
# 
# |============                                        |  22%
# | The actual coefficient of x1 is 1. Having been warned that omitting a correlated regressor would bias
# | estimates of x1's coefficient, we would expect the mean estimate of x1c's second row to be farther from 1
# | than the mean of x1c's first row. Using apply(x1c, 1, mean), find the means of each row.
# 
# > apply(x1c, 1, mean)
# x1       x1 
# 1.034403 1.476944 
# 
# | You are quite good my friend!
#         
#         |=============                                       |  26%
# | Histograms of estimates from x1c's first row (blue) and second row (red) are shown. Estimates from the
# | second row are clearly more than two standard deviations from the correct value of 1, and the bias due to
# | omitting the correlated regressor is evident. (The code which produced this figure is incidental to the
# | lesson, but is available as the function x1hist(), at the bottom of fitting.R.)
# 
# ...
# 
# |===============                                     |  30%
# | Adding even irrelevant regressors can cause a model to tend toward a perfect fit. We illustrate this by adding random regressors
# | to the swiss data and regressing on progressively more of them. As the number of regressors approaches the number of data points
# | (47), the residual sum of squares, also known as the deviance, approaches 0. (The source code for this figure can be found as
# | function bogus() in fitting.R.
# 
# ...
# 
# |=================                                   |  33%
# | In the figure, adding random regressors decreased deviance, but we would be mistaken to believe that such decreases are
# | significant. To assess significance, we should take into account that adding regressors reduces residual degrees of freedom.
# | Analysis of variance (ANOVA) is a useful way to quantify the significance of additional regressors. To exemplify its use, we will
# | use the swiss data.
# 
# ...
# 
# |===================                                 |  37%
# | Recall that the Swiss data set consists of a standardized fertility measure and socioeconomic indicators for each of 47
# | French-speaking provinces of Switzerland in 1888. Fertility was thought to depend on an intercept and five factors denoted as
# | Agriculture, Examination, Education, Catholic, and Infant Mortality. To begin our ANOVA example, regress Fertility on Agriculture
# | and store the result in a variable named fit1.
# 
# > fit1 <- lm(Fertility ~ Agriculture, data = swiss)
# 
# | You are doing so well!
# 
# |=====================                               |  41%
# | Create another model, named fit3, by regressing Fertility on Agriculture and two additonal regressors, Examination and Education.
# 
# > fit3 <- lm(Fertility ~ Agriculture + Examination + Education, data = swiss)
# 
# | You are doing so well!
# 
# |=======================                             |  44%
# | We'll now use anova to assess the significance of the two added regressors. The null hypothesis is that the added regressors are
# | not significant. We'll explain in detail shortly, but right now just apply the significance test by entering anova(fit1, fit3).
# 
# > anova(fit1, fit3)
# Analysis of Variance Table
# 
# Model 1: Fertility ~ Agriculture
# Model 2: Fertility ~ Agriculture + Examination + Education
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     45 6283.1                                  
# 2     43 3180.9  2    3102.2 20.968 4.407e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# | You are amazing!
# 
# |=========================                           |  48%
# | The three asterisks, ***, at the lower right of the printed table indicate that the null hypothesis is rejected at the 0.001
# | level, so at least one of the two additional regressors is significant. Rejection is based on a right-tailed F test, Pr(>F),
# | applied to an F value. According to the table, what is that F value?
# 
# 1: 3102.2
# 2: 20.968
# 3: 45
# 
# Selection: 2
# 
# | You nailed it! Good job!
# 
# |===========================                         |  52%
# | An F statistic is a ratio of two sums of squares divided by their respective degrees of freedom. If the two scaled sums are
# | independent and centrally chi-squared distributed with the same variance, the statistic will have an F distribution with
# | parameters given by the two degrees of freedom. In our case, the two sums are residual sums of squares which, as we know, have
# | mean zero hence are centrally chi-squared provided the residuals themselves are normally distributed. The two relevant sums are
# | given in the RSS (Residual Sum of Squares) column of the table. What are they?
# 
# 1: 45 and 43
# 2: 2 and 3102.2
# 3: 6283.1 and 3180.9
# 
# Selection: 3
# 
# | Keep up the great work!
# 
# |=============================                       |  56%
# | R's function, deviance(model), calculates the residual sum of squares, also known as the deviance, of the linear model given as
# | its argument. Using deviance(fit3), verify that 3180.9 is fit3's residual sum of squares. (Of course, fit3 is called Model 2 in
# | the table.)
# 
# > deviance(fit3)
# [1] 3180.925
# 
# | You are doing so well!
# 
# |===============================                     |  59%
# | In the next several steps, we will show how to calculate the F value, 20.968, which appears in the table printed by anova().
# | We'll begin with the denominator, which is fit3's residual sum of squares divided by its degrees of freedom. Fit3 has 43 residual
# | degrees of freedom. This figure is obtained by subtracting 4, the the number of fit3's predictors (the 3 named and the
#                                                                                                      | intercept,) from 47, the number of samples in swiss. Store the value of deviance(fit3)/43 in a variable named d.
# 
# > d <- deviance(fit3) / 43
# 
# | Great job!
#         
#         |=================================                   |  63%
# | The numerator is the difference, deviance(fit1)-deviance(fit3), divided by the difference in the residual degrees of freedom of
# | fit1 and fit3, namely 2. This calculation requires some theoretical justification which we omit, but the essential idea is that
# | fit3 has 2 predictors in addition to those of fit1. Calculate the numerator and store it in a variable named n.
# 
# > n <- deviance(fit1) - deviance(fit3)
# 
# | Not quite right, but keep trying. Or, type info() for more options.
# 
# | Enter n <- (deviance(fit1) - deviance(fit3))/2 at the R prompt.
# 
# > n <- (deviance(fit1) - deviance(fit3))/2
# 
# | Keep up the great work!
#         
#         |===================================                 |  67%
# | Calculate the ratio, n/d, to show it is essentially equal to the F value, 20.968, given by anova().
# 
# > n/d
# [1] 20.96783
# 
# | You are quite good my friend!
#         
#         |=====================================               |  70%
# | We'll now calculate the p-value, which is the probability that a value of n/d or larger would be drawn from an F distribution
# | which has parameters 2 and 43. This value was given as 4.407e-07 in the column labeled Pr(>F) in the table printed by anova(), a
# | very unlikely value if the null hypothesis were true. Calculate this p-value using pf(n/d, 2, 43, lower.tail=FALSE).
# 
# > pf(n/d, 2, 43, lower.tail=FALSE)
# [1] 4.406913e-07
# 
# | Excellent job!
# 
# |=======================================             |  74%
# | Based on the calculated p-value, a false rejection of the null hypothesis is extremely unlikely. We are confident that fit3 is
# | significantly better than fit1, with one caveat: analysis of variance is sensitive to its assumption that model residuals are
# | approximately normal. If they are not, we could get a small p-value for that reason. It is thus worth testing residuals for
# | normality. The Shapiro-Wilk test is quick and easy in R. Normality is its null hypothesis. Use shapiro.test(fit3$residuals) to
# | test the residual of fit3.
# 
# > shapiro.test(fit3$residuals)
# 
# Shapiro-Wilk normality test
# 
# data:  fit3$residuals
# W = 0.97276, p-value = 0.336
# 
# 
# | That's correct!
#         
#         |========================================            |  78%
# | The Shapiro-Wilk p-value of 0.336 fails to reject normality, supporting confidence in our analysis of variance. In order to
# | illustrate the use of anova() with more than two models, I have constructed fit5 and fit6 using the first 5 and all 6 regressors
# | (including the intercept) respectively. Thus fit1, fit3, fit5, and fit6 form a nested sequence of models; the regressors of one
# | are included in those of the next. Enter anova(fit1, fit3, fit5, fit6) at the R prompt now to get the flavor.
# 
# > anova(fit1, fit3, fit5, fit6) 
# Analysis of Variance Table
# 
# Model 1: Fertility ~ Agriculture
# Model 2: Fertility ~ Agriculture + Examination + Education
# Model 3: Fertility ~ Agriculture + Examination + Education + Catholic
# Model 4: Fertility ~ Agriculture + Examination + Education + Catholic + 
#         Infant.Mortality
# Res.Df    RSS Df Sum of Sq       F    Pr(>F)    
# 1     45 6283.1                                   
# 2     43 3180.9  2   3102.19 30.2107 8.638e-09 ***
#         3     42 2513.8  1    667.13 12.9937 0.0008387 ***
#         4     41 2105.0  1    408.75  7.9612 0.0073357 ** 
#         ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# | You nailed it! Good job!
#         
#         |==========================================          |  81%
# | It appears that each model is a significant improvement on its predecessor. Before ending the lesson, let's review a few salient
# | points.
# 
# ...
# 
# |============================================        |  85%
# | Omitting a regressor can bias estimation of the coefficient of certain other regressors. Which ones?
# 
# 1: Uncorrelated regressors
# 2: Correlated regressors
# 
# Selection: 2
# 
# | You got it!
# 
# |==============================================      |  89%
# | Including more regressors will reduce a model's residual sum of squares, even if the new regressors are irrelevant. True or
# | False?
#         
#         1: False
# 2: It depends on circumstances.
# 3: True
# 
# Selection: 3
# 
# | You are really on a roll!
#         
#         |================================================    |  93%
# | When adding regressors, the reduction in residual sums of squares should be tested for significance above and beyond that of
# | reducing residual degrees of freedom. R's anova() function uses an F-test for this purpose. What else should be done to insure
# | that anova() applies?
# 
# 1: Model residuals should be tested for normality.
# 2: Regressors should be tested for normality.
# 3: The residuals should be tested for having zero means.
# 
# Selection: 1
# 
# | Keep up the great work!
# 
# |==================================================  |  96%
# | That completes the lesson on underfitting and overfitting.
# 
# ...



## regression model motivation 
# how can we extend our linear regression, glm models?
# how to fit functions using linear models - adding squared terms?
# how do we fit complicated functions in basic linear models? i.e. sine curve? 
# we can complicated terms of x - a NOT point & gamma to our regular linear regression
# we can "kink" our lines to fit more curvy functions in regression...
# we "break" our regression at certain points to fit the more curvy line

# modelling sine curve plus noise
# simulation example
# here our line actually moves with the sine curve + noise data!!
# we define the breaks points as "knots"
# we spline at the different knots
# we define a line and the spline 
# we fit our model as the regular regression plus the spline term!!
n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
knots <- seq(0, 8 * pi, length = 20); 
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

# how to we correct the sharpness of our fitted curvy regression line?
# we add squared terms into the same regression model!
# the new model is the regression + knots + squared term!!
# all we have done is ordinary regression but fit a very complicated function!!
# this is regression splines...but we need to know where to put the knot points!
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2)
xMat <- cbind(1, x, x^2, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

## quick notes
# the collection of splines is a basis for foundation for modeling functions
# single knot point terms can fit hockey stick like processes!
# these basis can be used in GLMs well
# an issue is that the large number of parameters introduces tough inference
# we donn't know where the knot points are and we don't want to put in too many
# WE USE REGULARIZATION TO COMBAT THIS PROBLEM - we penalize coefficents that get too large after putting in lots of knots

## how do we model harmonics?
# can we recognize the notes from a chord????
# we do this by frequenies of different notes for one octave of a scale
# we sample this data every two seconds
# create a sinwave at each note frequency!!
# then we attempt to model this data!
# we create a basis - all sine functions for every notes
# fit linear model with chord as outcome with sine notes as predictors

##Chord finder, playing the white keys on a piano from octave c4 - c5
notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)
t <- seq(0, 2, by = .001); n <- length(t)
c4 <- sin(2 * pi * notes4[1] * t); e4 <- sin(2 * pi * notes4[3] * t); 
g4 <- sin(2 * pi * notes4[5] * t)
chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)
x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
fit <- lm(chord ~ x - 1)

plot(c(0, 9), c(0, 1.5), xlab = "Note", ylab = "Coef^2", axes = FALSE, frame = TRUE, type = "n")
axis(2)
axis(1, at = 1 : 8, labels = c("c4", "d4", "e4", "f4", "g4", "a4", "b4", "c5"))
for (i in 1 : 8) abline(v = i, lwd = 3, col = grey(.8))
lines(c(0, 1 : 8, 9), c(0, coef(fit)^2, 0), type = "l", lwd = 3, col = "red")

##how you would really do it
# we want to fit all sine and cosine frequencies in a linear model for all possible notes
# this is the fueria note regression - distrete fueria transformation
# basic idea is still to fit a model based on the note coefficients in the model
# the bars with high coeffiicents will give you the notes that stand out - the C MAJOR CHORD
# Tukey discovered how do the fast fueria model - even better than the linear model - but the "idea" of fitting a function with linear regression is important
# the scope of linear models is extreme - we can diagnose chords with models!!!
# REGRESSION IS FUCKING POWERFUL YOU BITCH!!!!
a <- fft(chord); plot(Re(a)^2, type = "l")




## Quiz Week 4: Regression Models
# https://rpubs.com/cheyu/reg-q4


## Question 1: 
# consider the space shuttle data in the MASS library
# fit a logistic regression model predicted by wind
# give the estimated odds ratio for autolander comparing winds - headwind / tailwind

# solution: 
# fit the logistic regression model and interpret the results

# load the data
library("MASS")
data(shuttle)
str(shuttle)
# 'data.frame':	256 obs. of  7 variables:
#         $ stability: Factor w/ 2 levels "stab","xstab": 2 2 2 2 2 2 2 2 2 2 ...
# $ error    : Factor w/ 4 levels "LX","MM","SS",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ sign     : Factor w/ 2 levels "nn","pp": 2 2 2 2 2 2 1 1 1 1 ...
# $ wind     : Factor w/ 2 levels "head","tail": 1 1 1 2 2 2 1 1 1 2 ...
# $ magn     : Factor w/ 4 levels "Light","Medium",..: 1 2 4 1 2 4 1 2 4 1 ...
# $ vis      : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
# $ use      : Factor w/ 2 levels "auto","noauto": 1 1 1 1 1 1 1 1 1 1 ...

# we need to change the data to be 1 for auto and 0 or non-auto
shuttle.new <- shuttle %>% 
        mutate(use = ifelse(use == "auto", 1, 0))

str(shuttle.new)

# fit wind as a factor to compare between heads and tails
# remove the intercept so we have clear interpretable coefficients
fit1 <- glm(use ~ factor(wind) - 1, family = "binomial", data = shuttle.new)
summary(fit1)$coef
# Estimate Std. Error  z value  Pr(>|z|)
# factor(wind)head 0.2513144  0.1781742 1.410499 0.1583925
# factor(wind)tail 0.2831263  0.1785510 1.585689 0.1128099

# we need to extract the coefficent to calculate the odds
coef.table <- data.frame(fit1$coefficients)

# we need to divide the heads coefficents by the tails coefficent AFTER WE BRING THE DATA INTO REGULAR SCALE
# remember the logistic regression will have interpretable intercepts in the log scale
# we need to use exp to translate back the results into the data scale!!
coef.table <- coef.table %>% 
        mutate(exp.trans = exp(fit1.coefficients))
str(coef.table)

# odds calculation of heads coefficient divided by the tails coefficient
coef.table[1,2] / coef.table[2,2]
# [1] 0.9686888





## Question 2:
# consider the same problem from above
# give the estimated odds ratio for use comparing head winds to tail winds ADJUSTING FOR wind strength = variable magn

# solution: 
# fit the same logistic model with the new magn variable included - interpret

# load the data
library("MASS")
data(shuttle)
str(shuttle)

# we need to change the data to be 1 for auto and 0 or non-auto
shuttle.new <- shuttle %>% 
        mutate(use = ifelse(use == "auto", 1, 0))

str(shuttle.new)

# fit wind as a factor to compare between heads and tails adjusting for wind strength
# remove the intercept so we have clear interpretable coefficients
fit2 <- glm(use ~ factor(wind) + factor(magn) - 1, family = "binomial", data = shuttle.new)
summary(fit2)$coef
# Estimate Std. Error       z value  Pr(>|z|)
# factor(wind)head    3.635093e-01  0.2840608  1.279688e+00 0.2006547
# factor(wind)tail    3.955180e-01  0.2843987  1.390717e+00 0.1643114
# factor(magn)Medium -1.009525e-15  0.3599481 -2.804642e-15 1.0000000
# factor(magn)Out    -3.795136e-01  0.3567709 -1.063746e+00 0.2874438
# factor(magn)Strong -6.441258e-02  0.3589560 -1.794442e-01 0.8575889

# we need to extract the coefficent to calculate the odds
coef.table2 <- data.frame(fit2$coefficients)

# we need to divide the heads coefficents by the tails coefficent AFTER WE BRING THE DATA INTO REGULAR SCALE
# remember the logistic regression will have interpretable intercepts in the log scale
# we need to use exp to translate back the results into the data scale!!
coef.table2 <- coef.table2 %>% 
        mutate(exp.trans = exp(fit2.coefficients))
str(coef.table2)
coef.table2

# odds calculation of heads coefficient divided by the tails coefficient
coef.table2[1,2] / coef.table2[2,2]
# [1] 0.9684981



## Question 3: 
# if you fit a logistic regression model to a binary variable...
# then fit another model for 1 - the outcome...
# what are we doing?
# we are flipping the repsonse of use! 
# NOTICE THAT 1 - RESPONSE of 1 and 0 = 0 (using auto!) and 1 (not using auto!)
# we guess that this change will flip the coeffients sign but not change them!

# solution: test out the model - what happens to the coefficients?

# fit a glm logistic regression - specific the family as binomial
# add in the 1 - term into the repsonse of the model
# view coeffients to see what happens to them!!
fit3 <- glm(I(1 - use) ~ factor(wind) - 1, family = "binomial", data = shuttle.new)
summary(fit3)$coef
# Estimate Std. Error   z value  Pr(>|z|)
# factor(wind)head -0.2513144  0.1781742 -1.410499 0.1583925
# factor(wind)tail -0.2831263  0.1785510 -1.585689 0.1128099

# original model without the 1 - term in the response!
summary(fit1)$coef
# Estimate Std. Error  z value  Pr(>|z|)
# factor(wind)head 0.2513144  0.1781742 1.410499 0.1583925
# factor(wind)tail 0.2831263  0.1785510 1.585689 0.1128099

# we correctly guessed that the coefficients would stay the same but flip thier signs!!!



## Question 4:
# consider the insect sprays data: InsectSprays
# fit a poisson model using spray as a factor level
# report the estimated relative rate comparing spray A to spray B

# solution - fit the poisson model without intercept for comparable interpreability
# extract coefficents to calculate the rate of SPRAY A / SPRAY B
# model is bugs dead (variable: count) vs. spray(as factor variable)

# load the data
data("InsectSprays")
str(InsectSprays)

# fit the model
# do not forget to fit without an intercept!
fit4 <- glm(count ~ factor(spray) - 1, family = "poisson", data = InsectSprays)
summary(fit4)$coef
# Estimate Std. Error   z value      Pr(>|z|)
# factor(spray)A 2.6741486 0.07580980 35.274443 1.448048e-272
# factor(spray)B 2.7300291 0.07372098 37.031917 3.510670e-300
# factor(spray)C 0.7339692 0.19999987  3.669848  2.426946e-04
# factor(spray)D 1.5926308 0.13018891 12.233229  2.065604e-34
# factor(spray)E 1.2527630 0.15430335  8.118832  4.706917e-16
# factor(spray)F 2.8134107 0.07071068 39.787636  0.000000e+00

# extract spray A and spray B coefficients and calcualte the rate
coef.table3 <- data.frame(summary(fit4)$coef)
coef.table3

# transform estimates into the correct form - need to calculate our of log units
coef.table3 <- data.frame(coef.table3[,1]) %>% 
        mutate(exp.trans = exp(coef.table3...1.))

# relative rate calculation 
coef.table3[1,2] / coef.table3[2,2]
# [1] 0.9456522





## Question 5:
# consider a poisson glm with an offset t
# model example: glm(count ~ I(x + offset(t)), family = poisson)
# x is factor variable comparing treatment (1) to control (0)
# t is the natural log of our monitoring time
# what is the impact of the coefficient for x if we change the units of the offset variable?
# example model: glm(count ~ I(x + offset(t*2)), family = poisson) where 2 = log(10) + t
# note: adding log(10) on the log scale is multiplying 10 on the original side...

# solution: first guess is the coefficient will not be changed we are just scaling the units of x...
# lets fit some models to find out how the effect actually changes the coefficient...

# fit the model with initial offset
fit5 <- glm(count ~ factor(spray) + offset(log(rep(sum(count), length(count)))) , 
            family = "poisson", data = InsectSprays)

# fit the model with the scaled offset * 10
fit6 <- glm(count ~ factor(spray) + 
                       offset(log(10) + log(rep(sum(count), length(count)))), 
               family = "poisson", data = InsectSprays)

# compare the results
coef(summary(fit5))
# Estimate Std. Error     z value     Pr(>|z|)
# (Intercept)    -3.85380927  0.0758098 -50.8352356 0.000000e+00
# factor(spray)B  0.05588046  0.1057445   0.5284477 5.971887e-01
# factor(spray)C -1.94017947  0.2138857  -9.0711059 1.178151e-19
# factor(spray)D -1.08151786  0.1506528  -7.1788745 7.028761e-13
# factor(spray)E -1.42138568  0.1719205  -8.2676928 1.365763e-16
# factor(spray)F  0.13926207  0.1036683   1.3433422 1.791612e-01

coef(summary(fit6))
# Estimate Std. Error     z value     Pr(>|z|)
# (Intercept)    -6.15639436  0.0758098 -81.2084191 0.000000e+00
# factor(spray)B  0.05588046  0.1057445   0.5284477 5.971887e-01
# factor(spray)C -1.94017947  0.2138857  -9.0711059 1.178151e-19
# factor(spray)D -1.08151786  0.1506528  -7.1788745 7.028761e-13
# factor(spray)E -1.42138568  0.1719205  -8.2676928 1.365763e-16
# factor(spray)F  0.13926207  0.1036683   1.3433422 1.791612e-01

# coefficients are the same but the intercept changes!! 
# intercept is the estimate of the model at x = 0, it will change based on the scale of t





## Question 6:
# consider the following data
# using a knot point at 0, fit a linear model that looks like a hockey stick
# our two lines will meet at x = 0
# model should include the knot point and an intercept
# what is the estimated slope of the line after 0?

# solution: define the knot point, fit model with the knot point included - interpret
# review splines lecture

# load the data
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

# define knots and spline term
knots <- 0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))

# define what the model will look like on the right hand side
xMat <- cbind(1, x, splineTerms)

# fit the model and review results
fit7 <- lm(y ~ xMat - 1)
coef(summary(fit7))
# Estimate Std. Error    t value     Pr(>|t|)
# xMat  -0.1825806 0.13557812  -1.346682 2.149877e-01
# xMatx -1.0241584 0.04805280 -21.313188 2.470198e-08
# xMat   2.0372258 0.08574713  23.758531 1.048711e-08

# the estimate of the slope after 0 (our knot point) is given by xMatx added to  xMat!!
# this answer is coefficient[2] + coefficient[3]
2.0372258 - 1.0241584









