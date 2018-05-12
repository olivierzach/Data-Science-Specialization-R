## Data Science Capstone Project
# complete project using all 9 courses of the Data Science Specialization
# partner with swiftkey

# project summary

# Around the world, people are spending an increasing amount of time on their mobile devices for email, 
# social networking, banking and a whole range of other activities. But typing on mobile devices can be a serious pain. 
# SwiftKey, our corporate partner in this capstone, 
# builds a smart keyboard that makes it easier for people to type on their mobile devices. 
# One cornerstone of their smart keyboard is predictive text models. When someone types:
        
        # I went to the

# the keyboard presents three options for what the next word might be. 
# For example, the three words might be gym, store, restaurant. 
# In this capstone you will work on understanding and building predictive text models like those used by SwiftKey.

# This course will start with the basics, 
# analyzing a large corpus of text documents to discover the structure in the data and how words are put together.
# It will cover cleaning and analyzing text data, then building and sampling from a predictive text model. 
# Finally, you will use the knowledge you gained in data products to build a predictive text product 
# you can show off to your family, friends, and potential employers.

# You will use all of the skills you have learned during the Data Science Specialization in this course,
# but you'll notice that we are tackling a brand new application: 
# analysis of text data and natural language processing. 
# This choice is on purpose. 
# As a practicing data scientist you will be frequently confronted with new data types and problems. 
# A big part of the fun and challenge of being a data scientist is figuring out how to work with these new data types to build data products people love. 
# The capstone will be evaluated based on the following assessments:

# An introductory quiz to test whether you have downloaded and can manipulate the data.
# An intermediate R markdown report that describes in plain language, plots, and code your exploratory analysis of the course data set.
# Two natural language processing quizzes, where you apply your predictive model to real data to check how it is working.
# A Shiny app that takes as input a phrase (multiple words), one clicks submit, and it predicts the next word.
# A 5 slide deck created with R presentations pitching your algorithm and app to your boss or investor.
# During the capstone you can get support from your fellow students, from us, and from the engineers at SwiftKey. 
# But we really want you to show your independence, creativity, and initiative. 
# We have been incredibly impressed by your performance in the classes up until now and know you can do great things.

# We have compiled some basic natural language processing resources below.
# You are welcome to use these resources or any others you can find while performing this analysis.
# One thing to keep in mind is that we do not expect you to become a world's expert in natural language processing.
# The point of this capstone is for you to show you can explore a new data type, 
# quickly get up to speed on a new application, and implement a useful model in a reasonable period of time.
# We think NLP is very cool and depending on your future goals may be worth studying more in-depth, 
# but you can complete this project by using your general knowledge of data science and basic knowledge of NLP.

# Here are a few resources that might be good places to start as you tackle this ambitious project.

# Text mining infrastucture in R
# CRAN Task View: Natural Language Processing
# Videos and Slides from Stanford Natural Language Processing course

## course project examples
# https://rpubs.com/bradleyboehmke/41172

## course dataset:
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

## text mining in R
# https://www.tidytextmining.com




# set up enviroment -------------------------------------------------------

# define working directory
setwd("/Users/zacholivier/Desktop/R/text_mine_capstone/en_US")

# install needed packages
pacman::p_load(tm, ggplot2, tidytext, data.table, tidyverse, rio, ggthemes, wordcloud,
               stringi, caret)



# read in and prep the data --------------------------------------------------------


# list of common words we may want to filter out of analysis
stop_words <- stop_words %>% as.tibble(.)

# list of profanity words to censor out of analysis
profanity <- readr::read_table("bad_words.txt", col_names = F) %>% 
        filter(X1 != is.na(X1)) %>% 
        rename(., word = X1) %>% 
        rbind(c("niggas"))

# function to clean text using tm
cleanText <- function(x) {
        
        x %>% 
        str_replace_all("[^[:graph:]]", " ") %>% 
        removeNumbers() %>% 
        removePunctuation() %>% 
        tolower() %>% 
        removeWords(., profanity$word)
}


# read in data
twitter <- readr::read_table("en_US.twitter.txt", col_names = F)
blogs <- readr::read_table("en_US.blogs.txt", col_names = F) 
news <- readr::read_table("en_US.news.txt", col_names = F)

# tokenize and clean twitter data
(twitter_df <- sample_frac(twitter, .1) %>% 
        tibble::rownames_to_column(., var = "line_id") %>% 
        rename(., text = X1) %>% 
        mutate(text = cleanText(text),
               source = "twitter"))

# dim(twitter_df); twitter_df ; names(twitter_df); rm(twitter)

# read in blogs data


# dim(blogs); blogs; names(blogs); 

# tokenize and clean blogs data
(blogs_df <- sample_frac(blogs, .1) %>% 
        tibble::rownames_to_column(., var = "line_id") %>% 
        rename(., text = X1) %>% 
        mutate(text = cleanText(text),
               source = "blogs"))

# dim(blogs_df); blogs_df; names(blogs_df); rm(blogs)

# read in newspaper data


# dim(news); news; names(news);

# tokenize and clean news data
(news_df <- sample_frac(news, .1) %>% 
        tibble::rownames_to_column(., var = "line_id") %>% 
        rename(., text= X1) %>% 
        mutate(text = cleanText(text),
               source = "news"))

# dim(news_df); news_df; names(news_df); rm(news)


# quiz week 1 -------------------------------------------------------------

# https://rpubs.com/bjyenis/295494

## Question 1:
# the en_US.blogs.txt file is how many megabytes?

# solution:
size <- file.info("en_US.blogs.txt")
(mb <- (size$size / 1024) / 1024)
# [1] 200.4242


## Question 2:
# the twitter file has how many lines?

# solution:
dim(twitter_df)[[1]]
# [1] 2360148


## Question 3:
# what is the length of the longest line seen in any of the three US text datasets?

# solution
a = sapply(X = list(twitter = twitter_df$tweet_text, 
                    blogs = blogs_df$blog_text,
                    news = news_df$news_text), FUN = nchar) 

sapply(a, max)

# twitter   blogs    news 
# 140    1306     901 




## Question 4:
# In the en_US twitter data set, 
# if you divide the number of lines where the word “love” (all lowercase) 
# occurs by the number of lines the word “hate” (all lowercase) occurs, 
# about what do you get?

# solution
love <- twitter_df %>% 
        filter(str_detect(tweet_text, "love")) %>% 
        nrow()

hate <- twitter_df %>% 
        filter(str_detect(tweet_text, "hate")) %>% 
        nrow()

love / hate
# [1] 4.664964



## Question 5:
# The one tweet in the en_US twitter data set that matches the word “biostats” says what?

# solution:
(bio <- twitter_df %>% 
        filter(str_detect(tweet_text, "biostats")))

# # A tibble: 1 x 2
# line_id tweet_text                                                               
# <chr>   <chr>                                                                    
# 1 556872  "i know how you feel i have biostats on tuesday and i have yet to study "


## Question 6:
# find 'a computer once beat me at chess, but it was no match for me at kickboxing' in the twitter dataset

# solution:
(chess <- twitter_df %>% 
        filter(str_detect(tweet_text, 
                          "a computer once beat me at chess but it was no match for me at kickboxing")))

# # A tibble: 3 x 2
# line_id tweet_text                                                               
# <chr>   <chr>                                                                    
# 1 519059  a computer once beat me at chess but it was no match for me at kickboxing
# 2 835824  a computer once beat me at chess but it was no match for me at kickboxing
# 3 2283423 a computer once beat me at chess but it was no match for me at kickboxing



# EDA ---------------------------------------------------------------------

## Week 2 Capstone Project: Exploratory Data Analysis

# we need to explore our swiftkey data
# understand the data to build a better prediction model
# how frequently to certain words appear?
# how to combinations of words that appear in the data?
# go into the data with a hypothesis as to what we are looking for
# without expectations - everything seems correct - we need to be aware as we are looking through it

# The first step in building a predictive model for text is understanding the distribution and relationship between the words,
# tokens, and phrases in the text. 
# The goal of this task is to understand the basic relationships you observe in the data and prepare to build
# your first linguistic models.

# Tasks to accomplish:

# Exploratory analysis:
# perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora.

# Understand frequencies of words and word pairs:
# build figures and tables to understand variation in the frequencies of words and word pairs in the data.


# Questions to consider:

# Some words are more frequent than others - what are the distributions of word frequencies?
# What are the frequencies of 2-grams and 3-grams in the dataset?
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
# How do you evaluate how many of the words come from foreign languages?
# Can you think of a way to increase the coverage?
# identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?


# combine all datasets to analyze different tokenization
(token_df <- bind_rows(twitter_df, blogs_df, news_df)) 

rm(twitter, twitter_df, blogs, blogs_df, news, news_df)


## single word analysis

# top words without stop words
token_df %>%
        tidytext::unnest_tokens(word, text) %>% 
        group_by(word) %>%
        anti_join(stop_words) %>% 
        summarize(count = n()) %>%
        mutate(freq = count / sum(.$count)) %>% 
        filter(count > 1) %>% 
        arrange(., -freq) %>% 
        top_n(25) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, freq)) %>% 
        ggplot(aes(x = as.factor(word), y = freq)) +
        geom_col(show.legend = F, fill = "dark blue") +
        coord_flip() +
        theme_few() +
        xlab("Word") + 
        ylab("Frequency") + 
        labs(title = "Top 25 Words by Frequency",
             subtitle = 'Excluding Stop Words') +
        scale_y_continuous(labels = scales::percent)

# top words with stop words
token_df %>%
        tidytext::unnest_tokens(word, text) %>% 
        group_by(word) %>%
        summarize(count = n()) %>%
        mutate(freq = count / sum(.$count)) %>% 
        filter(count > 1) %>% 
        arrange(., -freq) %>% 
        top_n(25) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, freq)) %>% 
        ggplot(aes(x = as.factor(word), y = freq)) +
        geom_col(show.legend = F, fill = "dark red") +
        coord_flip() +
        theme_few() +
        xlab("Word") + 
        ylab("Frequency") + 
        labs(title = "Top 25 Words by Frequency",
             subtitle = 'Including Stop Words') +
        scale_y_continuous(labels = scales::percent)


# tf_idf across each data set!
(tf_idf_df <- token_df %>% 
        tidytext::unnest_tokens(word, text) %>% 
        group_by(word, source) %>% 
        summarize(count = n()) %>% 
        bind_tf_idf(., term = word, document = source, n = count) %>% 
        arrange(desc(tf_idf)) %>% 
        ungroup())

# visualize tf_idf
tf_idf_df %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>% 
        group_by(source) %>% 
        top_n(25) %>% 
        ungroup() %>%
        ggplot(aes(word, tf_idf, fill = source)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~source, ncol = 2, scales = "free") +
        coord_flip() +
        theme_few() + 
        scale_y_continuous(labels = scales::percent) +
        xlab("Words") +
        ylab("TF-IDF") +
        labs(title = "Top 25 Words by TF-IDF",
             subtitle = "Compared across each source")




## n gram analysis

# top words without stop words
token_df %>%
        tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
        group_by(bigram) %>% 
        summarize(count = n()) %>% 
        mutate(freq = count / sum(.$count)) %>% 
        filter(count > 1) %>% 
        arrange(., -freq) %>% 
        top_n(15) %>% 
        ungroup() %>% 
        mutate(bigram = reorder(bigram, freq)) %>% 
        ggplot(aes(x = as.factor(bigram), y = freq)) +
        geom_col(show.legend = F, fill = "dark blue") +
        coord_flip() +
        theme_few() +
        xlab("Bigram") + 
        ylab("Frequency") + 
        labs(title = "Top 25 Bigrams by Frequency",
             subtitle = 'Including Stop Words') +
        scale_y_continuous(labels = scales::percent)

# tf_idf across each data set!
tf_idf_df <- token_df %>% 
                tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
                group_by(bigram, source) %>% 
                summarize(count = n()) %>% 
                bind_tf_idf(., term = bigram, document = source, n = count) %>% 
                arrange(desc(tf_idf)) %>% 
                ungroup()

## word cloud analysis

token_df %>%
        tidytext::unnest_tokens(word, text) %>% 
        group_by(word) %>%
        anti_join(stop_words) %>% 
        summarize(count = n()) %>% 
        with(wordcloud(word, count, max.words = 100))







# model -------------------------------------------------------------------

# The goal here is to build your first simple model for the relationship between words. 
# This is the first step in building a predictive text mining application. 
# You will explore simple models and discover more complicated modeling techniques.

# Tasks to accomplish:
 
# Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
# Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.

# Questions to consider:

# How can you efficiently store an n-gram model (think Markov Chains)?
# How can you use the knowledge about word frequencies to make your model smaller and more efficient?
# How many parameters do you need (i.e. how big is n in your n-gram model)?
# Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
#         How do you evaluate whether your model is any good?
#         How can you use backoff models to estimate the probability of unobserved n-grams?

# Hints, tips, and tricks:

# As you develop your prediction model, two key aspects that you will have to keep in mind are the size and runtime of the algorithm. These are defined as:
         
# Size: the amount of memory (physical RAM) required to run the model in R
# Runtime: The amount of time the algorithm takes to make a prediction given the acceptable input
# Your goal for this prediction model is to minimize both the size and runtime of the model in order to provide a reasonable experience to the user.
 
# Keep in mind that currently available predictive text models can run on mobile phones, which typically have limited memory and processing power compared to desktop computers. Therefore, you should consider very carefully (1) how much memory is being used by the objects in your workspace; and (2) how much time it is taking to run your model. Ultimately, your model will need to run in a Shiny app that runs on the shinyapps.io server.
 
# Tips, tricks, and hints:
 
# Here are a few tools that may be of use to you as you work on their algorithm:
         
# object.size(): this function reports the number of bytes that an R object occupies in memory
# Rprof(): this function runs the profiler in R that can be used to determine where bottlenecks in your function may exist. The profr package (available on CRAN) provides some additional tools for visualizing and summarizing profiling data.
# gc(): this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being used by R.
# There will likely be a tradeoff that you have to make in between size and runtime. For example, an algorithm that requires a lot of memory, may run faster, while a slower algorithm may require less memory. You will have to find the right balance between the two in order to provide a good experience to the user.

# back off modeling - storing n-grams?
# https://en.wikipedia.org/wiki/Katz%27s_back-off_model

# n grams examples
# https://en.wikipedia.org/wiki/N-gram


# The goal of this exercise is to build and evaluate your first predictive model. 
# You will use the n-gram and backoff models you built in previous tasks to build and evaluate your predictive model. 
# The goal is to make the model efficient and accurate.

# Tasks to accomplish:

# Build a predictive model based on the previous data modeling steps - you may combine the models in any way you think is appropriate.
# Evaluate the model for efficiency and accuracy - 
#         use timing software to evaluate the computational complexity of your model. 
# Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.

# Questions to consider:

# How does the model perform for different choices of the parameters and size of the model?
#         How much does the model slow down for the performance you gain?
#         Does perplexity correlate with the other measures of accuracy?
#         Can you reduce the size of the model (number of parameters) without reducing performance?

# https://rpubs.com/brianzive/textmining
# https://rpubs.com/BreaizhZut/MilesStone_NgramPrediction
# https://rstudio-pubs-static.s3.amazonaws.com/139244_a5629dbf418f465ab825c063f22535d5.html
# http://ssli.ee.washington.edu/WS07/notes/ngrams.pdf
# https://web.stanford.edu/~jurafsky/slp3/4.pdf
# https://rpubs.com/gikolev/192394
# https://github.com/gikolev/data_science_capstone/blob/master/model.R
# http://garonfolo.dk/herbert/2015/05/r-text-classification-using-a-k-nearest-neighbour-model
# https://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/
# https://www.r-bloggers.com/generate-text-using-markov-chains-sort-of

# plans for modeling: how others did it...
# build a dictionary for 1, 2, 3 and 4 - grams that are the most common in our corpus
# we will need to split these tables into columns by word (4-gram = 4 columns)
# each gram dictionary will be stored as a 'lookup' table
# to predict - search through the correct table (given words in string) and give the frequency of the most common n + 1 gram
# the algorithm should search "backwards" through the lookup tables using the available input strings...
# if no match throughout all lookup tables - give the most common 1 gram 

# plan for modeling: how can I do it?
# experiment with a fuzzy matching distance algorithm matching to the n + 1 gram lookup table
# for exmaple if input is 'I am a'
# fuzzy distance match 'I am a' into the 4-gram lookup table
# this will give us a percent match on a 4-gram matching the string 'I am a'


## develop test and training datasets

# define training and test split
inTrain <- sample_frac(token_df, .99) %>% as_tibble()

# split training dataset
ngram_train <- token_df %>% 
        filter(token_df$line_id %in% inTrain$line_id)

# split testining dataset
ngram_test <- token_df %>% 
        filter(!token_df$line_id %in% inTrain$line_id)



## develop n-gram lookup tables

# make a 'n' gram lookup table looping through the sequence 1:4
# result will be 4 tables each with a specific n gram (ex. table 4 = 4-gram table with frequency)
makeGrams <- function(df, grams) {
        
        ngram_list <- list()
        
        for (i in seq_along(grams)) {
                
                ngram_list[[i]] <- df %>% 
                        tidytext::unnest_tokens(ngram, text, token = "ngrams", n = i) %>% 
                        drop_na() %>% 
                        group_by(ngram) %>% 
                        summarize(count = n()) %>% 
                        filter(count > 1) %>% 
                        mutate(freq = count / sum(.$count)) %>% 
                        arrange(., -freq)
                
        }
        
        ngram_list <<- ngram_list
}



# apply make grams function to the training dataset
# result is our for n-gram lookup tables
makeGrams(df = ngram_train, grams = 1:4)

# seperate 1 gram tables
one_gram_table <- ngram_list[[1]]

# 2 gram table
two_gram_table <- ngram_list[[2]] %>% 
        separate(ngram, c('ngram_one', 'ngram_two'),
                 sep = " ", remove = F) 

# 3 gram table
three_gram_table <- ngram_list[[3]] %>% 
        separate(ngram, c('ngram_one', 'ngram_two', 'ngram_three'),
                 sep = " ", remove = F)

# four gram table
four_gram_table <- ngram_list[[4]] %>% 
        separate(ngram, c('ngram_one', 'ngram_two', 'ngram_three', 'ngram_four'),
                 sep = " ", remove = F)


## prediction function #1
# locate the correct n gram lookup table based on length
# compute the distance between the input string and all values in the lookup table
# extract the subset of strings in the lookup table closest to the input string (highest distance match)
# once we narrowed the space of close distance words down - choose the next word to be the word with the highest frequency
## this prediction function works well when the distance string can be accurately matched
# however this does not happen often as distance methods rely of movements to match - not exact character match
# algorithm was able to find matches of 2 and 3 gram strings and predict the new word nicely...
# but moving past 3 gram and staying at 1 gram leads to unpredictability of the next word prediction...
## predict the next word
predictGrams <- function(y) {
        
     library(stringdist)        

     # normalize the predict string                
     target_string <- cleanText(y)
     
     # grab the length to the input string to determine lookup table
     string_n <- str_count(target_string, ' ')
     
     if (string_n == 2) { # if there are two spaces we have 3 gram - string dist the 3 gram table
             
             ngram_df <- four_gram_table %>% 
                     mutate(jw = 1 - stringdist(a = target_string, b = .$ngram, # append distances
                                            method = 'jw', p = .1)) %>% 
                     top_n(3) %>% # get the top 3 results
                     arrange(-jw) %>% 
                     dplyr::select(ngram_four) # give the value of the next word in the string
             
             # print(ngram_df)
             
     } else if (string_n == 1) {# if there is one space we have 2 gram - string dist the 2 gram table
             
             ngram_df <- three_gram_table %>% 
                     
                     mutate(jw = 1 - stringdist(a = target_string, b = .$ngram, # append distances
                                                method = 'jw', p = .1)) %>%
                     top_n(3) %>% # get the top 3 results
                     filter(freq == max(freq)) %>% 
                     arrange(-jw) %>% 
                     dplyr::select(ngram_three) # give the value of the next word in the string
             
             # print(ngram_df)
             
             
     } else if (string_n == 0) { # if there are no spaces we have 1 gram - string dist the 1 gram table
             
             
             ngram_df <- two_gram_table %>% 
                     mutate(jw = stringdist(a = target_string, b = .$ngram, # append distances
                                                method = 'lcs')) %>% 
                     top_n(3) %>% # get the top 3 results
                     arrange(-jw) %>% 
                     dplyr::select(ngram_two) # give the value of the next word in the string
             
             # print(ngram_df)
             
     }


         print(ngram_df)
             
             
}
           

## prediction function #2
# find the right lookup table based on string input
# find the string that matches the input string exactly 
# if matched exactly provide the next word prediction as the word with the highest frequency
# if no match give the single word with the highest frequency
## Create predict function that takes text as input and predicts the next word
predGrams2 <- function(input_string) {
        
        # take in input string a format it 
        y <- cleanText(input_string) %>%  str_split(., " ") %>% as.data.frame()
        
        # get count of the input string to decide which lookup n gram table to use
        n <- length(y[[1]])
        
        
        if (n == 1) { # if our string is of length one...
                
                p <- two_gram_table %>% 
                        filter(ngram_one == y[1,]) %>% 
                        top_n(1, freq)
                
                print(p$ngram_two)
                
        } else if (n == 2) {
                
                p <- three_gram_table %>% 
                        filter(ngram_one == y[1,] & 
                                       ngram_two == y[2,]) %>% 
                        top_n(1, freq)
                
                print(p$ngram_three)
                
        } else if (n == 3) {
                
                p <- four_gram_table %>% 
                        filter(ngram_one == y[1,] & 
                                       ngram_two == y[2,] & 
                                       ngram_three == y[3,]) %>%
                        top_n(1, freq)
                
                print(p$ngram_four)

        } else {
                
                y_tail <- tail(y, 3)
                
                p <- four_gram_table %>% 
                        filter(ngram_one == y_tail[1,] & 
                                       ngram_two == y_tail[2,] & 
                                       ngram_three == y_tail[3,]) %>% 
                        top_n(1, freq)
                
                print(p$ngram_four)
                
        }
        
}


     
## Test predict function
predGrams2("I would like a cup of")
predGrams2("Nice to meet")
predGrams2("Crate and") # does not print need to update logic in n == 2 and greater statements
predGrams2("Peanut")     
predGrams2('I need a')     
     
     
     
     

# prediction function for shiny app -----------------------------------------------------------

# https://github.com/pmccullo/Coursera-Capstone-ShinyApp/blob/master/ui.R



# combine all datasets to analyze different tokenization
(token_df <- bind_rows(twitter_df, blogs_df, news_df)) 


## develop n-gram lookup tables

# make a 'n' gram lookup table looping through the sequence 1:4
# result will be 4 tables each with a specific n gram (ex. table 4 = 4-gram table with frequency)
makeGrams <- function(df, grams) {
        
        ngram_list <- list()
        
        for (i in seq_along(grams)) {
                
                ngram_list[[i]] <- df %>% 
                        tidytext::unnest_tokens(ngram, text, token = "ngrams", n = i) %>% 
                        drop_na() %>% 
                        group_by(ngram) %>% 
                        summarize(count = n()) %>% 
                        filter(count > 1) %>% 
                        mutate(freq = count / sum(.$count)) %>% 
                        arrange(., -freq)
                
        }
        
        ngram_list <<- ngram_list
}



# apply make grams function to the training dataset
# result is our for n-gram lookup tables
makeGrams(df = token_df, grams = 1:4)

# seperate 1 gram tables
one_gram_table <- ngram_list[[1]]

# 2 gram table
two_gram_table <- ngram_list[[2]] %>% 
        separate(ngram, c('ngram_one', 'ngram_two'),
                 sep = " ", remove = F) 

# 3 gram table
three_gram_table <- ngram_list[[3]] %>% 
        separate(ngram, c('ngram_one', 'ngram_two', 'ngram_three'),
                 sep = " ", remove = F)

# four gram table
four_gram_table <- ngram_list[[4]] %>% 
        separate(ngram, c('ngram_one', 'ngram_two', 'ngram_three', 'ngram_four'),
                 sep = " ", remove = F)     
     
     
grams_save <- list(one_gram_table, two_gram_table, three_gram_table, four_gram_table)  
             
save(one_gram_table, file = 'one_gram.rda')
save(two_gram_table, file =  'two_gram.rda')
save(three_gram_table, file = 'three_gram.rda')
save(four_gram_table, file =  'four_gram.rda')



# next word prediction algorithm
predText_zo <- function(input_string) {
        
        
        setwd("/Users/zacholivier/Desktop/R/text_mine_capstone/en_US")
        
        load(file = 'one_gram.rda') 
        load(file = 'two_gram.rda')
        load(file = 'three_gram.rda')
        load(file = 'four_gram.rda')
        
        
        
        
        # take in input string a format it 
        y <- cleanText(input_string) %>%  str_split(., " ") %>% as.data.frame()
        
        # get count of the input string to decide which lookup n gram table to use
        n <- length(y[[1]])
        
        
        if (n == 1) { # if our string is of length one...
                
                p <- two_gram_table %>% 
                        filter(ngram_one == y[1,]) %>% 
                        top_n(1, freq)
                
                print(p$ngram_two)
                
        } else if (n == 2) {
                
                p <- three_gram_table %>% 
                        filter(ngram_one == y[1,] & 
                                       ngram_two == y[2,]) %>% 
                        top_n(1, freq)
                
                print(p$ngram_three)
                
        } else if (n == 3) {
                
                p <- four_gram_table %>% 
                        filter(ngram_one == y[1,] & 
                                       ngram_two == y[2,] & 
                                       ngram_three == y[3,]) %>%
                        top_n(1, freq)
                
                print(p$ngram_four)
                
        } else {
                
                y_tail <- tail(y, 3)
                
                p <- four_gram_table %>% 
                        filter(ngram_one == y_tail[1,] & 
                                       ngram_two == y_tail[2,] & 
                                       ngram_three == y_tail[3,]) %>% 
                        top_n(1, freq)
                
                print(p$ngram_four)
                
        }
        
        
        
        
}        
        
        
        
        
        
        
        
        
        
        
        
        
        
        














































        
        
        





# creative exploration ----------------------------------------------------

# So far you have used basic models to understand and predict words. 
# In this next task, your goal is to use all the resources you have available to you 
# (from the Data Science Specialization, resources on the web, or your own creativity) 
# to improve the predictive accuracy while reducing computational runtime and model complexity (if you can). 
# Be sure to hold out a test set to evaluate the new, more creative models you are building.

# Tasks to accomplish:

# Explore new models and data to improve your predictive model.
# Evaluate your new predictions on both accuracy and efficiency.
# Questions to consider

# What are some alternative data sets you could consider using?
#         What are ways in which the n-gram model may be inefficient?
#         What are the most commonly missed n-grams? Can you think of a reason why they would be missed and fix that?
#         What are some other things that other people have tried to improve their model?
#         Can you estimate how uncertain you are about the words you are predicting?









## Set working directory
setwd("/Users/gik/Data_Analytics/R_Directory/coursera/Data_Science_Capstone/Exploratory_Analysis")

## Read blogs, news and twitter files into character vectors
blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

## Create test (10%) and train (90%) character vectors
set.seed(12345)
index <- sample(1:length(blogs), size = 0.1*length(blogs))
blogs_train <- blogs[index]
blogs_test <- blogs[-index]
index <- sample(1:length(news), size = 0.1*length(news))
news_train <- news[index]
news_test <- news[-index]
index <- sample(1:length(twitter), size = 0.1*length(twitter))
twitter_train <- twitter[index]
twitter_test <- twitter[-index]

## Create corpus
library(quanteda)
corpus_train <- corpus(c(blogs_train, news_train, twitter_train))

## Create list of profane words to filter out
profanity <- readLines("profanity_list_en.txt", encoding = "UTF-8", skipNul = TRUE)

## Create 1 through 4-gram document frequency matrices and extract frequency tables as data frames 
## and create separate columns for each word in the n-gram; remove n-grams occurring only once.
df_list <- list()
library(stringr)
library(dplyr)
strt <- Sys.time()
for(i in 1:4) {
        x <- dfm(corpus_train,
                 ngrams = i,
                 toLower = TRUE, 
                 removeNumbers = TRUE, 
                 removePunct = TRUE, 
                 removeSeparators = TRUE, 
                 ignoredFeatures = profanity, 
                 stem = FALSE,
                 verbose = FALSE)
        data <- as.data.frame(topfeatures(x, n = length(x)))
        colnames(data) <- "freq"
        data$ngram <- row.names(data)
        row.names(data) <- c(1:nrow(data))
        data$rank <- c(1:nrow(data))
        data$pct_total <- data$freq / sum(data$freq) * 100 
        data$pct_cumul <- cumsum(data$pct_total)
        data <- filter(data, freq > 1)
        df_list[[i]] <- data
        df_list[[i]] <- cbind(df_list[[i]],
                              as.data.frame(str_split_fixed(df_list[[i]]$ngram, "_", i)))
}
print(Sys.time()-strt)

## Assign character class to word columns
df_list[[1]]$V1 <- as.character(df_list[[1]]$V1)
df_list[[2]]$V1 <- as.character(df_list[[2]]$V1)
df_list[[2]]$V2 <- as.character(df_list[[2]]$V2)
df_list[[3]]$V1 <- as.character(df_list[[3]]$V1)
df_list[[3]]$V2 <- as.character(df_list[[3]]$V2)
df_list[[3]]$V3 <- as.character(df_list[[3]]$V3)
df_list[[4]]$V1 <- as.character(df_list[[4]]$V1)
df_list[[4]]$V2 <- as.character(df_list[[4]]$V2)
df_list[[4]]$V3 <- as.character(df_list[[4]]$V3)
df_list[[4]]$V4 <- as.character(df_list[[4]]$V4)

## Give names to the frequency tables
names(df_list) <- c("UnigramFreqTable",
                    "BigramFreqTable",
                    "TrigramFreqTable",
                    "QuadrigramFreqTable")

## Create predict function that takes text as input and predicts the next word
predict <- function(x) {
        y <- strsplit(tolower(x), " ")[[1]]
        n <- length(y)
        if (n == 1) {
                q <- which(df_list[[2]]$V1 == y)
                print(df_list[[2]][min(q), "V2"])
        } else if (n == 2) {
                q <- which(df_list[[3]]$V1 == y[1]
                           & df_list[[3]]$V2 == y[2])
                if (length(q) == 0) {
                        q <- which(df_list[[2]]$V1 == y[2])
                        print(df_list[[2]][min(q), "V2"])
                } else {
                        print(df_list[[3]][min(q), "V3"])
                }
        } else if (n >= 3) {
                z <- tail(y, n = 3) 
                q <- which(df_list[[4]]$V1 == z[1] 
                           & df_list[[4]]$V2 == z[2] 
                           & df_list[[4]]$V3 == z[3])
                if (length(q) == 0) {
                        q <- which(df_list[[3]]$V1 == z[2]
                                   & df_list[[3]]$V2 == z[3])
                        if (is.null(q)) {
                                q <- which(df_list[[2]]$V1 == z[3])
                                print(df_list[[2]][min(q), "V2"])
                        } else {
                                print(df_list[[3]][min(q), "V3"])
                        }
                } else {
                        print(df_list[[4]][min(q), "V4"])
                }
        }
}

## Test predict function
predict("I would like a cup of")
predict("Nice to meet")
predict("Crate and")
predict("Peanut")















library(tm);

makeCorpus <- function(d) {
        vectorSource <- c()
        files <- list.files(d)
        for (file in files) {
                fileFullPath <- paste(d, file, sep = filePathSep)
                vectorSource <- c(vectorSource, readLines(fileFullPath))
        }
        ovid <- Corpus(VectorSource(vectorSource))
        ovid
}

removeNonAscii <- content_transformer(function(input) { 
        iconv(input, "latin1", "ASCII", sub="")
})

removeExplicitPunctuation <- content_transformer(function(input) {
        output <- gsub("[[:punct:]]", " ", input)
        output
})

removeLeadingEndingWhiteSpaces <- content_transformer(function(input) {
        output <- gsub("^\\s+", "", input)
        output <- gsub("\\s+$", "", output)
        output
})

transformCorpus <- function(corpus) {
        corpus <- tm_map(corpus, removeNonAscii)
        corpus <- tm_map(corpus, removeExplicitPunctuation)
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        # corpus <- tm_map(corpus, removeWords, stopwords("english"))
        # corpus <- tm_map(corpus, stemDocument, mc.cores = 3) # E.g. running and run may have different linguistic context
        profanityRdata <- "profanity.RData"
        if (file.exists(profanityRdata)) {
                if (!exists("profanity")) {
                        load(profanityRdata)
                }
                corpus <- tm_map(corpus, removeWords, profanity)
        }
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, PlainTextDocument)
        corpus <- tm_map(corpus, removeLeadingEndingWhiteSpaces)
        corpus
}

preprocessText <- function(text) {
        as.character(transformCorpus(Corpus(VectorSource(text)))[[1]])
}

tagDocumentWithId <- function(corpus) {
        for(i in c(1 : length(corpus))) {
                DublinCore(corpus[[i]], "id") <- i
        }
        corpus
}

source("./week3-constructCorpus.R")
library("markovchain")

predictFollowingWord <- function(model, input, numberOfOutcome) {
        inputString <- input
        inputStringParts <- strsplit(inputString, " ")[[1]]
        inputStringLength <- length(inputStringParts)
        dictionary <- states(model)
        
        getRandomIndex <- function (len) (len * runif(1)) + 1
        getRandomWord <- function (len, dictionary) dictionary[getRandomIndex(len)]
        
        currentState <- NULL
        nextState <- NULL
        cache <- list()
        cache$stateHistory <- c()
        
        currentState <- inputStringParts[1]
        # print(paste("first word:", currentState))
        if (!currentState %in% dictionary) 
                currentState <- getRandomWord(inputStringLength, dictionary)
        
        # print(paste("check dictionary:", currentState))
        cache$stateHistory  <- c(cache$stateHistory, currentState)
        
        remainingInputStringParts <- inputStringParts[2:inputStringLength]
        
        for (remainingInputString in remainingInputStringParts) {
                nextState <- remainingInputString
                # print(paste("next word:", nextState))
                if (!nextState %in% dictionary) {
                        nextPossibilities <- conditionalDistribution(model, currentState)
                        nextStates <- dictionary[which.max(nextPossibilities)]
                        if (length(nextStates) > 0) 
                                nextState <- nextStates[getRandomIndex(length(nextStates))]
                        else
                                warning("Unable to find next state in model")
                }
                
                currentState <- nextState
                
                cache$stateHistory  <- c(cache$stateHistory, currentState)
        }
        
        cache$conditionalProbabilities <- 
                sort(conditionalDistribution(model, currentState),
                     decreasing = TRUE)[1:numberOfOutcome]
        
        cache
}

preprocessInputText <- function(inputText) {
        corpus <- Corpus(VectorSource(inputText))
        corpus <- transformCorpus(corpus)
        return(as.character(corpus[[1]]))
}

test <- function() {
        library(markovchain)
        load("./dormantroot/transitionMatrix.RData");
        markovChainModel <- new("markovchain", transitionMatrix = transitionMatrix)
        # save(markovChainModel, file = "markovChainModel")
        predictedWords <- predictFollowingWord(markovChainModel, preprocessInputText("jokingly wished the two could"), 4)
        colnames(t(as.matrix(predictedWords$conditionalProbabilities)))
}





# Method 2: applying different string matching methods
#osa Optimal string aligment, (restricted Damerau-Levenshtein distance).
#lv Levenshtein distance (as in R’s native adist).
#dl Full Damerau-Levenshtein distance.
#hamming Hamming distance (a and b must have same nr of characters).
#lcs Longest common substring distance.
#qgram q-gram distance.
#cosine cosine distance between q-gram profiles
#jaccard Jaccard distance between q-gram profiles
#jw Jaro, or Jaro-Winker distance.

#install.packages('stringdist')
library(stringdist)

distance.methods<-c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw')
dist.methods<-list()
for(m in 1:length(distance.methods))
{
        dist.name.enh<-matrix(NA, ncol = length(source2.devices$name),nrow = length(source1.devices$name))
        for(i in 1:length(source2.devices$name)) {
                for(j in 1:length(source1.devices$name)) { 
                        dist.name.enh[j,i]<-stringdist(tolower(source2.devices[i,]$name),tolower(source1.devices[j,]$name),method = distance.methods[m])      
                        #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
                }  
        }
        dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
        
        dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
        min.name.enh<-apply(dist.matrix, 1, base::min)
        for(i in 1:nrow(dist.matrix))
        {
                s2.i<-match(min.name.enh[i],dist.matrix[i,])
                s1.i<-i
                match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,]$name, s1name=source1.devices[s1.i,]$name, adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
        }
}
# Let's have a look at the results
library(reshape2)
matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix)




# data product ------------------------------------------------------------

# The goal of this exercise is to create a product to highlight the prediction algorithm that you have built
# and to provide an interface that can be accessed by others via a Shiny app..

# Tasks to accomplish:

# Create a data product to show off your prediction algorithm You should create a Shiny app that accepts an n-gram 
# and predicts the next word.

# Questions to consider:

# What are the most interesting ways you could show off your algorithm?
#         Are there any data visualizations you think might be helpful 
#         (look at the Swiftkey data dashboard if you have it loaded on your phone)?
#         How should you document the use of your data product (separately from how you created it) 
#         so that others can rapidly deploy your algorithm?
        
# Tips, tricks, and hints:

# Consider the size of the predictive model you have developed. 
# You may have to sacrifice some accuracy to have a fast enough/small enough model to load into Shiny.



# slide deck --------------------------------------------------------------

# The goal of this exercise is to "pitch" your data product to your boss or an investor.

# The slide deck is constrained to be 5 slides or less and should: 
#         (1) explain how your model works, 
#         (2) describe its predictive performance quantitatively and 
#         (3) show off the app and how it works.

# Tasks to accomplish:

# Create a slide deck promoting your product. Write 5 slides using RStudio Presenter explaining your product and why it is awesome!
        
# Questions to consider:

        # How can you briefly explain how your predictive model works?
        # How can you succinctly quantitatively summarize the performance of your prediction algorithm?
        # How can you show the user how the product works?
        
# Tips, tricks, and hints:

# The Rstudio presentation information is available here
# (https://support.rstudio.com/hc/en-us/articles/200486468-Authoring-R-Presentations).




