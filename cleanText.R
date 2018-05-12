# function to clean text using tm
cleanText <- function(x) {
        
        # install needed packages
        pacman::p_load(tidyverse, tidytext)
        
        x %>% 
                str_replace_all("[^[:graph:]]", " ") %>% 
                removeNumbers() %>% 
                removePunctuation() %>% 
                tolower() %>% 
                removeWords(., profanity$word)
}