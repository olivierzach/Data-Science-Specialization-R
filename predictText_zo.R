
# ZO Next Word Text Prediction Algorithm ------------------------------------------------------

# function predicts the next word in a string based on a heavy corpus
# corpus includes Twitter, News articles, and Blog articles
# function will load workflow including the text prediction frequency tables
# function will clean, split, and format the input string
# function will apply the text prediction algorithm and produce a prediction


# text prediction function --------------------------------------------------------------------

# next word prediction algorithm
predText_zo <- function(input_string) {
        
        # library(stringdist)

        # take in input string a format it 
        y <- cleanText(input_string) %>%
                str_split(., " ") %>%
                as.data.frame()
        
        a <- character(0)
        
        y_tail <- tail(y, 3) %>% as.data.frame() %>% mutate_all(funs(as.character))
        
                
        p <- four_gram_table %>% 
                filter(
                        ngram_one == y_tail[1,] & 
                        ngram_two == y_tail[2,] & 
                        ngram_three == y_tail[3,]      
                ) %>% 
                top_n(1, freq) %>% 
                arrange(-freq) %>% 
                filter(row_number() %in% 1)
        
        p_print <- data.frame(p$ngram_four, stringsAsFactors = F)
                
                
        if (is.na(p_print[1,])) {
                
                y_tail <- tail(y,2) %>% as.data.frame() %>% mutate_all(funs(as.character))
                
                p3 <- three_gram_table %>% 
                        filter(ngram_one == y_tail[1,] & 
                                       ngram_two == y_tail[2,]) %>% 
                        top_n(1, freq) %>% 
                        arrange(-freq) %>% 
                        filter(row_number() %in% 1)
                
               p_print = data.frame(p3$ngram_three, stringsAsFactors = F)
                
        } else {
                
                p_print = p_print
        }
        
        
        
        
        if (is.na(p_print[1,])) {
                
                y_tail <- tail(y,1) %>% as.data.frame() %>% mutate_all(funs(as.character))
                
                p2 <- two_gram_table %>% 
                        filter(ngram_one == y_tail[1,]) %>% 
                        top_n(1, freq) %>% 
                        arrange(-freq) %>% 
                        filter(row_number() %in% 1)
                
                p_print = data.frame(p2$ngram_two, stringsAsFactors = F)
                
        } else {
                
                p_print = p_print
        }
        
        
        
        if (is.na(p_print[1,])) {
                
                y_tail <- tail(y, 3) %>% as.data.frame() %>% mutate_all(funs(as.character))
                
                y_string <- paste(y_tail[1,], y_tail[2,], y_tail[3,])
                
                
                p_dist <- four_gram_table %>% 
                        mutate(
                                jw = 1 - stringdist(
                                        a = y_string,
                                        b = .$ngram, 
                                        method = 'jw',
                                        p = .1
                                )      
                        ) %>% 
                        top_n(5) %>% # get the top 3 results
                        arrange(-freq) %>% 
                        filter(row_number() %in% 1) %>% 
                        dplyr::select(ngram_four)
                
               p_print = data.frame(p_dist$ngram_four, stringsAsFactors = F)
                
        } else {
                
                p_print = p_print
        }
                
        p_print

}   
                
                
                
                
                
                
                
                
                
                
                
                
                
     


