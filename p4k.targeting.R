#install.packages("RSQLite") #perhaps needed
library("RSQLite"); library(tidyverse); library(ggthemes)

setwd("/Users/zacholivier/Desktop/R/p4k")

# connect to the sqlite file
sqlite    <- dbDriver("SQLite")
p4k.db <- dbConnect(sqlite,"database.sqlite")

# look at the tables available
dbListTables(p4k.db)
# [1] "artists" "content" "genres"  "labels"  "reviews" "years"


# read in the tables into R
artists <- dbReadTable(conn = p4k.db, name = "artists")
content <- dbReadTable(conn = p4k.db, name = "content")
genres <- dbReadTable(conn = p4k.db, name = "genres")
reviews <- dbReadTable(conn = p4k.db, name = "reviews")
years <- dbReadTable(conn = p4k.db, name = "reviews")

str(years); str(reviews); str(genres); dbDisconnect(p4k.db)



p4k.df <- left_join(reviews, genres) %>%
        left_join(., years) %>% 
        dplyr::select(., -url, -pub_weekday:-pub_year, -author_type) %>% 
        mutate(pub_date = lubridate::ymd(pub_date),
               genre = factor(genre),
               log.score = log(score)) %>% 
        filter(genre != "NA",
               score > 2.5,
               pub_date > as.Date("2010-01-01"))

str(p4k.df)
rm(artists, content, genres, reviews, years)

qplot(x = p4k.df$score, y = p4k.df$genre)

fit1 <- lm(score ~ genre + pub_date + genre*pub_date - 1, data = p4k.df)

summary(fit1)
# Call:
#         lm(formula = score ~ genre + pub_date + genre * pub_date - 1, 
#            data = p4k.df)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
# -7.0320 -0.5995  0.2254  0.8507  3.1751 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# genreelectronic             6.646e+00  1.776e-01  37.413  < 2e-16 ***
# genreexperimental           7.317e+00  2.603e-01  28.107  < 2e-16 ***
# genrefolk/country           6.753e+00  4.500e-01  15.007  < 2e-16 ***
# genreglobal                 7.524e+00  7.712e-01   9.756  < 2e-16 ***
# genrejazz                   6.464e+00  4.818e-01  13.419  < 2e-16 ***
# genremetal                  4.592e+00  3.741e-01  12.277  < 2e-16 ***
# genrepop/r&b                5.414e+00  3.124e-01  17.327  < 2e-16 ***
# genrerap                    7.072e+00  3.013e-01  23.469  < 2e-16 ***
# genrerock                   6.461e+00  1.174e-01  55.030  < 2e-16 ***
# pub_date                    1.933e-05  1.241e-05   1.557 0.119517    
# genreexperimental:pub_date -1.796e-05  2.211e-05  -0.812 0.416704    
# genrefolk/country:pub_date  1.224e-05  3.379e-05   0.362 0.717199    
# genreglobal:pub_date       -2.644e-05  5.478e-05  -0.483 0.629405    
# genrejazz:pub_date          4.205e-05  3.707e-05   1.134 0.256631    
# genremetal:pub_date         1.462e-04  2.884e-05   5.068 4.05e-07 ***
# genrepop/r&b:pub_date       8.141e-05  2.464e-05   3.304 0.000954 ***
# genrerap:pub_date          -3.092e-05  2.372e-05  -1.303 0.192430    
# genrerock:pub_date          1.494e-05  1.494e-05   1.000 0.317200    
# ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.305 on 20313 degrees of freedom
# Multiple R-squared:  0.9664,	Adjusted R-squared:  0.9663 
# F-statistic: 3.243e+04 on 18 and 20313 DF,  p-value: < 2.2e-16



# plot results ------------------------------------------------------------
library(ggthemes)

ggplot(data = p4k.df) +
        geom_jitter(aes(x = pub_date, y = score, color = genre), alpha = .1, pch = 21) +
        stat_smooth(aes(x = pub_date, y = score, color = genre), se = F) +
        theme_few() +
        labs(title = "Pitchfork Review Scores by Year") +
        xlab("") +
        ylab("Review Score")






























