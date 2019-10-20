# CA2 for Text Analytics and Recommender Systems
# Goal is to design, build and test a recommender system using public domain (or other) data
# demonstrate at least two different approaches/methods
# must include at least: 
# a. content-based approach, in which you utilise text analytics methods
#       to extract suitable product features from text-based product descriptions
# b. collaborative filtering approach either using explicit or implicity feedback
# c. a hybrid apporach combining the above mthods


# a. content-based approach

# library
library(dplyr)
library(ggplot2)

# ! Set your working directory
setwd("C:/Users/zamsg/Documents/GitHub/CARecSys/Data")

# Read in data
hotels <- read.csv("Booking.csv", stringsAsFactor=FALSE)

# Understand the data
names(hotels)
names(hotels)[1] <- "hotel_name"
length(unique(hotels$hotel_name)) # 24 hotels
length(unique(hotels$reviewer_country)) # 118 reviewer_country

# Data Preparation (intial)
hotels$review_date <- substring(hotels$review_date, 11)
hotels$review_date <- as.Date(strptime(hotels$review_date, format = "%d %B %Y"))
hotels$review_header <- substring(hotels$review_header, first = 4, last = nchar(hotels$review_header)-3)
hotels$stay <- as.Date(strptime((paste((substring(hotels$stay, 11)), "01")), format = "%B %Y %d"))
sapply(hotels, class)

# Data explorary
barplot(sort(table(hotels$hotel_name)), main ="Reviews for each Hotels", ylab = "No. of Reviews", las = 2)

# can add in explorary on reivew_date

hist(hotels$reviewer_rating, breaks = seq(0, 10, by = 1), 
     main = "Distribution of viewer Rating", xlab = "reviewer rating")

table(hotels$review_count) # positive skewed
hist(hotels$review_count, breaks = 20,
     main = "No. of Review Counts of the Reviewers", xlab = "review counts") # majority reviewers have less than 10 reviews

# can add in explorary on stay 

hotels %>%
  group_by(hotel_name) %>%
  summarise(overall_score = mean(overall_score)) %>%
  ggplot(aes(x = reorder(as.factor(hotel_name), overall_score), y = overall_score)) + geom_bar(stat = 'identity') +
  labs(x = "hotel_name", title = "Overall Score for Singapore Hotels in Little India") +
  coord_flip()
# can explore over scores;clean_score, comfort_score, loc_score, facilities_score, staff_score, value_score, wifi_score