
pacman::p_load(tidyverse, tm, stringr,NLP, RColorBrewer, wordcloud, Matrix, recommenderlab, slam, data.table)

setwd('C:/Users/nchandra/OneDrive - National University of Singapore/CARecSys')

wine <- read.csv('./Data/wine-reviews/winemag-data-130k-v2.csv', na.strings = c("","NA"), stringsAsFactors = FALSE)

head(wine,2)

str(wine)

summary(wine)

wine[!complete.cases(wine),]

#percentage of rows with missing value
nrow(wine[!complete.cases(wine),])/nrow(wine)

# if we narrow down to check only completeness of "description, designation, taser_name, points, price, variety, country, province"
nrow(wine[complete.cases(wine[c('description','designation', 'taster_name', 'points', 'price','variety', 'country', 'province')]),])/nrow(wine)

# Cleaned up df
wine_clean <- wine[complete.cases(wine[c('description','designation', 'taster_name', 'points', 'price','variety', 'country', 'province')]),]

dim(wine)

dim(wine_clean)

# check any missing twitter handle
sum(is.na(wine_clean$taster_twitter_handle))

wine_clean[is.na(wine_clean$taster_twitter_handle),]

# since some of the twitter_handle may be missing, taster_name will be used as user identifier
sum(is.na(wine_clean$taster_name))

#check missing product after clean up
sum(is.null(wine_clean$designation))

#check missing rating
sum(is.na(wine_clean$points))

# check missing price
sum(is.na(wine_clean$price))

sum(!complete.cases(wine_clean$designation))

#pre-processing
wine_clean %>%
    #convert taster_twitter_handle and taster_name to factor
    mutate(taster_twitter_handle = as.factor(taster_twitter_handle)) %>%
    mutate(taster_name = as.factor(taster_name)) %>%
    #convert variety and winery to factor
    mutate(variety = as.factor(variety)) %>%
    mutate(winery = as.factor(winery)) %>%
    #convert country as factor
    mutate(country = as.factor(country))

wine_clean %>%
    group_by(taster_name) %>%
    summarize(count = n()) %>%
    arrange(-count)

table(wine_clean$variety)

wine_clean %>%
    group_by(variety) %>%
    summarize(rev_count = n()) %>%
    filter(rev_count > 100) %>%
    arrange(-rev_count) %>%
    ggplot(aes(x = reorder(as.factor(variety),rev_count), y = rev_count)) + geom_bar(stat = 'identity') + coord_flip() +
    labs(x = 'Wine Variety', 
         y = 'Review Count', 
         title = "Review Count for Wine variety")

# review count for each wine designation
wine_clean %>%
    group_by(designation) %>%
    summarize(rev_count = n()) %>%
    filter(rev_count > 100) %>%
    arrange(-rev_count) %>%
    ggplot(aes(x = reorder(as.factor(designation),rev_count), y = rev_count)) + geom_bar(stat = 'identity') + coord_flip() +
    labs(x = 'Wine Variety', 
         y = 'Review Count', 
         title = "Review Count for Wine")

wine_clean$title[1]

#number of unique wine product
length(unique(wine_clean$designation))

#Item Based Collaborative Filtering

# will be using taster_twitter_handler as userid, designation as item, points as the rating
wine_cf <- wine_clean[,c('X', 'taster_name', 'designation', 'points')]

head(wine_cf)

wine_cf_matrix <- dcast(wine_cf, taster_name ~ designation, fun.aggregate = mean,value.var = 'points',fill=0)

rownames(wine_cf_matrix) = wine_cf_matrix[,1]

wine_cf_matrix[,1] = NULL

dim(wine_cf_matrix)

wine_cf[(wine_cf$taster_name == 'Michael Schachner'& wine_cf$designation == '347 Vineyards'),]

wine_cf_matrix[1:19,250:300]



wine$description[1:5]

mystopwords = c(stopwords('english'), 'the', 'and', 'wine')

desc_corpus <- VCorpus(VectorSource(wine$description))

desc_corpus <- tm_map(desc_corpus, content_transformer(tolower))

desc_corpus <- tm_map(desc_corpus, removeNumbers)

desc_corpus <- tm_map(desc_corpus, removeWords, c(stopwords('english'),'the', 'and', 'wine'))
desc_corpus <- tm_map(desc_corpus, removePunctuation)
desc_corpus <- tm_map(desc_corpus, stemDocument)
desc_corpus <- tm_map(desc_corpus, removeWords, stopwords('english'))
desc_corpus <- tm_map(desc_corpus, stripWhitespace)

for(i in 1:6){
    print(desc_corpus[[i]][1])
}

desc_dtm <- DocumentTermMatrix(desc_corpus, control = list(weighting = weightTfIdf))

desc_dtm

desc_dtm = removeSparseTerms(desc_dtm, 0.90)

desc_dtm

freq = data.frame(sort(colSums(as.matrix(desc_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

require('slam')

sim_mat_cos <- crossprod_simple_triplet_matrix(t(desc_dtm))/(sqrt(col_sums(t(desc_dtm)^2) %*% t(col_sums(t(desc_dtm)^2))))


