# CA2 for Text Analytics and Recommender Systems
# Goal is to design, build and test a recommender system using public domain (or other) data
# demonstrate at least two different approaches/methods
# must include at least: 
# a. content-based approach, in which you utilise text analytics methods
#       to extract suitable product features from text-based product descriptions
# b. collaborative filtering approach either using explicit or implicity feedback
# c. a hybrid apporach combining the above mthods

pacman::p_load(tidyverse, tm, stringr,NLP, RColorBrewer, wordcloud, Matrix, recommenderlab, slam, data.table)

setwd("C:/Users/zamsg/Documents/GitHub/CARecSys/Data/wine-reviews")

wine <- read.csv('winemag-data-130k-v2.csv', na.strings = c("","NA"), stringsAsFactors = FALSE)
colnames(wine)
head(wine,2)
# columns: " ", country, description, points, price, province, region_1, region_2, taster_name, taster_twitter_handle, title, variety, winery, 

dim(wine) # 129971 rows, 14 columns

str(wine)

summary(wine)

wine[!complete.cases(wine),]

#percentage of rows with missing value
nrow(wine[!complete.cases(wine),])/nrow(wine) #82.7%

# if we narrow down to check only completeness of "description, designation, taser_name, points, price, variety, country, province"
nrow(wine[complete.cases(wine[c('description','designation', 'taster_name', 'points', 'price','variety', 'country', 'province')]),])/nrow(wine) # 53.8% missing data

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

wine_clean<-subset(wine_clean,wine_clean$country=="France") # narrowed down from 69,919 rows to 11655 rows

# exploration
wine_clean %>%
    group_by(taster_name) %>%
    summarize(count = n()) %>%
    arrange(-count)

table(wine_clean$variety)

# review count for wine variety, top 500
wine_clean %>%
    group_by(variety) %>%
    summarize(rev_count = n()) %>%
    filter(rev_count > 200) %>%
    arrange(-rev_count) %>%
    ggplot(aes(x = reorder(as.factor(variety),rev_count), y = rev_count)) + geom_bar(stat = 'identity') + coord_flip() +
    labs(x = 'Wine Variety', 
         y = 'Review Count', 
         title = "Review Count for Wine variety")

# review count for each wine designation
wine_clean %>%
    group_by(designation) %>%
    summarize(rev_count = n()) %>%
    filter(rev_count > 20) %>%
    arrange(-rev_count) %>%
    ggplot(aes(x = reorder(as.factor(designation),rev_count), y = rev_count)) + geom_bar(stat = 'identity') + coord_flip() +
    labs(x = 'Wine Variety', 
         y = 'Review Count', 
         title = "Review Count for Wine")

wine_clean$title[1]

write.csv(wine_clean,file="wine_clean.csv")



#Item Based Collaborative Filtering

# will be using taster_twitter_handler as userid, designation as item, points as the rating
wine_cf <- wine_clean[,c('X', 'taster_name', 'designation', 'points')]

head(wine_cf)

# convert data between wide and long format
wine_cf_matrix <- dcast(wine_cf, taster_name ~ designation, fun.aggregate = mean, value.var = 'points',fill=0)

rownames(wine_cf_matrix) = wine_cf_matrix[,1]

wine_cf_matrix[,1] = NULL

dim(wine_cf_matrix) # 19 rows, 30337 cols

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


# Content based

# using designation and description
wine_cb <- wine_clean[, c('designation', 'description', 'winery', 'variety')]
head(wine_cb)

dim(wine_cb) # 11,655 rows

any(duplicated(wine_cb$description)) # there are duplicate wine
which(duplicated(wine_cb$description)) # there are duplicate wine
length(which(duplicated(wine_cb$description))) # 1003 rows

length(which(duplicated(wine_cb[, c("description", "designation")]))) # 1003 rows
# this means that both description and designation are duplicated 

wine_cb_unique <- (wine_cb[!duplicated(wine_cb[,"description"]),]) # remove duplicates
dim(wine_cb_unique) # 10,652 rows

# distribution of review
wine_cb_unique %>%
    group_by(designation) %>%
    summarize(rev_count = n()) %>%
    arrange(-rev_count)

# exploration on the number of words for description
doclen <- sapply(wine_cb_unique$description, function(x) length(strsplit(x, " ")[[1]]))
hist(doclen)
doclen[1]

#remove non-ASCII characters
wine_cb_unique$description <- iconv(wine_cb_unique$description, "UTF-8", "ASCII",sub='')

#Preprocessing the text...
corpus <- VCorpus(VectorSource(wine_cb_unique$description))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 'the', 'and', 'wine'))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 'the', 'and', 'wine'))
corpus <- tm_map(corpus, stripWhitespace)

# check
for(i in 1:5){
    print(corpus[[i]][1])
} 

#creating the matrix
dtm <- DocumentTermMatrix(corpus)
dtm_ti <- weightTfIdf(dtm)
dtm_ti

# cosine similarity
sim_mat_cos <- crossprod_simple_triplet_matrix(t(dtm_ti))/(sqrt(col_sums(t(dtm_ti)^2) %*% t(col_sums(t(dtm_ti)^2))))
result <- sort(sim_mat_cos[, 1], decreasing = T)[1:5] # reference movie id 1
result # top 5 similar wine based on description

#get the index of the most similar movies in 'result'
result_i <- as.integer(names(result))
head(result_i)

#print out for viewing/checking
for (i in 1:5) {
    cat(paste0("\n",i, "- ", result_i[i], " <title>", wine_cb_unique[result_i[i], 1], "\n<overview>", wine_cb_unique[result_i[i], 2]))
}


# Content Based continued
# some users tend to prefer wine from the same winery or same variety

wine_cb_unique <- within(wine_cb_unique, description_winery_variety <- paste(description, winery, variety, sep=" "))
head(wine_cb_unique)

#remove non-ASCII characters
wine_cb_unique$description_winery_variety <- iconv(wine_cb_unique$description_winery_variety, "UTF-8", "ASCII",sub='')

#Preprocessing the text...
corpus <- VCorpus(VectorSource(wine_cb_unique$description_winery_variety))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 'the', 'and', 'wine'))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 'the', 'and', 'wine'))
corpus <- tm_map(corpus, stripWhitespace)

# check
for(i in 1:5){
    print(corpus[[i]][1])
} 

#creating the matrix
dtm <- DocumentTermMatrix(corpus)
dtm_ti <- weightTfIdf(dtm)
dtm_ti

# cosine similarity
sim_mat_cos <- crossprod_simple_triplet_matrix(t(dtm_ti))/(sqrt(col_sums(t(dtm_ti)^2) %*% t(col_sums(t(dtm_ti)^2))))
result <- sort(sim_mat_cos[, 1], decreasing = T)[1:5] # reference movie id 1
result # top 5 similar wine based on description & winery

#get the index of the most similar movies in 'result'
result_ii <- as.integer(names(result))
head(result_ii)
head(result_i) # compare with previous

#print out for viewing/checking
for (i in 1:5) {
    cat(paste0("\n",i, "- ", result_ii[i], " <title> ", wine_cb_unique[result_i[i], 1], "\n<overview> ", wine_cb_unique[result_ii[i], 2],
               "\n<winery> ", wine_cb_unique[result_ii[i], 3], "\n<variety> ", wine_cb_unique[result_ii[i], 4]))
}

# clear that the recommender system took winery and wine variety into consideration.