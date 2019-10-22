
pacman::p_load(tidyverse, tm, stringr,NLP, RColorBrewer, wordcloud, Matrix, recommenderlab, slam, data.table)

setwd('C:/Users/nchandra/OneDrive - National University of Singapore/CARecSys')

wine <- read.csv('./Data/wine-reviews/winemag-data-130k-v2.csv', na.strings = c("","NA"), stringsAsFactors = FALSE)

head(wine,2)

str(wine)

summary(wine)

wine <- select(wine, -c('X'))

dim(wine)

# Remove duplicate records
wine_nodup <- distinct(wine)

dim(wine_nodup)

wine_nodup[!complete.cases(wine_nodup),]

#percentage of rows with missing value
nrow(wine_nodup[!complete.cases(wine_nodup),])/nrow(wine_nodup)

# if we narrow down to check only completeness of "description, designation, taser_name, points, price, variety, country, province"
nrow(wine_nodup[complete.cases(wine_nodup[c('description','designation', 'taster_name', 'points', 'price','variety', 'country', 'province')]),])/nrow(wine_nodup)

# new dataframe after removing NAs from description, designation, taster_name, points, price, variety, country, province
wine_clean <- wine_nodup[complete.cases(wine_nodup[c('description','designation', 'taster_name', 'points', 'price','variety', 'country', 'province')]),]

# check number of duplicated records in wine_clean
sum(duplicated(wine_clean))

#percentage of data left after the preprocessing
nrow(wine_clean)/nrow(wine)

dim(wine_clean)

# check any missing twitter handle
sum(is.na(wine_clean$taster_twitter_handle))

wine_clean[is.na(wine_clean$taster_twitter_handle),]

# since some of the twitter_handle may be missing, taster_name will be used as user identifier
sum(is.na(wine_clean$taster_name))

#check missing product title after clean up
sum(is.null(wine_clean$title))

#check missing rating
sum(is.na(wine_clean$points))

# check missing price
sum(is.na(wine_clean$price))

sum(!complete.cases(wine_clean$designation))

#convert data type
wine_clean <- wine_clean %>%
    #convert taster_twitter_handle and taster_name to factor
    mutate(taster_twitter_handle = as.factor(taster_twitter_handle)) %>%
    mutate(taster_name = as.factor(taster_name)) %>%
    #convert variety and winery to factor
    mutate(variety = as.factor(variety)) %>%
    mutate(winery = as.factor(winery)) %>%
    #convert country as factor
    mutate(country = as.factor(country))

head(wine_clean)

# assign tasterID and wineID as unique identifier
wine_clean <- wine_clean %>%
    mutate(tasterID = group_indices(.,taster_name), wineID = group_indices(.,title))

wine_clean %>%
    group_by(taster_name) %>%
    summarize(count = n()) %>%
    arrange(-count)

wine_clean %>%
    group_by(variety) %>%
    summarize(rev_count = n()) %>%
    arrange(-rev_count) %>%
    head(15) %>%
    ggplot(aes(x = reorder(as.factor(variety),rev_count), y = rev_count)) + geom_bar(stat = 'identity') + coord_flip() +
    labs(x = 'Wine Variety', 
         y = 'Review Count', 
         title = "Review Count for top 15 Wine variety")

# get mean point for each product
mean_score <- ungroup(wine_clean) %>%
    group_by(title) %>%
    summarize(mean_points = mean(points)) %>%
    arrange(desc(mean_points))

ggplot(mean_score, aes(mean_points)) + geom_histogram(binwidth = 1, alpha = 0.5, position = 'identity') +
labs(title = 'Distribution of Mean Points')

wine_clean %>%
    group_by(variety) %>%
    summarize(mean_points = mean(points), median_points = median(points), max_points = max(points))

# number of unique product
print(paste('Number of wine product:',length(unique(wine_clean$title))))

#number of unique designation
print(paste('Number of wine variety:',length(unique(wine_clean$variety))))

head(wine_clean, 3)

# check the wineID against unique number of title
max(wine_clean$wineID) == length(unique(wine_clean$title))

# check the userID against unique number of taster_name
max(wine_clean$tasterID) == length(unique(wine_clean$taster_name))

print(paste('Number of wine reviewer:', length(unique(wine_clean$taster_name)), ', for', length(unique(wine_clean$title)), 'wine products'))

#taster
taster <- wine_clean %>%
    group_by(tasterID, taster_name) %>%
    summarize(Rcount = n(), maxpoint = max(points), minpoint = min(points), average = mean(points))


# top 3 tasters by review count
head(taster[order(-taster$Rcount),],3)

ggplot(taster, aes(x = reorder(as.factor(taster_name),Rcount), y = Rcount)) + 
geom_bar(stat = 'identity') +
labs(x = "Taster Name", y = 'Count of Review', title = 'Count of Review by each taster') + 
coord_flip()

#wine
wineprod <- wine_clean %>%
    group_by(wineID, title) %>%
    summarize(Rcount = n(), maxpoint = max(points), minpoints = min(points), average = mean(points)) %>%
    arrange(desc(Rcount))


#top 5 most reviewed wine
head(wineprod[order(-wineprod$Rcount, -wineprod$average),], 5)

ggplot(wineprod, aes(Rcount)) + geom_histogram(binwidth = 1, alpha = 0.5, position = 'identity') +
labs( x = "Number of Review",
     y = "Count of Wine product", 
     title = 'Distribution of number or product based on review count',
    subtitle = 'Most of the wines have only 1 review')

# Do tasters who review more wine tend to give different scores?
wine4 <- merge(wine_clean, taster[c('tasterID', 'Rcount')], by.x = 'tasterID', by.y = 'tasterID', all.x = T)
wine4 <- merge(wine4, wineprod[c('wineID', 'average')], by.x = 'wineID', by.y = 'wineID', all.x = T)

avgpoint <- round(mean(wine4$points),2)

avgpoint

#distribution of wine mean point
ggplot(wine4, aes(x = average)) +
    geom_histogram(binwidth = 0.01, alpha = .5, position = 'identity') +
    geom_vline(aes(xintercept = mean(points)), color = 'blue') +
    labs(x = "Mean Points", y = "count", title = "Distribution of User Mean Ratings")

# Differences among reviewers
wine4$Rcut <- cut(wine4$Rcount, c(0,50,100,150,200,250,500,750,1000,1250,1500,1750,2000, 3000, 4000, 5000, 6000, 7000, 10000, 15000))

statbox <- ungroup(wine4) %>%
    group_by(Rcut) %>%
    summarize(avgpoint = round(mean(points, na.rm = T),2),
             medpoint = median(points),
             sdpoint = round(sd(points, na.rm = T),2))

colnames(statbox) <- c("Review Count", "Average Score", "Median Score", "Std Deviation")

statbox

# check for duplicate title for same taster
wine_clean %>%
    arrange(title, taster_name) %>%
    group_by(taster_name, title) %>%
    filter(n() >1)

wine_clean[wine_clean$title == 'Domaines Devillard 2011 ChÃ¢teau de Chamirey  (Mercurey)',]

# will be using taster_twitter_handler as userid, designation as item, points as the rating
wine_cf <- wine_clean[,c('tasterID', 'wineID', 'points')]

head(wine_cf)

wine_cf$tasterID <- as.factor(wine_cf$tasterID)
wine_cf$wineID <- as.factor(wine_cf$wineID)
wine_cf$points <- as.numeric(wine_cf$points)

wine_cf_matrix <- dcast(wine_cf, tasterID ~ wineID, fun.aggregate = mean,value.var = 'points',fill=0)

wine_cf_matrix[1:5, 1:5]

rownames(wine_cf_matrix) = wine_cf_matrix[,1]

wine_cf_matrix[,1] = NULL

dim(wine_cf_matrix)

wine_cf_matrix[1:5, 1:5]

#wine_cf[(wine_cf$taster_name == 'Michael Schachner'& wine_cf$designation == '347 Vineyards'),]

wine_cf_matrix[1:19,1000:1100]









wine_clean$description[1:5]

mystopwords = c(stopwords('english'), 'the', 'and', 'wine')

desc_corpus <- VCorpus(VectorSource(wine_clean$description))

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


