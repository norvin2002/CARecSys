pacman::p_load(tidyverse, tm, stringr,NLP, RColorBrewer, wordcloud, Matrix, recommenderlab, slam, data.table, 
               rARPACK, recosystem, ggplot2, textstem, arules, arulesViz)

setwd('C:/Users/nchandra/OneDrive - National University of Singapore/CARecSys')
df <- fread("./Data/amazon/Reviews.csv", sep = ",", header= TRUE)

tail(df)
str(df)
reviews <- as.data.frame(df)

head(reviews,3)

reviews$UserId[21700:21750] # noticed that some UserId start with #oc-
reviews$UserId <- str_replace_all(reviews$UserId, '#oc-', '' )
reviews$UserId[21700:21750] # check

# remove duplicated UsedId and ProductId
colnames(reviews)
reviews <- reviews[!duplicated(reviews[,c('ProductId','UserId')]),]; dim(reviews) #560,804 rows

# Select needed columns, ProductId, Id, UserId, ProfileName, Score, Summary, Text and count
reviews2 <- reviews[,c(1:4, 7, 9:10)]

head(reviews2)

# Review Count for each product
p_count <- reviews2 %>%
    group_by(ProductId) %>%
    summarize(count = n()) %>%
    arrange(-count)

# merge p_count to reviews2
reviews2 <- merge(reviews2, p_count, by.x = 'ProductId', by.y = 'ProductId', all.x = TRUE)

head(reviews2)

#median count of product
median_count <- median(reviews2$count)
median_count

#only keep product with more than median reviews
review_final <- reviews2 %>%
    filter(count >= median_count)

dim(review_final) # 284,194 rows

# Unique user
length(unique(review_final$UserId)) # 105,159 usersid

# Unique product
length(unique(review_final$ProductId)) #3,110 product id

# Most reviewed product
review_final %>%
    group_by(ProductId) %>%
    summarize(count = n()) %>%
    arrange(-count) %>%
    head()

# subset to 1st 50k data only for this project
review_final_small <- review_final[1:50000,]

head(review_final_small)

p_count <- review_final_small %>%
    group_by(ProductId) %>%
    summarize(count = n()) %>%
    arrange(-count)

# check if products are having enough review
p_count; tail(p_count)

dim(review_final_small) # check, 50,000 rows

review_final_small$UserId <- as.factor(review_final_small$UserId)
review_final_small$ProductId <- as.factor(review_final_small$ProductId)
review_final_small$Score <- as.numeric(review_final_small$Score)

str(review_final_small)

# Mean score for each product
mean_score <- review_final_small %>%
    group_by(ProductId) %>%
    summarize(mean_score = mean(Score))

# merge mean_score to reviews
review_final_small <- merge(review_final_small, mean_score, by.x = 'ProductId', by.y = 'ProductId', all.x = TRUE)

head(review_final_small)

# combine summary for later use
reviews3 <- ungroup(review_final_small) %>%
    group_by(ProductId) %>%
    mutate(combine_summary = paste(Summary, collapse= ' '))

reviews3$combine_summary[1:2] # check

review_final_small$combine_summary = reviews3$combine_summary
rm(reviews3)
review_final_small$combine_summary[1:2] # check

# check lengths
length(unique(reviews3$combine_summary)) # 491 unique combine_summary
length(unique(reviews3$ProductId)) # 679 unique products
# this means that some products have the same combine_summary

# select only ProductId, UserId, Score
review_cf <- review_final_small[,c(3,1,5)]

### Content Based
review_cb <- review_final_small[,c('ProductId','combine_summary', 'count')]
review_cb[1:2, 1:2]
head(review_cb)

rm(reviews)
rm(reviews2)
rm(reviews3)
rm(review-final)
rm(review_cf)
rm(df)
rm(review_final)
#rm(review_cb)

# remove duplicate, since we have already combine the summary    
review_cb_gather <- review_cb[!duplicated(review_cb[,c('ProductId','combine_summary')]),]
review_cb_gather <- review_cb_gather[order(review_cb_gather$ProductId),]
nrow(review_cb_gather[review_cb_gather$ProductId == '0006641040',]) ## there are more than 1 review for this product
nrow(review_cb[review_cb$ProductId ==  '0006641040',]) # there are 37 of them and the text as well summary differ from one another. the combine_summary code is faulty

# check lengths
length(unique(review_cb_gather$combine_summary)) # 491 unique combine_summary
length(unique(review_cb_gather$ProductId)) # 679 unique products
# this means that some products have the same combine_summary

#remove non-ASCII characters
review_cb_gather$combine_summary <- iconv(review_cb_gather$combine_summary, "UTF-8", "ASCII",sub='')

#Preprocessing the text...
corpus <- VCorpus(VectorSource(review_cb_gather$combine_summary))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 'the', 'and', 'will', 'product'))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
# corpus <- tm_map(corpus, lemmatize_strings)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 'the', 'and', 'will', 'product'))
corpus <- tm_map(corpus, stripWhitespace)

# check
for(i in 1:5){
    print(corpus[[i]][1])
} 

#creating the matrix
dtm <- DocumentTermMatrix(corpus)
dtm_ti <- weightTfIdf(dtm)
dtm_ti

sim_mat_cos <- crossprod_simple_triplet_matrix(t(dtm_ti))/(sqrt(col_sums(t(dtm_ti)^2) %*% t(col_sums(t(dtm_ti)^2))))
class(sim_mat_cos)
dim(sim_mat_cos)
sim_mat_cos[1:5,1:5]
rownames(sim_mat_cos) <- unique(review_cb_gather$ProductId)
colnames(sim_mat_cos) <- unique(review_cb_gather$ProductId)
result <- sort(sim_mat_cos[, 1], decreasing = T)[1:5] # reference product id on the first row
result # top 5 product id

result <- sort(sim_mat_cos[, 'B00004RAMY'], decreasing = T)[1:5] # reference product id on the first row
result # top 5 product id

#get the index of the most similar product in 'result'
result_i <- as.integer(names(result))
head(result_i)

# create tags for the product
m1 <- as.matrix(dtm); dim(m1) # 679 rows, 6392 columns
# Obtain top 5 key words for each product
tag <- t(apply(m1, 1, FUN = function(x) colnames(m1)[order(-x)[1:5]]))
tag_unite <- unite(data.frame(tag), "tag", 1:5, sep = " ")


## [WIP] colSum with colnames and sort, so that we can may use this info to add works in our stopword
# m1_sum <- colSums(data.frame(m1)) #Sum each column
# colnames(m1_sum) <- colnames(m1)
# class(m1_sum)
# sor <- sort(m1,decreasing = TRUE) # sort frequency in order
# colnames(sor)


#print out for viewing/checking
for (i in 1:5) {
    cat(paste0("\n",i, "- ", result_i[i], " <ProductId> ", review_cb_gather[result_i[i], 1], "\n<tags> ", tag_unite[result_i[i],]))
}

#to make the testing easier...
#simm-similarity matrix, movi-index of the selected movie, k-top k movies in result
recProd = function (simm, prod, k) {
    found <- sort(simm[, prod], decreasing = T)[2:(k+1)]
    print(found)
    cat(paste0("Selected product: <title> ", review_cb_gather[prod, 1]))
    cat("\nRecommended products:\n")
    resindex <- as.integer(names(found))
    print(resindex)
    for (i in 1:k) {
        cat(paste0("\n",i,"-", resindex[i], " <ProductId> ", review_cb_gather[resindex[i], 1], "\n<tags> ", tag_unite[result_i[i],]))
    }
}

# prod 100, top 3
recProd(sim_mat_cos, 100, 3)
# how to present the proper form of the word, unstem
# probably need to add in more stop words
# need to change the input of the user
recProd(sim_mat_cos, 'B00004RAMY', 3)

### Association
write.csv(review_cb_gather, file = 'review_cb_gather.csv')

# obtain product category
dim(review_cb_gather) # 679 unique product ids

# if previous we only lemmatize and not stem
# corpus1 <- tm_map(corpus, stemDocument)
# corpus1 <- tm_map(corpus1, removeWords, c(stopwords('english'), 'the', 'and', 'will', 'product'))
# corpus1 <- tm_map(corpus1, stripWhitespace)

# check
# for(i in 1:5){
    # print(corpus1[[i]][1])
# } 

#creating the matrix
# dtm_stem <- DocumentTermMatrix(corpus1)
# dtm_ti_stem <- weightTfIdf(dtm_stem)
# dtm_ti_stem

# m2 <- as.matrix(dtm_ti_stem); dim(m1) # 679 rows, 6408 columns

m2 <- as.matrix(dtm_ti)
write.csv(m2, file = 'm2.csv')

# view frequent words to determine product category
sor<-sort(colSums(m2), decreasing = TRUE)
sor[1:300]

pest_control = c('trap', 'bug', 'insect', 'pest')
entertainment = c('book', 'movi')
pet_related = c('dog', 'cat', 'pet')
beverage = c('tea', 'coffe')
oil = 'oil'
tibits = c('snack', 'candi', 'chip', 'haribo')
ingredients = c('past', 'pasta', 'rice', 'noodl', 'veget', 'chicken', 'beef')
seasoning = c('pepper', 'season', 'salt', 'cinnamon', 'chili', 'lemon')
fruit = c('fruit')
dessert = c('cake', 'chocol', 'pastri')

# we can perform rowSums because we did tf-idf
review_cb_gather_prodCat <- review_cb_gather
review_cb_gather_prodCat$pest_control <- rowSums(m2[, pest_control])
review_cb_gather_prodCat$entertainment <- rowSums(m2[, entertainment])
review_cb_gather_prodCat$pet_related <- rowSums(m2[, pet_related])
review_cb_gather_prodCat$beverage <- rowSums(m2[, beverage])
review_cb_gather_prodCat$oil <- m2[, 'oil']
review_cb_gather_prodCat$tibits <- rowSums(m2[, tibits])
review_cb_gather_prodCat$ingredients <- rowSums(m2[, ingredients])
review_cb_gather_prodCat$seasoning <- rowSums(m2[, seasoning])
review_cb_gather_prodCat$fruit <- m2[, fruit]
review_cb_gather_prodCat$dessert <- rowSums(m2[, dessert])

# obtain the most likely prodCat for each product
review_cb_gather_prodCat2 <- review_cb_gather_prodCat[, -c(1:3)]
review_cb_gather_prodCat2$prodCat <- colnames(review_cb_gather_prodCat2)[apply(review_cb_gather_prodCat2[],1,which.max)]
table(review_cb_gather_prodCat2$prodCat) # product classification

review_cb_gather_prodCat$prodCat <- review_cb_gather_prodCat2$prodCat; rm(review_cb_gather_prodCat2)
review_cb_gather_prodCat$prodCat <- as.factor(review_cb_gather_prodCat$prodCat)

# item frequency plot
review_cb_gather_prodCat %>%
    group_by(prodCat) %>%
    summarise(total_count = sum(count)) %>%
    arrange(-total_count)

allcount <- sum(review_cb_gather_prodCat$count)

review_cb_gather_prodCat %>%
    group_by(prodCat) %>%
    summarise(total_count = sum(count), item_frequency = total_count/allcount) %>%
    ggplot(aes(x = reorder(prodCat, -total_count), y = total_count)) +
                  geom_bar(stat = 'identity') +
                  geom_text(aes(label=round(item_frequency,2)), vjust = 1.5)

# beverage, tibits and pet_related made up of the majority
# will result in a bias results
# as such, to split into small grouping
# beverage split into coffee and tea
# pet_related split to dog and cat
# tibits split to tibits and sweets

tibits = c('snack', 'chip')
sweets = c('candi', 'haribo')
review_cb_gather_prodCat$beverage <- NULL
review_cb_gather_prodCat$pet_related <- NULL
review_cb_gather_prodCat$tibits <- NULL

review_cb_gather_prodCat$coffee <- m2[, 'coffe']
review_cb_gather_prodCat$tea <- m2[, 'tea']
review_cb_gather_prodCat$dog <- m2[, 'dog']
review_cb_gather_prodCat$cat <- m2[, 'cat']
review_cb_gather_prodCat$tibits <- rowSums(m2[, tibits])
review_cb_gather_prodCat$sweets <- rowSums(m2[, sweets])

# obtain the most likely prodCat for each product
review_cb_gather_prodCat2 <- review_cb_gather_prodCat[, -c(1:3)]
review_cb_gather_prodCat2$prodCat <- colnames(review_cb_gather_prodCat2)[apply(review_cb_gather_prodCat2[],1,which.max)]
table(review_cb_gather_prodCat2$prodCat) # product classification

review_cb_gather_prodCat$prodCat <- review_cb_gather_prodCat2$prodCat; rm(review_cb_gather_prodCat2)
review_cb_gather_prodCat$prodCat <- as.factor(review_cb_gather_prodCat$prodCat)

# item frequency plot
review_cb_gather_prodCat %>%
    group_by(prodCat) %>%
    summarise(total_count = sum(count)) %>%
    arrange(-total_count)

allcount <- sum(review_cb_gather_prodCat$count)

review_cb_gather_prodCat %>%
    group_by(prodCat) %>%
    summarise(total_count = sum(count), item_frequency = total_count/allcount) %>%
    ggplot(aes(x = reorder(prodCat, -total_count), y = total_count)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label=round(item_frequency,2)), vjust = 1.5)
# slightly more balanced




# getting the basket grouping for users
write.csv(review_cb_gather_prodCat, file = 'review_cb_gather_prodCat.csv')

# vlookup function
    vlookup <- function(ref, #the value or values that you want to look for
                        table, #the table where you want to look for it; will look in first column
                        column, #the column that you want the return data to come from,
                        range=FALSE, #if there is not an exact match, return the closest?
                        larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
    {
        if(!is.numeric(column) & !column %in% colnames(table)) {
            stop(paste("can't find column",column,"in table"))
        }
        if(range) {
            if(!is.numeric(table[,1])) {
                stop(paste("The first column of table must be numeric when using range lookup"))
            }
            table <- table[order(table[,1]),] 
            index <- findInterval(ref,table[,1])
            if(larger) {
                index <- ifelse(ref %in% table[,1],index,index+1)
            }
            output <- table[index,column]
            output[!index <= dim(table)[1]] <- NA
            
        } else {
            output <- table[match(ref,table[,1]),column]
            output[!ref %in% table[,1]] <- NA #not needed?
        }
        dim(output) <- dim(ref)
        output
    }
# end of vlookup function

# vlookup prodCat for 50k data set
vlookup('0006641040', review_cb_gather_prodCat, 'prodCat', range = FALSE) # test
review_final_small$prodCat <- vlookup(review_final_small$ProductId, review_cb_gather_prodCat, 'prodCat', range = FALSE)

sample(review_final_small$prodCat)

review_final_small %>%
    group_by(UserId) %>%
    
review_final_small_assoc <- review_final_small[, c('UserId', 'prodCat')]
review_final_small_assoc$value <- TRUE
head(review_final_small_assoc)
dim(review_final_small_assoc)

review_final_small_assoc_wide <- reshape(review_final_small_assoc, idvar = 'UserId', timevar = 'prodCat', direction = 'wide')
review_final_small_assoc_wide[is.na(review_final_small_assoc_wide)] <- FALSE

dim(review_final_small_assoc_wide)
head(review_final_small_assoc_wide) # check

# Clean up names
colnames(review_final_small_assoc_wide) <- gsub(x=colnames(review_final_small_assoc_wide),
                                                pattern="value.", replacement="")
colnames(review_final_small_assoc_wide) # check

transaction <- as(review_final_small_assoc_wide,"transactions")

fsets <- eclat(transaction, parameter = list(support = 0.05), control =
                   list(verbose=FALSE))

summary(fsets)
# 9 single item relationship

singleItems = fsets[size(items(fsets)) == 1]
inspect(singleItems)

# multiItems = fsets[size(items(fsets)) > 1]
# inspect(multiItems)

# install.packages("arulesViz")
library(arulesViz)

# Grules = apriori(transaction, parameter = list(support=0.01, confidence = 0.5))

inspect(sort(Grules, by = "lift"))

quality(Grules)

subrules = Grules[quality(Grules)$confidence > 0.55]
inspect(subrules)

plot(Grules)
plot(Grules, method = "grouped")
plot(Grules, method = "paracoord")

