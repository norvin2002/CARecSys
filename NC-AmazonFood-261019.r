
pacman::p_load(tidyverse, tm, stringr,NLP, RColorBrewer, wordcloud, Matrix, recommenderlab, slam, data.table, rARPACK, recosystem)

setwd('C:/Users/nchandra/OneDrive - National University of Singapore/CARecSys')

#memory.limit(size=64000)

memory.limit()

df <- fread("./Data/amazon/Reviews.csv", sep = ",", header= TRUE)

tail(df)

str(df)

reviews <- as.data.frame(df)

head(reviews,3)

reviews$UserId <- str_replace_all(reviews$UserId, '#oc-', '' )

# remove duplicated UsedId and ProductId
reviews <- reviews[!duplicated(reviews[,c(2,3)]),]

# Select needed columns, ProductId, Id, UserId, ProfileName, Score, Summary, Text, count, mean_score
reviews2 <- reviews[,c(1:4, 7, 9:10)]

reviews2 <- reviews2 %>%
    mutate(combine_summary = paste(Summary, Text))

nrow(reviews2[duplicated(reviews2$combine_summary),])

reviews2 <- reviews2[!duplicated(reviews2$combine_summary),]

head(reviews2)

p_count <- reviews2 %>%
    group_by(ProductId) %>%
    summarize(count = n()) %>%
    arrange(-count)

p_count

reviews2 <- merge(reviews2, p_count, by.x = 'ProductId', by.y = 'ProductId', all.x = T)

head(reviews2)

#median count of product
median_count <- median(reviews2$count)
median_count

#only keep product with more than median reviews
review_final <- reviews2 %>%
    filter(count >= median_count)
    

dim(review_final)

# Most reviewed product
review_final %>%
    group_by(ProductId) %>%
    summarize(count = n()) %>%
    arrange(-count) %>%
    head()

# Unique user
length(unique(review_final$UserId))

# Unique product
length(unique(review_final$ProductId))

# subset to 1st 50k data only for this project
review_final_small <- review_final[1:50000,]

# remove earlier count done at full dataset
review_final_small$count = NULL

# Re run review Count for each product in the subset only
p_count <- review_final_small %>%
    group_by(ProductId) %>%
    summarize(count = n()) %>%
    arrange(-count)

# Mean score for each product
mean_score <- review_final_small %>%
    group_by(ProductId) %>%
    summarize(mean_score = mean(Score))

# merge p_count to reviews2
review_final_small <- merge(review_final_small, p_count, by.x = 'ProductId', by.y = 'ProductId', all.x = TRUE)

# merge mean_score to reviews2
reviews_final_small <- merge(review_final_small, mean_score, by.x = 'ProductId', by.y = 'ProductId', all.x = TRUE)

head(review_final_small)

# double check for duplicate review text
nrow(review_final_small[duplicated(review_final_small$combine_summary),])

p_count <- review_final_small %>%
    group_by(ProductId) %>%
    summarize(count = n()) %>%
    arrange(-count)

# check if products are having enough review
min(review_final_small$count)

max(review_final_small$count)

dim(review_final_small)

# select only ProductId, UserId, Score
review_cf <- review_final_small[,c(3,1,5)]

summary(review_cf$Score)

hist(review_cf$Score)

boxplot(review_cf$Score)

head(review_cf)

length(unique(review_final_small$UserId))
length(unique(review_final_small$ProductId))

review_final_small %>%
    group_by(UserId) %>%
    summarize(count = n()) %>%
    arrange(-count) %>%
    tail()

## RECOMMENDER LAB IBCF ##

# set data format
review_cf$UserId <- as.factor(review_cf$UserId)
review_cf$ProductId <- as.factor(review_cf$ProductId)
review_cf$Score <- as.numeric(review_cf$Score)

############################# code below not in use############################################

#review_matrix <- dcast(review_cf, ProductId ~ UserId, fun.aggregate = mean, value.var = 'Score', fill = 0)

#review_matrix[1:10, 30:60]

#rownames(review_matrix) = review_matrix[,1]

#review_matrix[,1] = NULL

#print(object.size(review_matrix), units = 'auto')

#dim(review_matrix)

#review_matrix[1:5,1:5]

###############################################################################################

# Create sparse matrix
review_spmatrix <- sparseMatrix(i = as.integer(review_cf$UserId),
                               j = as.integer(review_cf$ProductId),
                               x = review_cf$Score,
                               dimnames = list(unique(review_cf$UserId), 
                                               unique(review_cf$ProductId))
                                )

dim(review_spmatrix)

review_spmatrix[1:10,1:5]

# Sparse matrix has much smaller size
print(object.size(review_spmatrix), units = 'auto')



# format sparse matrix for recommenderlab library
review_sprrm <- as(review_spmatrix, 'realRatingMatrix')

review_sprrm

dim(review_sprrm)

print(object.size(review_sprrm), units = 'auto')

#image(review_sprrm, main = "Raw Ratings")

# Split the data into training and testing subsets on 80/20 with up to 5 items recommended for each user ('given'), 
# any rating greater than 3 is considered positive rating
given <- 1 #refer to https://stackoverflow.com/questions/30128383/error-evaluationscheme-recommenderlab-in-r
# some of the UserId only has 1 review so the given value cannot be more than 1
rating_threshold <- 4 # set 4 and above considered positive
eval <- evaluationScheme(review_sprrm, method = 'split', train = 0.8, given = given, goodRating = rating_threshold)

#IBCF with cosine similarity and not normalized data for k most similar items
IBCF_N_C <- Recommender(data = getData(eval, 'train'),
                        method = "IBCF", 
                        parameter = list(normalize = NULL, 
                                         method = 'Cosine', k = 30))

# IBCF with cosine similarity and centered data for k most similar items
IBCF_C_C <- Recommender(data = getData(eval, 'train'), 
                        method = 'IBCF', 
                        parameter = list(normalize = "center", method = "Cosine", k = 30))

# IBCF with cosine similarity and z-score normalised data for k most similar item
IBCF_Z_C <- Recommender(data = getData(eval, 'train'),
                       method = 'IBCF',
                       parameter = list(normalize = "Z-score", method = 'Cosine', k = 30))

# Popular Model
POP <- Recommender(data = getData(eval, 'train'), method = 'POPULAR', param = list(normalize = 'center'))

# Evaluation of IBCF
pIBCF_N_C <- predict(IBCF_N_C, getData(eval, 'known'), type = 'ratings')
pIBCF_C_C <- predict(IBCF_C_C, getData(eval, 'known'), type = 'ratings')
pIBCF_Z_C <- predict(IBCF_Z_C, getData(eval, 'known'), type = 'ratings')

pPOP <- predict(POP, getData(eval, 'known'), type = 'ratings')

# Performance statistics
perf_stats <- rbind(
    IBCF_N_C = calcPredictionAccuracy(pIBCF_N_C, getData(eval, 'unknown')),
    IBCF_C_C = calcPredictionAccuracy(pIBCF_C_C, getData(eval, 'unknown')),
    IBCF_Z_C = calcPredictionAccuracy(pIBCF_Z_C, getData(eval, 'unknown')),
    pPOP = calcPredictionAccuracy(pPOP, getData(eval, 'unknown'))
    )

perf_stats

#rm(IBCF_N_C, IBCF_C_C, IBCF_Z_C)

IBCF_N_C

#pred <- predict(IBCF_N_C, review_sprrm[1,])

# predict items for user
pred<-predict(IBCF_N_C, getData(eval,'unknown'), type = 'ratings')

# item - item cosine similarity matrix
as(pred, 'matrix')[100:150, 200:250]

pred<-predict(IBCF_N_C, getData(eval,'unknown'), n= 5)

as(pred, "list")

# Try to recommend item to 1 specific user
#rec_item <- predict(IBCF_N_C, review_sprrm['A3IPGCVNG88DSR',], n= 5)
#rec_item <- predict(IBCF_N_C, review_sprrm['A26OPNLJ0JYHPH',], n = 20)
rec_item <- predict(IBCF_N_C, review_sprrm["AYZ0PR5QZROD1",], n= 5 )

# display the recommendation
as(rec_item, "list")

getRatings(rec_item)

review_cf[review_cf$UserId == 'A26OPNLJ0JYHPH',]

pred_rating <- predict(IBCF_N_C, review_sprrm["A3IPGCVNG88DSR",], n= 10)

# display rating
as(pred_rating, "list")

getRatings(pred_rating)

review_cf[review_final_small$UserId %in% c('AYZ0PR5QZROD1'),]

########## Not Used #############

# Create a string containing 6 new products
# cust_order <- sample(review_cf$ProductId, size = 6)

#cust_order <- levels(droplevels(cust_order))

#cust_order

#nrow(review_cf[review_cf$ProductId %in% cust_order,])



#new_order_mat <- review_cf %>%
    #select(UserId,ProductId) %>%
    #unique() %>%
    #mutate(value = as.numeric(ProductId %in% cust_order))
    

#new_order_matrix <- sparseMatrix(i = as.numeric('new_order_mat$UserId'),
#                              j = as.numeric(new_order_mat$ProductId),
#                              x = new_order_mat$value,
#                              dimnames = list(unique(new_order_mat$UserId), 
#                                               unique(new_order_mat$ProductId))
#                                 )

#dim(new_order_matrix)

#new_order_rrm <- as(new_order_matrix, 'realRatingMatrix')

#new_order<- review_cf %>%
#    select(UserId, ProductId) %>%
#    unique() %>%
#    mutate(value = as.numeric(ProductId %in% cust_order)) %>%
#    spread(key = ProductId, value = value) %>%
#    as.matrix() %>%
#    as("realRatingMatrix")

#new_order<- review_cf %>%
#    select(UserId, ProductId) %>%
#    unique() %>%
#    mutate(value = as.numeric(ProductId %in% cust_order)) %>%
#    spread(key = ProductId, value = value) %>%
#    as.matrix()

#pred_new_order <- predict(IBCF_N_C, newdata = new_order, n = 10)

#as(pred_new_order, "list")

# ALTERNATING LEAST SQUARE on RECOSYSTEM

# Train test split for Recosystem ALS
smp_size <- floor(0.9 * nrow(review_cf))
train_indexes <- sample(1: nrow(review_cf), size = smp_size)
trainevents <- review_cf[train_indexes, ]; dim(trainevents)
testevents  <- review_cf[-train_indexes, ]; dim(testevents)

#testevents1  <- review_cf[-train_indexes, ]; dim(testevents)
#head(testevents1)

# load into recosystem format
trainset = data_memory(trainevents$UserId, trainevents$ProductId, trainevents$Score, index1= TRUE)
testset  = data_memory(testevents$UserId, testevents$ProductId, testevents$Score, index1= TRUE)

# get optimised factorisation using r$tune
r = Reco()
opts = r$tune(trainset, opts=list(dim=c(20, 30, 40), lrate=c(0.1,0.2), costp_l1=0, costq_l1=0, niter=40))

opts

opts$min

opts = r$tune(trainset, opts=list(dim=c(5, 10, 20), lrate=c(0.1,0.2), costp_l1=0, costq_l1=0, niter=40))

opts

opts = r$tune(trainset, opts=list(dim=c(1:5), lrate=c(0.1,0.2), costp_l1=0, costq_l1=0, niter=40))

opts

opts$min

# Best Dim obtained
r$train(trainset, opts = opts$min)

# get predictions:  this multiplies the user vectors in testset, with the item vectors in Q
testevents$prediction <- r$predict(testset, out_memory())   # out_memory means output to memory, can also use "out_file"
head(testevents)

# New dataset not seen before purely for testing ALS model
review_cftest <- review_final[100001:100500,c(3,1,5)]

review_cftest$UserId <- as.factor(review_cftest$UserId)
review_cftest$ProductId <- as.factor(review_cftest$ProductId)
review_cftest$Score <- as.numeric(review_cftest$Score)

head(review_cftest)

review_cftestset  = data_memory(review_cftest$UserId, review_cftest$ProductId, review_cftest$Score, index1= TRUE)

review_cftest$prediction <- r$predict(review_cftestset, out_memory())

head(review_cftest, 30)

## Matrix Factorisation Approach to recommend products to user given the product info provided by user
#get factorised matrices
r$output() # exports the two matrix to the current directory (as mat_P.txt, mat_Q.txt)


P = as.matrix(read.table("mat_P.txt")) # user
Q = as.matrix(read.table("mat_Q.txt")) # product

rownames(P) = as.factor(unique(review_cf$UserId))
rownames(Q) = as.factor(unique(review_cf$ProductId))
head(P) # the user factors matrix, rows = user, columns are the latent features
head(Q) # the item factors matrix, rows = items, columns are the latent features

### Testing the ALS Recommender ###

# Create a string containing 5 new products
cust_order <- sample(review_cf$ProductId, size = 5)

cust_order <- levels(droplevels(cust_order))

cust_order

# empty matrix container for the final result
result = matrix(nrow = 0, ncol = 2)

#for each item in the cust_order, do a matrix multiplication to find top 5 user based on rating
for (item in cust_order){
    T = as.matrix(Q[item,])
    prats = P %*% T
    prats= prats[order(prats, decreasing = TRUE),]
    similar_user = names(prats)[1:5]
    #print(similar_user)
    
    # for each user, do a matrix multiplication back to find relevant products
    for (name in similar_user){
        # empty temporary matrix
        mat = ''
        T2 = as.matrix(P[name,])
        prats2 = Q %*% T2
        prats2 = prats2[order(prats2, decreasing = TRUE),]
        similar_product = prats2[1:5]
        mat <- cbind(as.vector(names(similar_product)), as.vector(similar_product))
        #print(mat)
        #retutn a matrix
        result <- rbind(result, mat)
    }
    
}

#name the matrix
colnames(result) <- c('ProductId', 'Rating')

dim(result)

result_df <- as.data.frame(result[order(result[,2], result[,1], decreasing = TRUE),])

# convert Rating from factor to numeric, extra step in between, convert to char before numeric
result_df$Rating <- as.numeric(as.character(result_df$Rating))

head(result_df,10)

# groupby and get the mean rating of each product, sort descending
cf_recom_product <- result_df %>%
    group_by(ProductId) %>%
    summarize(meanRating = mean(Rating)) %>%
    arrange(-meanRating) %>%
    mutate(Rank = order(meanRating, decreasing = TRUE))

# top 5 relevant products
cf_recom_product

#################### CODE BLOCK BUILDING ###############

# Create a string containing 6 new products
cust_order1 <- sample(review_cf$ProductId, size = 1)

cust_order1 <- levels(droplevels(cust_order1))

target = cust_order1

T = as.matrix(Q[target,]) ; T

head(P)

prats = P %*% (T)

head(prats)

prats= prats[order(prats, decreasing = TRUE),]

head(prats)

#top 1 similar user
similar_user <- names(prats)[1]

similar_user

T2 = as.matrix(P[similar_user,]) ; T2

prats2 = Q %*% (T2)

prats2 = prats2[order(prats2[,1], decreasing = TRUE),]

# top 5 recommended products
prats2[1:5]

########################### END OF CODE BLOCK #########################

rm(opts)
rm(IBCF_C_C)
rm(IBCF_Z_C)
rm(POP)

rm(reviews)

# Content Based Recommender
#review_cb <- ungroup(review_final_small) %>%
    #mutate(combine_summary = paste(Summary, Text, sep = ' '))

# copy the data over
review_cb <- review_final_small

#review_cb <- ungroup(review_final_small) %>%
#    group_by(ProductId) %>%
#    mutate(combine_summary2 = paste(Summary, collapse= ' '))

review_cb[duplicated(review_cb$combine_summary),]

review_cb[review_cb$combine_summary == "very good This product is a very health snack for your pup as it is made of 100% beef liver. My puppy does all of his tricks to get this treat. It is a little pricy but the container is large so it should last a long time as long as you don't overfeed.",]

review_cb <- review_cb[order(review_cb$ProductId), ]

review_cb_prod <- review_cb[,c(1,8,9)]

review_cb_prod <- review_cb_prod[order(review_cb_prod$ProductId),]

#collapse the summary to each ProductId
review_cb_prod2 <- aggregate(combine_summary ~ ProductId, data = review_cb_prod, FUN = paste, collapse = " ")


dim(review_cb_prod2)

#review_cb_prod2[review_cb_prod2$ProductId == '0006641040',]

nrow(review_cb_prod2[duplicated(review_cb_prod2$combine_summary),])

# check lengths
length(unique(review_cb_prod2$combine_summary))
length(unique(review_cb_prod2$ProductId))

# Remove non ASCII Character
review_cb_prod2$combine_summary <- iconv(review_cb_prod2$combine_summary, "UTF-8", "ASCII",sub='')

mystopwords = c(stopwords('english'), 'the', 'and', 'will', 'product')

desc_corpus <- VCorpus(VectorSource(review_cb_prod2$combine_summary))

desc_corpus <- tm_map(desc_corpus, content_transformer(tolower))

desc_corpus <- tm_map(desc_corpus, removeNumbers)

desc_corpus <- tm_map(desc_corpus, removeWords, mystopwords)
desc_corpus <- tm_map(desc_corpus, removePunctuation)

desc_corpus <- tm_map(desc_corpus, stemDocument)
desc_corpus <- tm_map(desc_corpus, removeWords, mystopwords)
desc_corpus <- tm_map(desc_corpus, stripWhitespace)

for(i in 1:3){
    print(desc_corpus[[i]][1])
}

#creating the matrix
dtm <- DocumentTermMatrix(desc_corpus)
dtm_ti <- weightTfIdf(dtm)
dtm_ti

dtm_ti2 = removeSparseTerms(dtm_ti, 0.99)

dtm_ti2

freq = data.frame(sort(colSums(as.matrix(dtm_ti2)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

gc()

## Compute cosine similarity
# slam package for cosine similarity computation
sim_mat_cos <- crossprod_simple_triplet_matrix(t(dtm_ti2))/(sqrt(col_sums(t(dtm_ti2)^2) %*% t(col_sums(t(dtm_ti2)^2))))

length(unique(review_cb_prod2$ProductId))

#assign colnames and rownames to the sim_mat_cos
rownames(sim_mat_cos) <- unique(review_cb_prod2$ProductId)
colnames(sim_mat_cos) <- unique(review_cb_prod2$ProductId)

#sim_mat_cos[1:5,1:30]

## Function to return a prediction matrix, cannot get output out, need some working ###
#recProd = function (simm, prod, k) {
#    recom = matrix(ncol =2, nrow = 0)
#    for (i in cust_order){
#        output = ''
#        result <- sort(simm[, i], decreasing = TRUE)[2:(k+1)]
#        output <- cbind(as.vector(names(result)), as.vector((result)))
#        recom <- rbind(recom, output)
#        }
#    #print(recom)
#    return(recom)
#    }

#sort(sim_mat_cos[, 'B00004RBDZ'], decreasing = TRUE)[2:(5+1)]

k = 5 # number of product recommended
simm = sim_mat_cos 
recom = matrix(ncol =2, nrow = 0)
    for (i in cust_order){
        output = ''
        result <- sort(simm[, i], decreasing = TRUE)[2:(k+1)]
        output <- cbind(as.vector(names(result)), as.vector((result)))
        recom <- rbind(recom, output)
        }

colnames(recom) <- c('ProductId', 'Rating')

cb_recom <- as.data.frame(recom[order(recom[,2], recom[,1], decreasing = TRUE),])

# convert Rating from factor to numeric, extra step in between, convert to char before numeric
cb_recom$Rating <- as.numeric(as.character(cb_recom$Rating))

head(cb_recom,10)

# groupby and get the mean rating of each product, sort descending
cb_recom_product <- cb_recom %>%
    group_by(ProductId) %>%
    summarize(meanRating = mean(Rating)) %>%
    arrange(-meanRating) %>%
    mutate(Rank = order(meanRating, decreasing = TRUE))

# top 5 relevant products
cb_recom_product

#names(result)

#result_i <- names(result)
#head(result_i)

# create tags for the product
#m1 <- as.matrix(dtm_ti2); dim(m1) # 679 rows, 6392 columns


# Obtain top 5 key words for each product
#tag <- t(apply(m1, 1, FUN = function(x) colnames(m1)[order(-x)[1:5]]))
#tag_unite <- unite(data.frame(tag), "tag", 1:5, sep = " ")

#saveRDS(sim_mat_cos, "./sim_mat_cos.rds")

#recProd = function (simm, prod, k) {
#    found <- sort(simm[, prod], decreasing = TRUE)[2:(k+1)]
#    print(found)
#    cat(paste0("Selected product : <title>", colnames(review_cb_prod2[prod,1])))
#    cat(paste0("Selected product: <title> ", review_cb_prod2[prod, 1]))
#    cat("\nRecommended products:\n")
#    resindex <- names(found)
#    print(resindex)
#    for (i in 1:k) {
#        cat(paste0("\n",i,"-", resindex[i], " <ProductId> ", 
#                   review_cb_prod2[resindex[i], 1], 
#                   "\n<tags> ", 
#                   tag_unite[result_i[i],]))
#    }
#}

review_final[review_final$ProductId == '0006641040',]



#### Hybrid Recommender #####

#Check for intersect of the recommendations
common <- intersect(cf_recom_product$ProductId, cb_recom_product$ProductId)

common

cf_recom_product[cf_recom_product$ProductId == common,]

cb_recom_product[cb_recom_product$ProductId == common,]

#normalize cf_recom_product and cb_recom_product
cf_recom_product$meanRating = with(cf_recom_product, (meanRating - min(meanRating)) / (max(meanRating) - min(meanRating)))
cb_recom_product$meanRating = with(cb_recom_product, (meanRating - min(meanRating)) / (max(meanRating) - min(meanRating)))

cf_recom_product

cb_recom_product

# combine the normalized products from each recommender system
combi_prod_recom <- rbind(cf_recom_product[,1:2], cb_recom_product[,1:2])

# Combined normalized products recommendation, sorted descending by meanRating
combi_prod_recom <- combi_prod_recom[order(combi_prod_recom$meanRating, decreasing = TRUE),]
#combi_prod_recom

# total number of items
nrow(combi_prod_recom)

# recommend items from top 5, middle 3 and bottom 2 of the list to introduce a mix
combi_recom_mix <- combi_prod_recom[c(1:5, 
                                      (nrow(combi_prod_recom)/2):(nrow(combi_prod_recom)/2+2),
                                      (nrow(combi_prod_recom)-1): nrow(combi_prod_recom)),
                                   ]

combi_recom_mix

cust_order


