library("recommenderlab")
library("data.table")
library("ggplot2")
library("countrycode")
library("Binarize")

#purchase <- read.csv("purchase.csv", header = TRUE)
purchase <- read.csv("purchase_modified.csv", header = TRUE)

purchase <- data.table(purchase)
#purchase$cat <- NULL
#purchase$name <- NULL
#purchase$cost <- NULL
head(as.matrix(purchase))
length(unique(purchase$id))


tourIDs <- as.vector(purchase$id)
length(tourIDs)
# Assigning unique tour ids to the column. Factoring long varying tour ids to numeric integers ids
# need to add a hash based fucntion to generate factors based on their long ids.
purchase$id <- as.numeric(as.factor(purchase$id))

length(unique(purchase$name))
length(unique(purchase$id))
# generating unique user ids for each purchase
userIds <- 1:length(purchase$id) 
purchase$userIds <- as.numeric(userIds)

str(purchase)
# computing similarity of first n users.


#The columns are as follows:
#  • category: This is a letter specifying the content of the column. The
#  columns containing a user or an item ID belong to the categories C
#  and V, respectively.
#  • value: This is a number specifying the user or item ID.
# We can assign the column names and select the rows containing either usersor items:


head(purchase)
table_users <- data.frame(purchase$id, purchase$userIds)
names(table_users) <- c("id", "userIds")
# table_users <- purchase

tail(table_users)
length(unique(table_users$id))

####### Generating Binary Ratings  ###########

# custom method of generating the binary rating matrix
nrows = length(table_users$userIds)
df <- data.frame(matrix(ncol = length(unique(table_users$id)), nrow  = nrows))
dim(df)
# setting column names
x <- c(unique(table_users$id))
length(x)
colnames(df) <- x
colnames(df)

tail(df)
for (i in 1:nrows) {
  
  row <- table_users[i,]
  indexOfColumn <- which(colnames(df)==row$id)
  # print(indexOfColumn)
  # indexOfColumn <- grep(row$userIds, colnames(df))
  df[i,indexOfColumn] <- 1
  
  # Randomly adding transactions
  rn = sample(1:4, 1)
  rn = floor(runif(sample(1:4, 1), min=1, max=60))
  print(rn)
  for (j in 1:length(rn)){
    df[i,j] <- 1
  }

}

dim(df)
head(df)

# find total zero containing rows
indremoved = which(apply(df, 1, function(x) all(x == 0)) )
# totalZeroContainingRows
length(indremoved)
#df2 = df[ -indremoved, ]
#dim(df2)
# lest find occurences of tour with id == 1 
#unique(which(colnames(df)==1))
tail(df)

binaryMatrix <- as.matrix(x = df)
dim(binaryMatrix)
#binaryMatrix <- matrix(as.numeric(unlist(df)),nrow=nrow(df))

# binaryMatrix <- matrix( rep( 0, len= length(table_users$id)), nrow = length(table_users$userIds))

tail(binaryMatrix)
image(binaryMatrix)
binaryMatrix[is.na(binaryMatrix)] <- 0

ratings_matrix <- as(binaryMatrix, "binaryRatingMatrix")
ratings_matrix
image(ratings_matrix, main = "Binary rating matrix")

# Checking first 100 rows of purchases
image(ratings_matrix[1:100, ], main = "Binary rating matrix")
dim(ratings_matrix)
#image(ratings_matrix, main = "Binary rating matrix")


# What is my Sparsity ?
# Sparsity  = Number of Empty cells / Total Number of cells.

library(tidyverse)
dff <- (map(df, ~sum(is.na(.))))
totalEmptyCell <- sum(sapply(dff, function(x) sum(x)))
totalCell <- dim(df)[1] * dim(df)[2]
Sparsity <- totalEmptyCell / totalCell
cat("Sparsity of Percentage", Sparsity * 100)

####### Finsihed Binary Ratings  ###########

####### Modeling  ###########

which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]
(recc_data_train)

####### -> IBCF  

# recommenderRegistry
# recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
# recommender_models
# View(recommender_models$UBCF_realRatingMatrix$parameters)
# 
# recc_model <- Recommender(data = recc_data_train, method = "UBCF")
# recc_model
# model_details <- getModel(recc_model)
# view(model_details)
# names(model_details)
# model_details$data
# 
# 
# n_recommended <- 2
# recc_predicted <- predict(object = recc_model,newdata = recc_data_test, n = n_recommended) 
# recc_predicted
# 
# # Finding actual values on the 
# recc_predicted@itemLabels
# recc_matrix <- sapply(recc_predicted@items, function(x){
#   colnames(ratings_matrix)[x]
# })
# length(unique(recc_matrix))
# dim(recc_matrix)
# 
# 
# 
# ## create 90/10 split (known/unknown) for the first 500 users in Jester5k
# e <- evaluationScheme(ratings_matrix, method="split", train=0.9,
#                       k=10, given=0, goodRating=1)
# 
# e <- evaluationScheme(ratings_matrix, method="cross-validation",
#                       k=3, given=1,goodRating=1)
# e
# r <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(method = "Jaccard"))
# 
# r
# n_recommended <- 6
# p <- predict(object = r, newdata = recc_data_test,n = n_recommended)
# p
# p@items
# 
# recc_matrix <- sapply(p@items, function(x){
#   colnames(ratings_matrix)[x]
# })
# length(unique(recc_matrix))
# 
# # list tour names
# 
# recc_matrix
# library(caret)
# confusionMatrix(data = p@ratings)
# 
# calcPredictionAccuracy(x=p,data=recc_data_test,given=p@n)
# 
# 
# recommenderRegistry$get_entry_names()
# rec <- Recommender(ratings_matrix, method = "POPULAR")
# rec
# str(getModel(rec))
# recommenderRegistry$get_entry("POPULAR", dataType = "binaryRatingMatrix")
# recommenderRegistry$get_entry("SVD", dataType = "realRatingMatrix")


##############################################################
# es <- evaluationScheme(ratings_matrix, method="cross-validation",
#                        k=4, given=1)
# es
# r <- Recommender(getData(es, "train"), "IBCF")
# r
# p <- predict(r, getData(es, "known"), type="topNList", n=3)
# p
# algorithms <- list(
#   RANDOM = list(name = "RANDOM", param = NULL),
#   POPULAR = list(name = "POPULAR", param = NULL)
# )
# 
# evlist <- evaluate(es, algorithms)
# evlist
# avg(evlist)
# plot(evlist, main = "ROC curve",legend="topright")
# plot(evlist,"prec/rec", main = "Precision-recall",legend="topright")

# eval_accuracy  <- calcPredictionAccuracy(p, getData(e, "unknown"), given=p@n, goodRating=1,byUser=FALSE)

########################## All In. Brute Force. hallelujah ! ####################################
number_of_folds = 4
# 'given' is a problematic parameter here.
es <- evaluationScheme(ratings_matrix, method="cross-validation",k=number_of_folds, given=1)
es

models_to_evaluate <- list(
  IBCF_jack = list(name = "IBCF", param = list(method =
                                                "Jaccard")),
  IBCF_cos = list(name = "IBCF", param = list(method =
                                                "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method =
                                                "pearson")),
  UBCF_jack = list(name = "UBCF", param = list(method =
                                                 "Jaccard")),
  UBCF_cos = list(name = "UBCF", param = list(method =
                                                "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method =
                                                "pearson")),
  random = list(name = "RANDOM", param=NULL)
)

# Different number of recommendations
# for : 1,5,10,20,....,100 recommendations
n_recommendations <- c(1, 5, seq(10, 100, 10))
n_recommendations
list_results <- evaluate(x = es, method = models_to_evaluate, n= n_recommendations)

# checking which ones are best. Particularly, IBCF cosine and Jackard

#sapply(list_results, class) == "evaluationResults"
avg_matrices <- lapply(list_results, avg)
class(avg_matrices)

tail(avg_matrices$IBCF_cos)
tail(avg_matrices$IBCF_jack)
avg_matrices$IBCF_jack[order(avg_matrices$IBCF_jack)] # higest 56.260135135
avg_matrices$IBCF_cos[order(avg_matrices$IBCF_cos)] # higest  56.302364865

# Plotting ROC curve
plot(list_results,  legend = "bottomright") 
title("ROCcurve")


# Plotting precision/recall curve
plot(list_results, "prec/rec", legend = "topleft")
title("Precision-recall")



########### TODO: Visualizations to add ###########
## 1. most recommended tour by all of the methods ? 
## 2. most recommended tour among the users ?
## 3. ?