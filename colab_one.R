# load CSV version of jester subset


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

row <- table_users[177,]
row
indexOfColumn <- which(colnames(df)==row$id)
indexOfColumn
df[177,indexOfColumn] <- 1
df[177,indexOfColumn]
df[177,59]
tail(df)
for (i in 1:nrows) {
  
  row <- table_users[i,]
  indexOfColumn <- which(colnames(df)==row$id)
  print(indexOfColumn)
  # indexOfColumn <- grep(row$userIds, colnames(df))
  
  df[i,indexOfColumn] <- 1
  
  print(df[i,])
}

dim(df)
tail(df)
# find total zero containing rows
indremoved = which(apply(df, 1, function(x) all(x == 0)) )
# totalZeroContainingRows
length(indremoved)
#df2 = df[ -indremoved, ]
#dim(df2)
# lest find occurences of tour with id == 1 
unique(which(colnames(df)==1))
tail(df)
binaryMatrix <- as.matrix(x = df)
dim(binaryMatrix)
#binaryMatrix <- matrix(as.numeric(unlist(df)),nrow=nrow(df))
dim(binaryMatrix)

# binaryMatrix <- matrix( rep( 0, len= length(table_users$id)), nrow = length(table_users$userIds))

binaryMatrix[is.na(binaryMatrix)] <- 0
tail(binaryMatrix)
image(binaryMatrix)

ratings_matrix <- as(binaryMatrix, "binaryRatingMatrix")
ratings_matrix
image(ratings_matrix, main = "Binary rating matrix")

# Checking first 100 rows of purchases
image(ratings_matrix[1:100, ], main = "Binary rating matrix")
dim(ratings_matrix)
#image(ratings_matrix, main = "Binary rating matrix")


# What is my Sparsity ?
# Sparsity  = Number of Empty cells / Total Number of cells.
sum( df==1 ) 
sum(is.na(ratings_matrix))
sapply(df, function(x) sum(x==0))
library(tidyverse)

dff <- (map(df, ~sum(is.na(.))))
totalEmptyCell <- sum(sapply(dff, function(x) sum(x)))
totalCell <- dim(df)[1] * dim(df)[2]
Sparsity <- totalEmptyCell / totalCell
Sparsity
class(dff)

####### Finsihed Binary Ratings  ###########


####### Startign my IBFC   ###########

which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]

# model_parameters <- list(method = "Jaccard",k=30)
model_parameters <- NULL

recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = model_parameters)

# about the model
# someInfo about the model
model_details <- getModel(recc_model)
model_details$description
names(model_details)
dim(recc_model@model$sim)
image(recc_model@model$sim, main = "Heatmap of the first rows and all columns")
range(recc_model@model$sim)

# prediction data
dist_ratings <- as(recc_model@model$sim, "matrix")



n_recommended <- 6
recc_predicted <- predict(object = recc_model,newdata = recc_data_test, n = n_recommended)
slotNames(recc_predicted)
recc_predicted

head(recc_predicted@itemLabels)

table_labels <- data.frame(id = recc_predicted@itemLabels)
table_labels <- merge(table_labels, table_items,
                      by = "id", all.x = TRUE, all.y = FALSE,
                      sort = FALSE)


# recommendation for the second user:
recommendation_indexes_for <- recc_predicted@items[[2]]
# These six are recommended to him: str(recommendation_indexes_for) : 25 34 35 38 48 52

# get actual tour id
movie_user_2 <- recc_predicted@itemLabels[recommendation_indexes_for]
movie_user_2 <- as.numeric(movie_user_2)
# now getting tour name
slotNames(movie_user_2)
movie_names <- (unique(purchase[purchase$id %in% movie_user_2,]$name))
str(movie_names)
# recc_predicted@items[1:5]

# getting recommendation for all users
# Removing null predictions.

preds  <- recc_predicted@items[is.list(recc_predicted@items)]
tail(preds)

length(recc_predicted@items)

filter(recc_predicted@items,length(x) > 0)



# Removing null predictions.

newV <- mapply(function(x) { if (length(x) > 0) return(TRUE) else return(FALSE) }, recc_predicted@items)
filteredPred <- recc_predicted@items[newV == TRUE]
filteredPred


recc_matrix <- sapply(filteredPred, function(x){
  unique(purchase[purchase$id %in% x])
  
})

unique(recc_matrix)
length(recc_matrix)
head(recc_matrix)

dim(recc_matrix)
str(recc_matrix)
unique(recc_matrix[[1]])

number_of_items <- factor(table(recc_matrix))

number_of_items <- factor(recc_matrix)

# qplot(recc_predicted@ratings) + geom_histogram(binwidth = 1) + ggtitle("Distribution of movies per user")
# qplot(factor(table(recc_matrix$name))) +geom_histogram(binwidth = 10)  + ggtitle("Distribution of movies per user")
ggplot(purchase,x ="sad") +geom_histogram(binwidth = 10)  + ggtitle("Distribution of movies per user")

################## Lets evaluate ##################

class(ratings_movies)

recc_predicted

results <- evaluate(x = recc_predicted, data=eval_sets, method = recc_model, n = seq(10, 100, 10))
class(results)


movies2 <- as(recc_predicted@items, "transactions") 

eval_accuracy <- calcPredictionAccuracy(x = recc_predicted, data = recc_data_test, byUser =TRUE)
head(eval_accuracy)

################## ROC ##################


items_to_keep <- 15
rating_threshold <- 3
n_eval <- 1
eval_sets <- evaluationScheme(data = ratings_movies, method = "split",
                              train = percentage_training, given = items_to_keep, goodRating =
                                rating_threshold, k = n_eval)


rmat <- as.matrix(data.frame(purchase$id,purchase$userIds))

str(recc_data_train)
rmat <- as.matrix(recc_data_train)
rmat <- as(ratings_matrix,"realRatingMatrix")
dim(rmat)

set.seed(1)
## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(recc_data_train, method="split", train=0.9,
                      k=1, given=0,goodRating=0)
e
## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")
## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"))
str(p@data)
str(p)

image(p@items)
#p@data@x[p@data@x[] >= 1] <- 1
#p@data@x[p@data@x[] < 1] <- 0
## compute error metrics averaged per user and then averaged over all
## recommendations
#rm(IBCF_N_E, IBCF_C_E, IBCF_Z_E)

ms <- calcPredictionAccuracy(p@ratings, getData(e, "unknown"))
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser=TRUE))



recommenderRegistry$get_entry_names()
# POPULAR_binaryRatingMatrix
# RANDOM_binaryRatingMatrix
# ALS_implicit_binaryRatingMatrix
recommenderRegistry$get_entry("POPULAR", dataType = "binaryRatingMatrix")

ratings_matrix <- as(binaryMatrix, "binaryRatingMatrix")

unique(binaryMatrix)
str(ratings_matrix)


####################### Some Tried Algorithms ##############

## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(ratings_matrix, method="split", train=0.9,
                      k=10, given=0, goodRating=1)

e <- evaluationScheme(ratings_matrix, method="cross-validation",
                            k=10, given=0,goodRating=1)
e
r <- Recommender(getData(e, "known"), "IBCF")
r
p <- predict(r, getData(e, "known"))
p
calcPredictionAccuracy(x=p,data=getData(e, "unknown"),given=p@n)
recommenderlab::calcPredictionAccuracy(x=p,data=getData(e, "unknown"),byUser=FALSE,given=p@n)

################# A test #################
data("MSWeb")

MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 20)
dim(MSWeb10)
image(MSWeb10)
## create an evaluation scheme
es <- evaluationScheme(MSWeb10, method="cross-validation",
                       k=4, given=3)
## run evaluation
## now run evaluate with a list
algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL)
)
evlist <- evaluate(es, algorithms, n=c(1,3,5,10))
avg(evlist)
plot(evlist, legend="topright")

which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]

dim(recc_data_train)
image(recc_data_train)
es <- evaluationScheme(ratings_matrix, method="cross-validation",
                       k=4, given=1)
ev <- evaluate(es, "POPULAR")

avg(ev)
plot(ev, annotate = TRUE)

# now run with different algorithms

algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL)
)
evlist <- evaluate(es, algorithms, n=c(1,3,5,10))
avg(evlist)
plot(evlist, legend="topright")




