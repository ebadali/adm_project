# load CSV version of jester subset


#inventory <- read.csv("inventory.csv", header = FALSE)
purchase <- read.csv("purchase.csv", header = TRUE)
#dim(inventory)
dim(purchase)

# load  
# head(inventory)
head(purchase)

slotNames(purchase)



# Dont need 'cat', 'name' and 'cost'
purchase$cat <- NULL
purchase$name <- NULL
purchase$cost <- NULL

object.size(as(purchase,"matrix"))

head(as.matrix(purchase))

# image(as.matrix(purchase), main = "User similarity")

userIds <- 1:length(purchase$id) 
head(userIds)

purchase$userIds <- as.numeric(userIds)

head(as.matrix(purchase))

# renaming the coloumn
names(purchase)[1] <- "tourIds"

purchase$tourIds <- as.character(purchase$tourIds)
head(as.matrix(purchase))
summary(purchase)


library("recommenderlab")
library("data.table")
library("ggplot2")
library("countrycode")
purchase <- read.csv("purchase.csv", header = TRUE)

purchase <- data.table(purchase)
purchase$cat <- NULL
purchase$name <- NULL
purchase$cost <- NULL
head(as.matrix(purchase))

# Assigning unique ids to the column. Factoring long varying tour ids to numeric integers ids
purchase$id <- as.numeric(as.factor(purchase$id))


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
table_users <- purchase

head(table_users)

####### Generating Binary Ratings  ###########

# custom method of generating the binary rating matrix
nrows = length(table_users$userIds)
df <- data.frame(matrix(ncol = length(unique(table_users$id)), nrow  = nrows))
x <- c(unique(table_users$id))
length(x)
colnames(df) <- x
colnames(df)
dim(df)

for (i in 0:nrows) {
  row <- table_users[i,]
  indexOfColumn <- which(colnames(df)==row$userIds)
  # indexOfColumn <- grep(row$userIds, colnames(df))
  
  df[i,indexOfColumn] <- 1
  print(row)
}
unique(which(colnames(df)==1))



binaryMatrix <- matrix(as.numeric(unlist(df)),nrow=nrow(df))


# binaryMatrix <- matrix( rep( 0, len= length(table_users$id)), nrow = length(table_users$userIds))

binaryMatrix[is.na(binaryMatrix)] <- 0
image(binaryMatrix)

ratings_matrix <- as(binaryMatrix, "binaryRatingMatrix")
ratings_matrix
image(ratings_matrix[1:50, 1:50], main = "Binary rating matrix")
#image(ratings_matrix, main = "Binary rating matrix")
####### Finsihed Binary Ratings  ###########


####### Startign my IBFC   ###########

which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]


recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))
image(recc_model@model$sim)

