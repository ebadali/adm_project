# install.packages('servr') 
#install.packages("rjson")

library(odbc)
library("rjson")
sink("out", append=FALSE, split=FALSE)

##### Initialize Some Data #####
Hash_Key <- 'jldasnfk23493285#CASJasjdgasu'


# Lets simulate the Minimal version of Security by matching the first argument with a key

# test if there is at least one argument: if not, return an error
print(args)

args <- commandArgs(TRUE)
Key <- toString(args[1])

someFileName <- toString(args[2])

# Failte Ireland top free attractions
# Rule set
ruleSet <- args[3]
#print(ruleSet)

# Unesco sites in ireland
# Rule set
ruleSet2 <- args[4]
print(ruleSet2)

if (Key == Hash_Key) {
  print("Key Matched")
} else {
  print("Not Authorized")
}

giveToursName <- function(someFileName) {
  print(someFileName)
  print('Loading file name')
  
  # ############# 0. Loading all tours from file #############
  #PayloadFileName = 'payload.json'
  result = fromJSON(file = someFileName)
  #print(result)

  json_file <- lapply(result, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  jsnData <- do.call("rbind", json_file)
  #print(jsnData)
  json_data_frame <- as.data.frame(jsnData, check.rows = FALSE, check.names = FALSE)
  #json_data_frame <- as.data.frame(jsnData)
  print('printing frame')
  print(json_data_frame)
  
  head(json_data_frame)
  # ############# 1. Get Tour Name of all tours #############
  
  tour_names = json_data_frame$rawdata.product.tour_name
  #tour_names = json_data_frame[119]
  print('tour names')
  print(tour_names)
  print('invidividually')
  for (i in tour_names) {
    print(i)
  }
  
  # ############# 2. Add a field called weight #############
  # ############# 3. Calculate the relevence of the tour name #############
  
  
    return(json_data_frame)
}

giveToursRuleSet_one <- function(somePayload){

  result = fromJSON(somePayload)
  #print(result)
  
  json_file <- lapply(result, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  jsnData <- do.call("rbind", json_file)
  #print(jsnData)
  json_data_frame <- as.data.frame(jsnData, check.rows = FALSE, check.names = FALSE)  
  
 return(json_data_frame) 
}

giveToursRuleSet_two <- function(somePayload){
  
  result = fromJSON(somePayload)
  #print(result)
  
  json_file <- lapply(result, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  jsnData <- do.call("rbind", json_file)
  #print(jsnData)
  json_data_frame <- as.data.frame(jsnData, check.rows = FALSE, check.names = FALSE)  
  
  return(json_data_frame) 
}

print('getting tour names')
toursDataFrame <- (giveToursName(someFileName))


print('removing details.')
print('$$$$$$$$$$$$$$$$$$$$$$$$$$')

#toursDataFrame[is.na(toursDataFrame)] <- ""
#toursDataFrame$rawdata.product.details <- NULL

print(toursDataFrame$rawdata.product.tour_name)
print(toursDataFrame$rawdata.product.provider)

print("
#''''''''''''''''''''''''''''
#Cleansing / Structuring Part
#''''''''''''''''''''''''''''
")

numberOfTours <- c(1:length(toursDataFrame$rawdata.product.tour_name));

# 1. Form an strcuture of the payload
#Image Table

#filteredImages <- toursDataFrame$rawdata.product.images[toursDataFrame$rawdata.product.images != "unknown"]
filteredImages <- toursDataFrame$rawdata.product.images
#image_ids <- c(1:length(filteredImages));
imageTable = data.frame( img_id=numberOfTours,image_url=filteredImages)
print(imageTable)


#Price Table
#price_ids <- c(1:length(toursDataFrame$rawdata.product.price.label));
toursDataFrame$rawdata.product.price.id[is.na(toursDataFrame$rawdata.product.price.id)] <- "BOOYA"
#toursDataFrame$rawdata.product.price.id <- sub(NULL,"RNDS1",toursDataFrame$rawdata.product.price.id )
priceTable = data.frame(price_id=numberOfTours,identifier=toursDataFrame$rawdata.product.price.id,label=toursDataFrame$rawdata.product.price.label,cost=toursDataFrame$rawdata.product.price.cost,commission=toursDataFrame$rawdata.product.commission,commission_type=toursDataFrame$rawdata.product.commission_type)
print("----- Checcking this -----")
print(priceTable)

#Description Table
# TODO: Make description,overview and highlights utf8 encoded
descTable = data.frame( desc_id=numberOfTours,tour_name=toursDataFrame$rawdata.product.tour_name, tour_id=toursDataFrame$rawdata.product.tour_id,duration=toursDataFrame$rawdata.product.duration,weight=toursDataFrame$rawdata.weight)
#descTable = data.frame( desc_id=desc_ids,tour_name=toursDataFrame$rawdata.product.tour_name, tour_id=toursDataFrame$rawdata.product.tour_id,duration=toursDataFrame$rawdata.product.duration,description=toursDataFrame$rawdata.product.details.description,overview=toursDataFrame$rawdata.product.details.overview,highlights=toursDataFrame$rawdata.product.details.highlights)
descTable[is.na(descTable)] <- "unknown"
print(descTable)


#CompanyInfo Table
#company_ids <- c(1:length(toursDataFrame$rawdata.product.tour_name));

filteredImages <- toursDataFrame$rawdata.product.images[toursDataFrame$rawdata.product.images != "unknown"]

toursDataFrame$rawdata.companyInfo.company_name <- sub("^$","Staypal",toursDataFrame$rawdata.companyInfo.company_name )
toursDataFrame$rawdata.companyInfo.company_terms <- sub("^$","Standard",toursDataFrame$rawdata.companyInfo.company_terms )
companyTable = data.frame( company_id=numberOfTours,provider=toursDataFrame$rawdata.product.provider,name=toursDataFrame$rawdata.companyInfo.company_name, terms=toursDataFrame$rawdata.companyInfo.company_terms)
print(companyTable)

#Meta Table
MetaInfoTable = data.frame( meta_id=numberOfTours,provider=toursDataFrame$rawdata.product.provider,name=toursDataFrame$rawdata.companyInfo.company_name, terms=toursDataFrame$rawdata.companyInfo.company_terms)
print(MetaInfoTable)


# 2. Establish SSMS Connection

getDbConnector <- function(){

  
  #Driver={ODBC Driver 13 for SQL Server};Server=tcp:warehousetwo.database.windows.net,1433;Database=db_two;Uid=app@warehousetwo;Pwd={your_password_here};Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;
  con <- dbConnect(odbc(),
                   Driver = "ODBC Driver 13 for SQL Server",
                   Server = "87.44.4.153",
                   Database = "dwbi",
                   UID = "sa",
                   PWD = "sqlServerMist3rEbad1!",
                   Port = 1433)
  on.exit(dbDisconnect(con))
  return(con) 
}

#connectionObject = getDbConnector()

# connectionObject <- dbConnect(odbc(),
#                  Driver = "ODBC Driver 13 for SQL Server",
#                  Server = "159.65.19.184",
#                  Database = "dwbi",
#                  UID = "sa",
#                  PWD = "sqlServerMist3rEbad1!",
#                  Port = 1433)
# on.exit(dbDisconnect(connectionObject))

# print(connectionObject)


# 3. Push data to Tables

writeFrameToDb <- function(connectionObj,tableName,dataFrame,isOverWrite){
  
  data <- dbWriteTable(connectionObj, tableName, dataFrame, overwrite=isOverWrite)
  return (data)
}


### 3.1 Actually pushing frames to db
#### A temporary Table : descTable = data.frame(c(1:10),c(21:30))
#descTable = data.frame(c(1:100),c(201:300))
#writingResult <- writeFrameToDb(connectionObject,"TestTable_1",descTable,isOverWrite=TRUE)


write.csv(imageTable, file = "imageTable.csv",na="")
# write.csv(priceTable, file = "priceTable.csv",na="")
# write.csv(companyTable, file = "companyTable.csv",na="")
# write.csv(descTable, file = "descTable.csv",na="")


# print(writeFrameToDb(connectionObject,"RawDataDesc",descTable,isOverWrite=TRUE))
# print(writeFrameToDb(connectionObject,"RawDataImage",imageTable,isOverWrite=TRUE))
# print(writeFrameToDb(connectionObject,"RawDataPrice",priceTable,isOverWrite=TRUE))
# print(writeFrameToDb(connectionObject,"RawDataCompany",companyTable,isOverWrite=TRUE))


## Test Reading the table
#data <- dbReadTable(connectionObject, "TestTable_1")
#print(data)






