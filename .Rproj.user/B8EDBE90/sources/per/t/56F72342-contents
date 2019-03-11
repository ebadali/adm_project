
library("rjson")
result <- fromJSON(file = "allInventory.json", method='C')
print(result)
head(result)
json_fileinv <- lapply(result, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
print(json_fileinv)
jsnDatainv <- do.call("rbind", json_fileinv)

json_data_frame <- as.data.frame(jsnDatainv,check.rows = FALSE, check.names = FALSE)


install.packages("RJSONIO")
install.packages("RCurl")
library(RJSONIO)
library(RCurl)

jsonData = RJSONIO::fromJSON("allInventory.json",simplifyVector = FALSE)

head(jsonData)
View(jsonData)
View(jsonData)

# Tour DataFrame
tourDataFrame <-data.frame(id=character(),name=character(),cat=character(),cost=character(),company=character() ,stringsAsFactors=FALSE)

for(i in 1:length(jsonData)) {
  temp <- jsonData[[i]]

  companyName <- temp$publishedContent$companyInfo["company_name"]
  #print(companyName)
  #companyName <- if(is.na(companyName)) "Unknown" else companyName
  tourDataFrame[nrow(tourDataFrame) + 1,] = list(id=temp$publishedContent$product$tour_id,
                                                 name=temp$publishedContent$product$tour_name,
                                                 cat=temp$publishedContent$product$cat,
                                                 cost=temp$publishedContent$product$price[[1]]$cost,
                                                 company=companyName)
}

print(tourDataFrame)
