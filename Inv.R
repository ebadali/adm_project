library("rjson")
# sink("out", append=FALSE, split=FALSE)


json_fileinv = fromJSON(file = "./allinventory.json")

View(json_fileinv)
#json_fileinv <- lapply(result_inv, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})


tourDataFrame <-data.frame(id=character(),name=character(),cat=character(),cost=character(),company=character() ,stringsAsFactors=FALSE)

for(i in 1:length(json_fileinv)) {
  temp <- json_fileinv[[i]]
  
  companyName <- temp$publishedContent$companyInfo["company_name"]
  #print(companyName)
  #companyName <- if(is.na(companyName)) "Unknown" else companyName
  tourDataFrame[nrow(tourDataFrame) + 1,] = list(id=temp$publishedContent$product$tour_id,
                                                 name=temp$publishedContent$product$tour_name,
                                                 cat=temp$publishedContent$product$cat,
                                                 cost=temp$publishedContent$product$price[[1]]$cost,
                                                 company=companyName)
}

# Tour Data is in the frame
print(tourDataFrame)

tourDataFrame <- apply(tourDataFrame,2,as.character)
write.csv(tourDataFrame,"inventory.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")


json_filepurchase = fromJSON(file = "./allPurchase.json")
View(json_filepurchase)

purchaseDataFrame <-data.frame(id=character(),name=character(),cat=character(),cost=character(),stringsAsFactors=FALSE)

for(i in 1:length(json_filepurchase)) {
  temp <- json_filepurchase[[i]]
  
  companyName <- temp$tourid
  print(companyName)
  purchaseDataFrame[nrow(purchaseDataFrame) + 1,] = list(id=temp$tourid,
                                                 name=temp$tour_detail$tourname,
                                                 cat=temp$tour_detail$cat,
                                                 cost=temp$tour_detail$total)
}

# All purchase data.
print(purchaseDataFrame)
purchaseDataFrame <- apply(purchaseDataFrame,2,as.character)
write.csv(purchaseDataFrame,"purchase.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")



jsnDatainv <- do.call("rbind", json_fileinv)
json_data_frameinv <- as.data.frame(jsnDatainv, check.rows = FALSE, check.names = FALSE)
print(json_data_frameinv$publishedContent$tags)
print(json_data_frameinv$product$tags)

aa <- as.data.frame(json_data_frameinv$publishedContent, check.rows = FALSE, check.names =FALSE )
#json_data_frame <- as.data.frame(jsnData)
json_data_frameinv
write.csv(json_data_frameinv,"inv.csv")

Parameters_inv <- data.frame(json_data_frameinv$publishedContent.product.tour_id, json_data_frameinv$publishedContent.product.location, json_data_frameinv$publishedContent.product.tour_name)
View(Parameters_inv)

