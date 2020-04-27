# Import AMC model data
temp_model <- fromJSON("www/3_load_external_data/data_files/shinyjson.json")

json_file <- lapply(temp_model, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
AMC_model<-as.data.frame(json_file)

names(AMC_model) <- gsub("\\.", " ", names(AMC_model))

#Renaming the first empty column to date
AMC_model <- cbind(rownames(AMC_model), AMC_model)
rownames(AMC_model) <- NULL

colnames(AMC_model)[1] <- "TypeDate"

AMC_model <- rev(cSplit(AMC_model, "TypeDate", "."))

colnames(AMC_model)[1] <- "DataDate"
colnames(AMC_model)[2] <- "DataType"

AMC_model$DataDate <- as.Date(AMC_model$DataDate)
