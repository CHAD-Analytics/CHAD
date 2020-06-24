# library(httr)
# library(jsonlite)
# 
# CountyPredTable <- setNames(data.frame(matrix(ncol = 12, nrow = 0)),
#                             c("Date","HCasesEst","HCasesEstLow","HCasesEstUp","ICUCasesEst","ICUCasesEstLow",
#                               "ICUCasesEstUp","TotalHospitalBeds","TotalICUBeds","EstHospBedsAvail","EstICUBedsAvail","FIP"))
# 
# 
# for (i in 1:3143){
#   newfip<-CountyInfo$FIPS[i]
#   CountyPred<-paste("https://open.torchinsight.com/api/data/covid-19/predictions/county/",newfip)
#   res = GET(CountyPred)
#   data = fromJSON(rawToChar(res$content))
#   
#   if (is.null(data)){
#   } else {
#       colnames(data) <- c("Date","HCasesEst","HCasesEstLow","HCasesEstUp","ICUCasesEst","ICUCasesEstLow",
#                           "ICUCasesEstUp","TotalHospitalBeds","TotalICUBeds","EstHospBedsAvail","EstICUBedsAvail")
#       data<-as.data.frame(data)
#       data$FIP<-rep(newfip,nrow(data)) 
#       CountyPredTable <- rbind(CountyPredTable,data)
#   }
# }
# 
# save(CountyPredTable, file = "Torch_Model.rda")
# closeAllConnections()