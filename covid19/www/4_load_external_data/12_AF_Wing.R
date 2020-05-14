
# AFNAFS = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/NAF.csv")
# AFNAFS = vroom::vroom("www/4_load_external_data/data_files/NAF.csv")
# AFNAFS = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/NAF.csv")
# AFNAFS = vroom::vroom("www/4_load_external_data/data_files/NAF.csv")
# NAFList <- sort(unique(AFNAFS$NAF), decreasing = FALSE)
# AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)
# WingList <- sort(unique(AFWings$Wing), decreasing = FALSE)
# WingList <- c("All",WingList)

# ##Constructing the cimdtest and himdtest matrices
# AFBaseLocations2 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/AllBaseList.csv")
# AFBaseLocations2<-data.frame(AFBaseLocations2) 
# AFBaseLocations2$Lat <- as.numeric(AFBaseLocations2$Lat)
# AFBaseLocations2$Long <- as.numeric(AFBaseLocations2$Long)
#  
# 
# AFBaseLocations<-merge(AFBaseLocations2,CountyInfo, by.x = c("State2","County"), by.y = c("State","County"))
# #AFBaseLocations<-AFBaseLocations[!duplicated(AFBaseLocations$'Location Name'), ]
# #only keep the useful columns
# AFBaseLocations<-data.frame(AFBaseLocations$'Location Name',AFBaseLocations$State2,
#                              AFBaseLocations$County,AFBaseLocations$Country1,AFBaseLocations$Branch,
#                              AFBaseLocations$Operational,AFBaseLocations$Lat,AFBaseLocations$Long,
#                              AFBaseLocations$'Major Command',AFBaseLocations$FIPS)
# 
# colnames(AFBaseLocations)<-c("Base","State","County","Country","Branch","Operational","Lat","Long","Major Command","FIPS")
# 
# x<-data.frame(AFBaseLocations$Long,AFBaseLocations$Lat)
# y<-data.frame(CountyInfo$Longitude,CountyInfo$Latitude)
# cimd <- distm(y, x, fun=distHaversine)/1609.34
# colnames(cimd)<- AFBaseLocations$Base
# 
# x<-data.frame(AFBaseLocations$Long,AFBaseLocations$Lat)
# y<-data.frame(HospitalInfo$LONGITUDE,HospitalInfo$LATITUDE)
# himd <- distm(y, x, fun=distHaversine)/1609.34
# colnames(himd)<- AFBaseLocations$Base
# 
# write.csv(cimd,"C:/Users/taylo/Documents/CHAD/cimd.csv", row.names = FALSE)
# write.csv(himd,"C:/Users/taylo/Documents/CHAD/himd.csv", row.names = FALSE)
# write.csv(AFBaseLocations,"C:/Users/taylo/Documents/CHAD/AFBaseLocations.csv", row.names = FALSE)
#write.csv(GlobalData,"C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/GlobalData.csv", row.names = FALSE)


# setwd("C:/Users/taylo/Documents/CHAD/covid19/www/3_load_local_data/")
# save(cimd, file = "cimd.rda")
# save(himd, file = "himd.rda")
# save(AFBaseLocations, file = "AFBaseLocations.rda")
# save(AFNAFS, file = "AFNAS.rda")
cimd<-vroom::vroom("www/4_load_external_data/data_files/cimd.csv")
himd<-vroom::vroom("www/4_load_external_data/data_files/himd.csv")
AFBaseLocations<-vroom::vroom("www/4_load_external_data/data_files/AFBaseLocations.csv")
AFNAFS<-vroom::vroom("www/4_load_external_data/data_files/NAF.csv")

# setwd("C:/Users/taylo/Documents/CHAD/covid19/www/3_load_local_data/")
# save(cimdtest, file = "cimd.rda")
# save(himdtest, file = "himd.rda")
# save(AFBaseLocations, file = "AFBaseLocations.rda")
# save(AFNAFS, file = "AFNAS.rda")

