# AFNAFS = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/NAF.csv")
# AFNAFS = vroom::vroom("www/4_load_external_data/data_files/NAF.csv")
# NAFList <- sort(unique(AFNAFS$NAF), decreasing = FALSE)
# AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)
# WingList <- sort(unique(AFWings$Wing), decreasing = FALSE)
# WingList <- c("All",WingList)

# ##Constructing the cimdtest and himdtest matrices
# AFBaseLocations2 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/AllServiceBases.csv")
# 
# AFBaseLocations2$Lat <- as.numeric(AFBaseLocations2$Lat)
# AFBaseLocations2$Long <- as.numeric(AFBaseLocations2$Long)
# 
# x<-data.frame(AFBaseLocations2$Long,AFBaseLocations2$Lat)
# y<-data.frame(CountyInfo$Longitude,CountyInfo$Latitude)
# cimd <- distm(y, x, fun=distHaversine)/1609.34
# colnames(cimd)<- AFBaseLocations2$Base
# 
# x<-data.frame(AFBaseLocations2$Long,AFBaseLocations2$Lat)
# y<-data.frame(HospitalInfo$LONGITUDE,HospitalInfo$LATITUDE)
# himd <- distm(y, x, fun=distHaversine)/1609.34
# colnames(himd)<- AFBaseLocations2$Base
# 
# AFBaseLocations2<-data.frame(AFBaseLocations2)
# AFBaseLocations<-merge(AFBaseLocations2,CountyInfo, by.x = c("State","County"), by.y = c("State","County"))
# AFBaseLocations<-AFBaseLocations[!duplicated(AFBaseLocations$Base), ]
# #only keep the useful columns
# AFBaseLocations<-data.frame(AFBaseLocations$Base,AFBaseLocations$City,AFBaseLocations$State,
#                              AFBaseLocations$County,AFBaseLocations$Country,AFBaseLocations$Branch,
#                              AFBaseLocations$Operational,AFBaseLocations$Lat,AFBaseLocations$Long,
#                              AFBaseLocations$Major.Command,AFBaseLocations$FIPS)
# 
# colnames(AFBaseLocations)<-c("Base","City","State","County","Country","Branch","Operational","Lat","Long","Major Command","FIPS")
# 
# write.csv(cimd,"C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/cimd.csv", row.names = FALSE)
# write.csv(himd,"C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/himd.csv", row.names = FALSE)
# write.csv(AFBaseLocations,"C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/AFBaseLocations.csv", row.names = FALSE)
# 
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

