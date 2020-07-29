# # #Read in IHME data for projecting data in the future
# zipdf <- unzip("www/4_load_external_data/data_files/ihme-covid19.zip", list = TRUE)
# # csv_file <- zipdf$Name[tolower(tools::file_ext(zipdf$Name)) == "csv"]
# csv_file<-paste(Hospitalization_all_locs.csv)
# IHME_Model <- vroom::vroom(unzip("www/4_load_external_data/data_files/ihme-covid19.zip", csv_file),
#                             col_names = T,
#                             delim = ",")


#States/regions are not tied to country names, need to merge with county info
#IHME_Model <- vroom::vroom("C:/Users/taylo/Documents/CHADNew/covid19/www/4_load_external_data/data_files/Hospitalization_all_locs.csv")
IHME_Model<-vroom::vroom("www/4_load_external_data/data_files/Reference_hospitalization_all_locs.csv")  #Store URL for most recent IHME Zip File
#IHME_Model2<-vroom::vroom("www/4_load_external_data/data_files/Best_mask_hospitalization_all_locs.csv") 
#IHME_Model3<-vroom::vroom("www/4_load_external_data/data_files/Worse_hospitalization_all_locs.csv")
IHME_Summary<-vroom::vroom("www/4_load_external_data/data_files/Summary_stats_all_locs.csv")

# url <-"https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip"
# 
# #Create 2 temp files
# temp <- tempfile()
# temp2 <- tempfile()
# 
# #Download the zip file and unzip
# download.file(url, temp)
# unzip(zipfile = temp, exdir = temp2)
# 
# #Defnie path up to the latest date
# IHME_Path <- list.files(path = temp2, full.names = TRUE)
# #Ammend path with desired file name
# #Using Projected data here but could also use "Summary_stats_all_locs.csv" if you wanted that info
# IHME_Path <- paste(IHME_Path, "/Hospitalization_all_locs.csv", sep = "")
# #Stores the data
# IHME_Model <- read.csv(IHME_Path)
# #Unlinks the temp files
# unlink(c(temp, temp2))

IHME_Model$date <- as.Date(IHME_Model$date,format = "%Y-%m-%d")
#IHME_Model2$date <- as.Date(IHME_Model2$date,format = "%Y-%m-%d")
#IHME_Model3$date <- as.Date(IHME_Model3$date,format = "%Y-%m-%d")
#IHME_Summary$date <- as.Date(IHME_Summary$date,format = "%Y-%m-%d")

by.x.name = names(IHME_Model)[grep("loc",names(IHME_Model),fixed=TRUE)[1]]
#by.x.name = names(IHME_Model2)[grep("loc",names(IHME_Model2),fixed=TRUE)[1]]
#by.x.name = names(IHME_Model3)[grep("loc",names(IHME_Model3),fixed=TRUE)[1]]
by.x.name = names(IHME_Summary)[grep("loc",names(IHME_Summary),fixed=TRUE)[1]]

IHME_Model <- merge(IHME_Model,StateList, by.x = names(IHME_Model[c(by.x.name)]),by.y = names(StateList)[1],all = TRUE)
#IHME_Model2 <- merge(IHME_Model2,StateList,by.x = names(IHME_Model2[c(by.x.name)]),by.y = names(StateList)[1],all = TRUE)
#IHME_Model3 <- merge(IHME_Model3,StateList,by.x = names(IHME_Model3[c(by.x.name)]),by.y = names(StateList)[1],all = TRUE)
IHME_Summary <- merge(IHME_Summary,StateList,by.x = names(IHME_Summary[c(by.x.name)]),by.y = names(StateList)[1],all = TRUE)

names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"
#names(IHME_Model2)[names(IHME_Model2)=="state.abb"] <- "State"
#names(IHME_Model3)[names(IHME_Model3)=="state.abb"] <- "State"
names(IHME_Summary)[names(IHME_Summary)=="state.abb"] <- "State"

#IHME_Model$State <- ifelse(is.na(IHME_Model$state1), IHME_Model$location_name, toString(IHME_Model$state1))

closeAllConnections()
