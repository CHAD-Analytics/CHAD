# # #Read in IHME data for projecting data in the future
# zipdf <- unzip("www/4_load_external_data/data_files/ihme-covid19.zip", list = TRUE)
# # csv_file <- zipdf$Name[tolower(tools::file_ext(zipdf$Name)) == "csv"]
# csv_file<-paste(Hospitalization_all_locs.csv)
# IHME_Model <- vroom::vroom(unzip("www/4_load_external_data/data_files/ihme-covid19.zip", csv_file),
#                             col_names = T,
#                             delim = ",")


#States/regions are not tied to country names, need to merge with county info
IHME_Model<-vroom::vroom("www/4_load_external_data/data_files/Hospitalization_all_locs.csv")


IHME_Model$date <- as.Date(IHME_Model$date,
                           format = "%Y-%m-%d")

by.x.name = names(IHME_Model)[grep("loc",names(IHME_Model),fixed=TRUE)[1]]

# state2abbr(state)
# 
# #Left join keeps all of the countries but still need to replace state names with abbreviations
# IHME_Model <- left_join(IHME_Model,
#                         StateList,  # defined in 1_StateInfo.R
#                         by.x = names(IHME_Model[c(by.x.name)]),
#                         by.y = names(StateList)[1])
 
 
IHME_Model <- merge(IHME_Model,
                     StateList,  # defined in 1_StateInfo.R
                     by.x = names(IHME_Model[c(by.x.name)]),
                     by.y = names(StateList)[1])
 
names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"
 
closeAllConnections()
