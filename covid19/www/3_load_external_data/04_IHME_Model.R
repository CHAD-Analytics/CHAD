#Read in IHME data for projecting data in the future
zipdf <- unzip("www/3_load_external_data/data_files/ihme-covid19.zip", list = TRUE)
csv_file <- zipdf$Name[tolower(tools::file_ext(zipdf$Name)) == "csv"]

IHME_Model <- vroom::vroom(unz("www/3_load_external_data/data_files/ihme-covid19.zip", csv_file), 
                           col_names = T, 
                           delim = ",")

IHME_Model$date <- as.Date(IHME_Model$date, 
                           format = "%Y-%m-%d")

by.x.name = names(IHME_Model)[grep("loc",names(IHME_Model),fixed=TRUE)[1]]

IHME_Model <- merge(IHME_Model, 
                    StateList,  # defined in 1_StateInfo.R
                    by.x = names(IHME_Model)[1], 
                    by.y = names(StateList)[1])

names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"

closeAllConnections()