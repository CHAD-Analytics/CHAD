#Read in UT data for projecting data in the future

#UT_Model<-read.csv("data/UT-COVID19-states-forecast-latest.csv")
UT_Model<-vroom::vroom("https://raw.githubusercontent.com/UT-Covid/USmortality/master/forecasts/UT-COVID19-states-forecast-latest.csv")
UT_Model$date <- as.Date(UT_Model$date, 
                           format = "%Y-%m-%d")

by.x.name = names(UT_Model)[grep("loc",names(UT_Model),fixed=TRUE)[1]]
UT_Model <- merge(UT_Model, 
                  StateList, 
                  by.x = names(UT_Model)[1], 
                  by.y = names(StateList)[1])
names(UT_Model)[names(UT_Model)=="state.abb"] <- "State"

closeAllConnections()