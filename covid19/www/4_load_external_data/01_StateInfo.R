# load fips_codes object from tigris package
  data("fips_codes", package = "tigris")
 
# get state infomration based on county fips code
  StateInfo = fips_codes[c(5,1,2,4)]
 
# combine state and county codes to get CountyFIPS code
  StateInfo$county_code = paste(StateInfo$state_code,
                                StateInfo$county_code, 
                                sep = "")
  
# make countyFIPS code a numeric value
  StateInfo[, c(3,4)] <- sapply(StateInfo[, c(3,4)], 
                                as.numeric)
 
# names for headers
  colnames(StateInfo)[1:4] = c("County Name", 
                               "State",
                               "stateFIPS",
                               "CountyFIPS")
  
# Create StateList for use in other data sets downstream
  StateList <- data.frame(state.name, state.abb)
  
  curdate <- Sys.Date() - 1  
  curdate <- format(curdate, format="%m-%d-%Y")
  filename1 <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",curdate,".csv")
  filename2 <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",curdate,".csv")  
  CovidActiveCases1 = as.data.frame(data.table::fread(filename1))
  CovidActiveCases2 = as.data.frame(data.table::fread(filename2))
  CovidActiveCases1$Last_Update <- strptime(as.character(CovidActiveCases1$Last_Update), "%Y-%m-%d")
  CovidActiveCases2$Last_Update <- strptime(as.character(CovidActiveCases2$Last_Update), "%Y-%m-%d")
  CovidActiveCases1 <- merge(CovidActiveCases1,StateList,by.x = names(CovidActiveCases1)[3],by.y = names(StateList)[1])
  CovidActiveCases2 <- merge(CovidActiveCases2,StateList,by.x = names(CovidActiveCases2)[1],by.y = names(StateList)[1])
  names(CovidActiveCases1)[names(CovidActiveCases1)=="state.abb"] <- "State"
  names(CovidActiveCases2)[names(CovidActiveCases2)=="state.abb"] <- "State" 
