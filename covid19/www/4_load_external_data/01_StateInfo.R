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
  
  # curdate <- Sys.Date() - 2  
  # curdate <- format(curdate, format="%m-%d-%Y")
  # filename1 <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",curdate,".csv")
  # filename2 <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",curdate,".csv")  
  # CovidActiveCases1 = as.data.frame(data.table::fread(filename1))
  # CovidActiveCases2 = as.data.frame(data.table::fread(filename2))
  # CovidActiveCases1$Last_Update <- strptime(as.character(CovidActiveCases1$Last_Update), "%Y-%m-%d")
  # CovidActiveCases2$Last_Update <- strptime(as.character(CovidActiveCases2$Last_Update), "%Y-%m-%d")
  # CovidActiveCases1 <- merge(CovidActiveCases1,StateList,by.x = names(CovidActiveCases1)[3],by.y = names(StateList)[1])
  # CovidActiveCases2 <- merge(CovidActiveCases2,StateList,by.x = names(CovidActiveCases2)[1],by.y = names(StateList)[1])
  # names(CovidActiveCases1)[names(CovidActiveCases1)=="state.abb"] <- "State"
  # names(CovidActiveCases2)[names(CovidActiveCases2)=="state.abb"] <- "State" 


  #Using JHU Github folder for US projections
  #Can also use this here for world projections "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports"
  URL <- "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us"
  
  #capture full page
  pg <- read_html(URL)
  
  #capture just the "clickable" extensions
  extensions <- html_attr(html_nodes(pg, "a"), "href")
  
  #You can look at this data frame for all extensions on the page
  #Looking here the most recent update is 17 rows away from the bottom rather than just 1 row
  #There were additional links on the page after the "Read.me" file (generic links at the bottom of the page not files)
  #As long as the github page set up does not change the most recent file should consistently be 17 rows away from the last row
  extensions <- as.data.frame(extensions)
  
  #Make into data frame and pull the 17th row from the bottom which should consistently be our most recent file
  Most_Recent_Extension <- as.data.frame(extensions[(nrow(extensions)-20), ])
  
  #We could attempt to follow extensions and go to the next two pages down the line and pull the raw file
  #However its probably easier to just update the "raw" link with the most recently populated date from the available extensions
  #Will just capture the most recent date in the folder rather than attempt to follow links
  
  #Convert to character to enable use of left/right functions
  #This is a weird column name could rename if desired
  Most_Recent_Extension$'extensions[(nrow(extensions) - 20), ]' <- as.character(Most_Recent_Extension$'extensions[(nrow(extensions) - 20), ]')
  
  #Capture date from most recent file in folder
  righttext <-  substr(Most_Recent_Extension[1,], nchar(Most_Recent_Extension[1,]) - (14-1), nchar(Most_Recent_Extension[1,]))   
  lefttext <- substr(righttext, 1, 10)

  #Add this date to the "raw" link
  #In the future we could also explore going down the extensions to arrive at the raw file but its easy enough to call it out here
  Most_Recent_Link1 <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",lefttext,".csv")
  Most_Recent_Link2 <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",lefttext,".csv")
  
  #Read in the most recent data
  try(CovidActiveCases1 <- read.csv(Most_Recent_Link1))
  try(CovidActiveCases2 <- read.csv(Most_Recent_Link2))
  CovidActiveCases1$Last_Update <- strptime(as.character(CovidActiveCases1$Last_Update), "%Y-%m-%d")
  CovidActiveCases2$Last_Update <- strptime(as.character(CovidActiveCases2$Last_Update), "%Y-%m-%d")
  CovidActiveCases1 <- merge(CovidActiveCases1,StateList,by.x = names(CovidActiveCases1)[3],by.y = names(StateList)[1])
  CovidActiveCases2 <- merge(CovidActiveCases2,StateList,by.x = names(CovidActiveCases2)[1],by.y = names(StateList)[1])
  names(CovidActiveCases1)[names(CovidActiveCases1)=="state.abb"] <- "State"
  names(CovidActiveCases2)[names(CovidActiveCases2)=="state.abb"] <- "State"
  