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
