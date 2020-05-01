#' Build a data frame of state information
#' 
#' 
#' @import tigris
state_info_data <- function(){
  
   # load fips_codes object from tigris package
     data("fips_codes", package = "tigris")
     
   # get state infomration based on county fips code
     state_info = fips_codes[c(5,1,2,4)]
     
   # combine state and county codes to get CountyFIPS code
     state_info$county_code = paste(state_info$state_code,
                                  state_info$county_code, 
                                  sep = "")
      
   # make countyFIPS code a numeric value
     state_info[, c(3,4)] <- sapply(state_info[, c(3,4)], 
                                    as.numeric)
     
   # names for headers
     colnames(state_info)[1:4] = c("County Name", 
                                   "State",
                                   "stateFIPS",
                                   "CountyFIPS")
   
     return(state_info)
  
}
