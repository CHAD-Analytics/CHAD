#'
#'
#' @importFrom tigris counties
#' @importFrom data.table fread
#' @importFrom stats aggregate
covid_data <- function(cases_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                       deaths_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

{
  
 ### Read in state info data
 #--------------------------
 state_info_data()  

 ### Prep COVID Confirmed Cases data
 #----------------------------------
   
 # Read in data   
   confirmed_cases = data.frame(data.table::fread(cases_url))
   
 # Save data as plotting_county_data for use below 
   plotting_county_data = confirmed_cases
   
 # Format columns and column names
   confirmed_cases = confirmed_cases[colSums(!is.na(confirmed_cases)) > 0]
   colnames(confirmed_cases)[1] = "CountyFIPS"
   
 # Merge state_info data with confirmed cases
   confirmed_cases = merge(state_info,
                           confirmed_cases,
                           by = "CountyFIPS")
   
   confirmed_cases[is.na(confirmed_cases)] <- 0
   colnames(confirmed_cases)[1] <- "CountyFIPS"
   confirmed_cases <- dplyr::filter(confirmed_cases, CountyFIPS != 0)
   confirmed_cases <- head(confirmed_cases,-1)

  
 ### Prep COVID Confirmed Deaths data
 #-----------------------------------
    
 # Read in data   
   deaths = data.frame(data.table::fread(deaths_url))
   deaths = deaths[,c(5, 13:ncol(deaths))]
   colnames(deaths)[1] = "CountyFIPS"
   
 # Merge state_info data with confirmed cases
   deaths = merge(state_info,
                  deaths,
                  by = "CountyFIPS")
   
 ### Prep IHME projection data 
 #-------------------------------   
 ihme_model_data()

 ### Prep plotting_county_data - object created above
   
 # 
   plotting_county_data = plotting_county_data[colSums(!is.na(PlottingCountyData)) > 0]
   plotting_county_data = data.frame(plotting_county_data[,5],
                                     rev(plotting_county_data)[,1])
   
   colnames(plotting_county_data) = c("GEOID","Cases")
   
 # Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
   county_df = tigris::counties(state = NULL, 
                                cb = TRUE, 
                                resolution = "5m")
   
 ### Create National Data table on summary page
 #---------------------------------------------

  national_data_table = confirmed_cases
  national_data_table$State = as.factor(national_data_table$State)
  national_data_table = national_data_table[,-c(1,2,4)]
  national_data_table = stats::aggregate(.~State, national_data_table, sum)
  RateofCovidChange = rev(national_data_table)[c(1:7)]
  RateofCovidChange = ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)
  
  national_death_table = deaths
  national_death_table$State = as.factor(national_death_table$State)
  national_death_table = national_death_table[,-c(1,2,4)]
  national_death_table = stats::aggregate(.~State, national_death_table, sum)
  RateofDeathChange = rev(national_death_table)[c(1:7)]
  RateofDeathChange = ceiling(rowSums(RateofDeathChange[1:6]-RateofDeathChange[2:7])/6)
  
  national_data_table = data.frame(national_data_table$State, 
                                   national_data_table[,length(national_data_table)],
                                   RateofCovidChange, 
                                   national_death_table[,length(national_death_table)], 
                                   RateofDeathChange)
  
  colnames(national_data_table) = c("State",
                                    "Total Cases",
                                    "Average New Cases Per Day", 
                                    "Total Deaths",
                                    "Average New Deaths Per Day")
  
  national_data_table$`Cases Per 100,000 People` = c(731545,4903185,3017825,
                                                     7278717,39512223,5758736,
                                                     3565287,705749,973764,
                                                     21477737,10617423,1415872,
                                                     3155070,1787065,12671821,
                                                     6732219,2913314,4467673,
                                                     4648794,6949503,6045680,
                                                     1344212,9986857,5639632,
                                                     6137428,2976149,1068778,
                                                     10488084,762062,1934408,
                                                     1359711,8882190,2096829,
                                                     3080156,19453561,11689100,
                                                     3956971,4217737,12801989,
                                                     1059361,5148714,884659,
                                                     6833174,28995881,3205958,
                                                     8535519,623989,7614893,
                                                     5822434,1792147,578759)
  
  national_data_table$`Cases Per 100,000 People` = round(national_data_table$`Total Cases` / (national_data_table$`Cases Per 100,000 People` / 100000))



}
