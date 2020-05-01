#' Reads in and prepares COVID data
#'
#' @export
confirmed_cases_data <- function()
  
{
  
  CovidConfirmedCases <- PlottingCountyData <- vroom::vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  CovidConfirmedCases <- CovidConfirmedCases[colSums(!is.na(CovidConfirmedCases)) > 0]
  CovidConfirmedCases <- CovidConfirmedCases[,c(5, 12:ncol(CovidConfirmedCases))]
  colnames(CovidConfirmedCases)[1]<-"CountyFIPS"

  CovidDeaths<-vroom::vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  CovidDeaths<-CovidDeaths[,c(5, 13:ncol(CovidDeaths))]
  colnames(CovidDeaths)[1]<-"CountyFIPS"
  
  state_info = state_info_data()
  
  CovidConfirmedCases <- merge(state_info,
                               CovidConfirmedCases,
                               by = "CountyFIPS")
  
  CovidDeaths <- merge(state_info,
                       CovidDeaths,
                       by = "CountyFIPS")

  CovidConfirmedCases[is.na(CovidConfirmedCases)] <- 0
  colnames(CovidConfirmedCases)[1] <- "CountyFIPS"
  
  
  colnames(CovidDeaths)[1]<-"CountyFIPS"
  CovidDeaths[is.na(CovidDeaths)]<-0
  
  # Is this the right place?
  NationalDataTable <- CovidConfirmedCases
    
  # Calculate county case doubling rate for most recent day
  CovidConfirmedCases <- dplyr::filter(CovidConfirmedCases, CountyFIPS != 0)
  CovidConfirmedCases <- head(CovidConfirmedCases,-1)

  # Get rid of days with incorrect cumulative reporting of zeros after reported cases have been seen
  for(i in 6:(ncol(CovidConfirmedCases))){
    
      CovidConfirmedCases[,i] = ifelse(CovidConfirmedCases[,i] < CovidConfirmedCases[,(i-1)],
                                       CovidConfirmedCases[,(i-1)],
                                       CovidConfirmedCases[,i])
    
  }

  currCount = 0

  v <- double(ncol(CovidConfirmedCases))

for(i in 1:nrow(CovidConfirmedCases)){
    
    j = 0
    cases = CovidConfirmedCases[i,ncol(CovidConfirmedCases)]
    
    if(cases != 0){
        
       while (cases/2 < CovidConfirmedCases[i,ncol(CovidConfirmedCases)-j])
         {
             j = j + 1
         }
         
         days = j
        
    } else {
      
        days = 0
        
    }
    
    v[i] <- days
    
}

CovidConfirmedCasesRate <- cbind(CovidConfirmedCases,v)

# Pick back up with PlottingCountyData
  PlottingCountyData<-PlottingCountyData[colSums(!is.na(PlottingCountyData)) > 0]
  PlottingCountyData<-data.frame(PlottingCountyData[,5],rev(PlottingCountyData)[,1])
  colnames(PlottingCountyData)<-c("GEOID","Cases")
# Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
  county_df<-counties(state = NULL, cb = TRUE, resolution = "5m")

# Pick-up with NationalDataTable
  NationalDataTable$State<-as.factor(NationalDataTable$State)
  NationalDataTable<-NationalDataTable[,-c(1,2,4)]
  NationalDataTable<-stats::aggregate(.~State, NationalDataTable, sum)
  RateofCovidChange<-rev(NationalDataTable)[c(1:7)]
  RateofCovidChange<-ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)

  NationalDeathTable<-CovidDeaths
  NationalDeathTable$State<-as.factor(NationalDeathTable$State)
  NationalDeathTable<-NationalDeathTable[,-c(1,2,4)]
  NationalDeathTable<-stats::aggregate(.~State, NationalDeathTable, sum)
  RateofDeathChange<-rev(NationalDeathTable)[c(1:7)]
  RateofDeathChange<-ceiling(rowSums(RateofDeathChange[1:6]-RateofDeathChange[2:7])/6)

  NationalDataTable<-data.frame(NationalDataTable$State, 
                                NationalDataTable[,length(NationalDataTable)],
                                RateofCovidChange, 
                                NationalDeathTable[,length(NationalDeathTable)], 
                                RateofDeathChange)
  
  colnames(NationalDataTable)<-c("State",
                                 "Total Cases",
                                 "Average New Cases Per Day", 
                                 "Total Deaths",
                                 "Average New Deaths Per Day")
  
  NationalDataTable$`Cases Per 100,000 People`<-c(731545,4903185,3017825,
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
  NationalDataTable$`Cases Per 100,000 People`<-round(NationalDataTable$`Total Cases`/(NationalDataTable$`Cases Per 100,000 People`/100000))

  return(list(CovidConfirmedCases = CovidConfirmedCases,
              CovidDeaths = CovidDeaths,
              PlottingCountyData = PlottingCountyData,
              NationalDataTable = NationalDataTable,
              county_df = county_df,
              RateofDeathChange = RateofDeathChange,
              RateofCovidChange = RateofCovidChange,
              CovidConfirmedCasesRate = CovidConfirmedCasesRate))
  
}
