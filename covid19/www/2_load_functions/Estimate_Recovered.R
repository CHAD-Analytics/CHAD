Estimate_Recovered <- function(CONUSSelect,ChosenBase,IncludedCounties){
  
  ##Uncomment to test plot function without running the app
  #i<-80
  # ChosenBase = AFBaseLocations$Base[i]
  # CONUSSelect <- "CONUS"
  # ChosenBase = "Hanscom AFB"
  # #CONUSSelect <- "OCONUS"
  # #ChosenBase = "Andersen AFB"
  # SocialDistance = 15
  # DaysProjected = 30
  # HospitalInfo$DistanceMiles = himd[,as.character(ChosenBase)]
  # IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= 50))
  # IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
  # CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
  # value = NULL
  # IncludedCounties<-GetCounties(ChosenBase,50,value,value)
  ###

    
    #Find counties in radius
    CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    
    #Compute cumlative cases and deaths in selected counties
    CumDailyCovid<-colSums(CovidCountiesCases[,5:length(CovidCountiesCases)])
    CumDailyDeaths<-colSums(CovidCountiesDeath[5:length(CovidCountiesDeath)])
    
    #Estimate recovered from cases and deaths
    DailyCases = CumDailyCovid[2:length(CumDailyCovid)] - CumDailyCovid[1:(length(CumDailyCovid)-1)]
    DailyDeaths = CumDailyDeaths[2:length(CumDailyDeaths)] - CumDailyDeaths[1:(length(CumDailyDeaths)-1)]
    DailyRecovered = DailyCases[1:(length(CumDailyCovid)-5)] - DailyDeaths[5:length(DailyDeaths)]
    CumRecovered = cumsum(DailyRecovered)
    cumcount<-nrow(as.data.frame(CumRecovered))
    CRec <- CumRecovered[cumcount]
    CRec <- substr(CRec,1,9)
    CRec <- as.numeric(CRec)
  
  # #Find counties in radius
  # if (CONUSSelect == "CONUS"){
  #   CovidCountiesCases<-subset(CovidActiveCases1, FIPS %in% IncludedCounties$FIPS)
  #   RC <- CovidCountiesCases$Recovered      
  #   RC <- sum(RC)
  # } else {
  #   if (IncludedCounties$State == "HI" || IncludedCounties$State == "AK" || IncludedCounties$State == "PR") {
  #     CovidCountiesCases<-subset(CovidActiveCases1, FIPS %in% IncludedCounties$FIPS)
  #     RC <- CovidCountiesCases$Active  
  #     RC <- sum(AC)        
  #   } else {
  #     CovidCountiesCases<-subset(GlobalActive, FIPS %in% IncludedCounties$FIPS) 
  #     RC <- CovidCountiesCases$total_recovered
  #     RC <- sum(RC)
  #   }
  # }  

  Final <- CRec  
  
}
