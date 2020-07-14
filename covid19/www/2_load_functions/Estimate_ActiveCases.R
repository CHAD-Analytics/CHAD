Estimate_ActiveCases <- function(CONUSSelect,ChosenBase,IncludedCounties){

  # ##Uncomment to test plot function without running the app
  # #i<-80
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
  # ###
  
  # BaseState<-dplyr::filter(AFBaseLocations, Base == toString(ChosenBase))
  # if (CONUSSelect == "CONUS"){
  #   hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
  #   TTBCounty <- sum(IncludedHospitals$BEDS)
  #   StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
  # } else {
  #   StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$Country[1]))
  # }
  # 
  # #Get regional and state populations
  # RegPop <- sum(IncludedCounties$Population)
  # StPop <- sum(StPopList$Population)
  # 
  # # Use Population ratio to scale IHME
  # PopRatio <- RegPop/StPop
  
  
  #Find counties in radius
  # if (CONUSSelect == "CONUS"){
  #     CovidCountiesCases<-subset(CovidActiveCases1, FIPS %in% IncludedCounties$FIPS)
  #     #CovidCountiesCases %>% group_by(FIPS) %>% summarize(Last_Update = max(Last_Update))
  #     AC <- CovidCountiesCases$Active  
  #     AC <- sum(AC)
  # } else {
  #     if (IncludedCounties$State == "HI" || IncludedCounties$State == "AK" || IncludedCounties$State == "PR") {
  #       CovidCountiesCases<-subset(CovidActiveCases1, FIPS %in% IncludedCounties$FIPS)
  #       AC <- CovidCountiesCases$Active  
  #       AC <- sum(AC)        
  #     } else {
  #       CovidCountiesCases<-subset(GlobalActive, FIPS %in% IncludedCounties$FIPS) 
  #       AC <- NA
  #     }
  # }
  # 
  # Final <- AC
  
  
  
  # Get recovered, total and death number first
  #Find counties in radius
  CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
  
  #Compute cumlative cases and deaths in selected counties
  CumCovid<-colSums(CovidCountiesCases[,5:length(CovidCountiesCases)])
  CumDeaths<-colSums(CovidCountiesDeath[5:length(CovidCountiesDeath)])
  
  #Estimate recovered from cases and deaths
  DailyCases = CumCovid[2:length(CumCovid)] - CumCovid[1:(length(CumCovid)-1)]
  DailyDeaths = CumDeaths[2:length(CumDeaths)] - CumDeaths[1:(length(CumDeaths)-1)]
  DailyRecovered = DailyCases[1:(length(CumCovid)-14)] - DailyDeaths[5:(length(CumDeaths)-9)]
  CumRecovered = cumsum(DailyRecovered)
  cumcount<-nrow(as.data.frame(CumRecovered))
  CRec <- CumRecovered[cumcount]
  CRec <- substr(CRec,1,9)
  CRec <- as.numeric(CRec)
  
  # Totals
  TotCases = rev(CumCovid)[1] 
  TotDeaths = rev(CumDeaths)[1]
  
  # Active
  AC = TotCases - TotDeaths - CRec
  
}
  
  
  

