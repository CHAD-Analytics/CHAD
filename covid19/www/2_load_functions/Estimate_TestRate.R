Estimate_TestRate <- function(CONUSSelect,ChosenBase,IncludedCounties){

  # ##Uncomment to test plot function without running the app
  # #i<-80
  # #ChosenBase = AFBaseLocations$Base[i]
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
  # 


    
  #Testing_Rate - Total number of people tested per 100,000 persons.
  BaseState<-dplyr::filter(AFBaseLocations, Base == toString(ChosenBase))
  #Find counties in radius
  if (CONUSSelect == "CONUS"){
    StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
    RegPop <- sum(IncludedCounties$Population)
    StPop <- sum(StPopList$Population)
    PopRatio <- RegPop/StPop    
    CovidCountiesCases<-subset(CovidActiveCases2, State %in% IncludedCounties$State)
    
    # PT <- CovidCountiesCases$People_Tested  
    # PT <- sum(PT)
    # TotalPopulation <-  sum(IncludedCounties$Population)
    # TR <- (PT/TotalPopulation)*100000
    
    TR <- CovidCountiesCases$Testing_Rate*PopRatio  
    TR <- sum(TR)
  } else {
    StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$Country[1]))
    RegPop <- sum(IncludedCounties$Population)
    StPop <- sum(StPopList$Population)
    PopRatio <- RegPop/StPop

    CovidCountiesCases<-subset(CovidActiveCases2, Country %in% IncludedCounties$Country)    
        
    PT <- CovidCountiesCases$total_tested
    PT <- sum(PT)    
    TotalPopulation <-  sum(IncludedCounties$Population)
    TR <- (PT/TotalPopulation)*100000
    
    #CovidCountiesCases<-subset(GlobalActive, FIPS %in% IncludedCounties$FIPS) 
    #TR <- "Not Available"
  }

  TR<-round(TR, digits = 2)
  
  Final <- TR
  
}
