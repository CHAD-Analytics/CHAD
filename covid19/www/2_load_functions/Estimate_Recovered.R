Estimate_Recovered <- function(CONUSSelect,ChosenBase,IncludedCounties){
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
  
  #Find counties in radius
  if (CONUSSelect == "CONUS"){
    CovidCountiesCases<-subset(CovidActiveCases1, FIPS %in% IncludedCounties$FIPS)
    RC <- CovidCountiesCases$Recovered      
    RC <- sum(RC)
  } else {
    if (IncludedCounties$State == "HI" || IncludedCounties$State == "AK" || IncludedCounties$State == "PR") {
      CovidCountiesCases<-subset(CovidActiveCases1, FIPS %in% IncludedCounties$FIPS)
      RC <- CovidCountiesCases$Active  
      RC <- sum(AC)        
    } else {
      CovidCountiesCases<-subset(GlobalActive, FIPS %in% IncludedCounties$FIPS) 
      RC <- CovidCountiesCases$total_recovered
      RC <- sum(RC)
    }
  }  

  Final <- RC  
  
}
