CalculateCovid1000<-function(IncludedCounties){
  
  #Get total confirmed cases in the selected region
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  ceiling((sum(rev(CovidCounties)[,1]))/(sum(IncludedCounties$Population))*1000)
  
}
