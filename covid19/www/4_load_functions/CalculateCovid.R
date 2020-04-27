#' Get total confirmed cases in the selected region
CalculateCovid<-function(IncludedCounties){
  
    #Get total confirmed cases in the selected region
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    sum(rev(CovidCounties)[,1])
}
