#' Get counties within a specified radius 
GetCounties<-function(base,radius){
    
    #Find counties in radius
  baseDF = dplyr::filter(AFBaseLocations, Base == base)
  CountyInfo$DistanceMiles = cimd[,as.character(base)]
  IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius | FIPS == baseDF$FIPS)
  return(IncludedCounties)
  
}
