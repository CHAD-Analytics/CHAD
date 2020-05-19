#' Get counties within a specified radius 
GetCounties<-function(base, radius, additionalCounties, excludedCounties ){
  
  #Find counties in radius
  baseDF = dplyr::filter(AFBaseLocations, Base == base)
  baseDF = baseDF[1,]
  CountyInfo$DistanceMiles = cimd[,as.character(base)]
  IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius | FIPS == baseDF$FIPS)
  if (is.null(additionalCounties)){}else{IncludedCounties<-rbind(IncludedCounties,additionalCounties)}
  if (is.null(excludedCounties)){}else{IncludedCounties<-subset(IncludedCounties, !(FIPS %in% excludedCounties$FIPS))}
  IncludedCounties<-IncludedCounties %>% distinct(FIPS, .keep_all = TRUE)
  return(IncludedCounties)
  
}