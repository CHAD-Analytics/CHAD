CalculateDeaths<-function(IncludedCounties){
  
    #Get total deaths in the selected region
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    sum(rev(CovidCountiesDeath)[,1])
}
