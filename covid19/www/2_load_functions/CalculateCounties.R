#' Get the total population in the selected region
CalculateCounties<-function(IncludedCounties){
  
    #Get the total population in the selected region
    TotalPopulation <-  sum(IncludedCounties$Population)
    TotalPopulation
    
}
