CaseDblRate <- function(IncludedCounties){
  
  #Find counties in radius
  CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  
  #Compute cumlative cases and deaths in selected counties
  CumDailyCovid<-colSums(CovidCountiesCases[,5:length(CovidCountiesCases)])
  
  j = 0
  cases = CumDailyCovid[length(CumDailyCovid)]
  
  if (cases != 0){
    
    while (cases/2 < CumDailyCovid[length(CumDailyCovid)-j])
    {
      j = j + 1
    }
    
    days = j
    
  }else{
    
    days = 0
  }
  
  v <- days
    
}
