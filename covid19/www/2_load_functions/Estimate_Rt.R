Estimate_Rt <- function(IncludedCounties){
  
  #Find counties in radius
  CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  
  #Compute cumlative cases and deaths in selected counties
  CumDailyCovid<-colSums(CovidCountiesCases[,5:length(CovidCountiesCases)])
  
  #5-day and 14-day averages
  len = length(CumDailyCovid)
  cases5day = CumDailyCovid[len] - CumDailyCovid[len-5]
  cases14day = CumDailyCovid[len] - CumDailyCovid[len-14]
  
  avg5 = cases5day/5
  avg14 = cases14day/14
  
  if (avg14 == 0){
    Rt = "Undefined for Region"
  } else{
    Rt = round(avg5/avg14,2)
  }
  
}
