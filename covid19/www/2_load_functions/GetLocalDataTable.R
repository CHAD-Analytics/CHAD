#' Create Data Table for local statistics
GetLocalDataTable<-function(IncludedCounties){
  
  CovidCounties<-subset(CovidConfirmedCases, 
                        CountyFIPS %in% IncludedCounties$FIPS)
  DeathCounties<-subset(CovidDeaths, 
                        CountyFIPS %in% IncludedCounties$FIPS)
  # CaseRate <- subset(CovidConfirmedCasesRate, 
  #                    CountyFIPS %in% IncludedCounties$FIPS)
  
  CaseChange1Day  = scales::percent((rev(CovidCounties)[,1]/rev(CovidCounties)[,2])-1)
  CaseChange3Day  = scales::percent((rev(CovidCounties)[,1]/rev(CovidCounties)[,4])-1)
  #CaseChange5Day  = scales::percent((rev(CovidCounties)[,1]/rev(CovidCounties)[,6])-1)
  CaseChange7Day  = scales::percent((rev(CovidCounties)[,1]/rev(CovidCounties)[,8])-1)
  CaseChange14Day = scales::percent((rev(CovidCounties)[,1]/rev(CovidCounties)[,15])-1)
  
  CountyDataTable<-cbind(IncludedCounties,
                         rev(CovidCounties)[,1],
                         rev(DeathCounties)[,1])
                         #rev(CaseRate)[,1])
  
  CountyDataTable<-data.frame(CountyDataTable$State,
                              CountyDataTable$County,
                              CountyDataTable$Population, 
                              rev(CountyDataTable)[,2], 
                              rev(CountyDataTable)[,1],
                              #rev(CountyDataTable)[,1],
                              CaseChange1Day,
                              CaseChange3Day,
                              #CaseChange5Day,
                              CaseChange7Day,
                              CaseChange14Day,
                              round(CountyDataTable$DistanceMiles,0))
  
  colnames(CountyDataTable)<-c("State/Country",
                               "County/Region",
                               "Population",
                               "Total Confirmed Cases",
                               "Total Fatalities", 
                               #"Case Doubling Rate (days)",
                               "1-Day Case Change",
                               "3-Day Case Change",
                               #"5-Day Case Change",
                               "7-Day Case Change",
                               "14-Day Case Change",
                               "Distance" )
  
  CountyDataTable <- CountyDataTable[order(CountyDataTable$Distance),]
  
  CountyDataTable
}