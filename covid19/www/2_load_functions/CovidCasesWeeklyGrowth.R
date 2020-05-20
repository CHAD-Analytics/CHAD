CovidCasesWeeklyGrowth<-function(IncludedCounties){

  #Get cases and deaths in selected region
  CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  CaseTot = colSums(CovidCountiesCases[,5:ncol(CovidCountiesCases)])
  CaseTot = CaseTot[CaseTot > 30]
  CaseWeekly = CaseTot[8:length(CaseTot)] - CaseTot[1:(length(CaseTot)-7)]
  GrowthWeekly = round(CaseWeekly[8:length(CaseWeekly)]/CaseWeekly[1:(length(CaseWeekly)-7)],2)-1
  GrowthWeekly = ifelse(is.infinite(GrowthWeekly), 0, GrowthWeekly)
  GrowthWeekly = ifelse(is.na(GrowthWeekly),0,GrowthWeekly)
  
  #Get cases on Monday
  ForecastDate = as.Date(names(GrowthWeekly), "%m/%d/%y")
  ForecastDay = weekdays(ForecastDate)
  CasesDF = data.frame(ForecastDate,ForecastDay,GrowthWeekly)
  CasesDF = dplyr::filter(CasesDF, ForecastDay == "Monday")
  
  #Plot Weekly Growth Rate
  Chart1Data<-cbind.data.frame(CasesDF$ForecastDate,CasesDF$GrowthWeekly)
  colnames(Chart1Data)<-c("Forecast Date","Weekly Growth")
  Chart1DataSub <- melt(data.table(Chart1Data), id=c("Forecast Date"))
}
  