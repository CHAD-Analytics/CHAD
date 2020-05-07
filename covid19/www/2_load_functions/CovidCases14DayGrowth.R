CovidCases14DayGrowthChart<-function(IncludedCounties){

  #Get cases and deaths in selected region
  CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
  
  #Find Daily new cases
  DailyNewCases <- CovidCountiesCases[,6:length(CovidCountiesCases)] -
    CovidCountiesCases[,5:(length(CovidCountiesCases)-1)]
  DailyNewCasesT <- colSums(DailyNewCases)
  DailyNewCases14Day = round(rollmean(DailyNewCasesT, 14, align = "right"))
  CaseGrowth14Day = round(DailyNewCases14Day[2:length(DailyNewCases14Day)]/
                    DailyNewCases14Day[1:(length(DailyNewCases14Day)-1)],2)
  CaseGrowth14Day = ifelse(is.infinite(CaseGrowth14Day), NA, CaseGrowth14Day)
  
  #Plot 14 day moving average
  ForecastDateB<- seq(as.Date("2020-02-06"), length=length(CaseGrowth14Day), by="1 day")
  Chart1Data<-cbind.data.frame(ForecastDateB,CaseGrowth14Day)
  colnames(Chart1Data)<-c("ForecastDate","New Cases")
  Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
}
