CovidCasesPer3DayAverageChart<-function(IncludedCounties){
  
  #Get cases and deaths in selected region
  CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
  
  #Find Daily new cases
  DailyNewCases <- CovidCountiesCases[,6:length(CovidCountiesCases)] -
    CovidCountiesCases[,5:(length(CovidCountiesCases)-1)]
  DailyNewCasesT <- colSums(DailyNewCases)
  DailyNewCasesThreeDay = round(rollmean(DailyNewCasesT, 3, align = "right"))
  
  #Find New Deaths
  DailyNewDeaths <- CovidCountiesDeath[,6:length(CovidCountiesDeath)] -
    CovidCountiesDeath[,5:(length(CovidCountiesDeath)-1)]
  DailyNewDeathsT <- colSums(DailyNewDeaths)
  DailyNewDeathsThreeDay = round(rollmean(DailyNewDeathsT, 3, align = "right"))
  
  #Plot 3 day moving average
  ForecastDate = as.Date(names(DailyNewCasesThreeDay), "%m/%d/%y")
  # ForecastDateB<- seq(as.Date("2020-1-25"), length=length(DailyNewCasesThreeDay), by="1 day")
  Chart1Data<-cbind.data.frame(ForecastDate,DailyNewCasesThreeDay,DailyNewDeathsThreeDay)
  colnames(Chart1Data)<-c("ForecastDate","New Cases","New Fatalities")
  Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
  
}