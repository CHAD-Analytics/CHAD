# This function creates the dataframe for plotting daily cases, deaths, estimated hospitalizations in selected region
CovidCasesPerDayChart<-function(IncludedCounties){
      
    #Get cases and deaths in selected region
    CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    
    #Find Daily new cases
    DailyNewCases <- CovidCountiesCases[,6:length(CovidCountiesCases)] -
      CovidCountiesCases[,5:(length(CovidCountiesCases)-1)]
    DailyNewCasesT <- colSums(DailyNewCases)
    
    #Find New Deaths
    DailyNewDeaths <- CovidCountiesDeath[,6:length(CovidCountiesDeath)] -
      CovidCountiesDeath[,5:(length(CovidCountiesDeath)-1)]
    DailyNewDeathsT <- colSums(DailyNewDeaths)
    
    #Clean up the dataset to prepare for plotting
    #ForecastDate<- seq(as.Date("2020-1-23"), length=length(DailyNewCases), by="1 day")
    ForecastDate<- seq(as.Date("2020-1-22"), length=length(DailyNewCases), by="1 day")
    Chart1Data<-cbind.data.frame(ForecastDate,DailyNewCasesT,DailyNewDeathsT)
    colnames(Chart1Data)<-c("ForecastDate","New Cases","New Fatalities")

    Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
}
