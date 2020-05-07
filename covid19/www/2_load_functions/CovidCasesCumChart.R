#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesCumChart<-function(IncludedCounties){
    
    #Find counties in radius
    CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    
    #Compute cumlative cases and deaths in selected counties
    CumDailyCovid<-colSums(CovidCountiesCases[,5:length(CovidCountiesCases)])
    CumDailyDeaths<-colSums(CovidCountiesDeath[5:length(CovidCountiesDeath)])
    
    #Estimate recovered from cases and deaths
    DailyCases = CumDailyCovid[2:length(CumDailyCovid)] - CumDailyCovid[1:(length(CumDailyCovid)-1)]
    DailyDeaths = CumDailyDeaths[2:length(CumDailyDeaths)] - CumDailyDeaths[1:(length(CumDailyDeaths)-1)]
    DailyRecovered = DailyCases[1:(length(CumDailyCovid)-14)] - DailyDeaths[5:(length(DailyDeaths)-9)]
    CumRecovered = cumsum(DailyRecovered)
    
    
    #Clean up the dataset to get ready to plot it
    ForecastDate<- seq(as.Date("2020-1-22"), length=length(CumDailyCovid), by="1 day")
    ForecastDateR<- seq(as.Date("2020-2-05"), length=length(CumRecovered), by="1 day")
    ChartData<-cbind.data.frame(ForecastDate,CumDailyCovid,CumDailyDeaths)
    colnames(ChartData)<-c("ForecastDate","Total Cases","Total Fatalities")
    ChartData2 = cbind.data.frame(ForecastDateR, "Estimated Recovered", CumRecovered)
    colnames(ChartData2)<-c("ForecastDate", "variable", "value")
    
    ChartDataSub <- melt(data.table(ChartData), id=c("ForecastDate"))
    ChartDataSub = rbind(ChartDataSub, ChartData2)
    
}
