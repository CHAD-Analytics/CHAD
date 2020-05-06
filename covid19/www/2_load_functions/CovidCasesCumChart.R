#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesCumChart<-function(IncludedCounties){
    
    #Find counties in radius
    CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    
    #Compute cumlative cases and deaths in selected counties
    CumDailyCovid<-colSums(CovidCountiesCases[,5:length(CovidCountiesCases)])
    CumDailyDeaths<-colSums(CovidCountiesDeath[5:length(CovidCountiesDeath)])
    
    
    #Clean up the dataset to get ready to plot it
    #ForecastDate<- seq(as.Date("2020-1-23"), length=length(CumDailyCovid), by="1 day")
    ForecastDate<- seq(as.Date("2020-1-22"), length=length(CumDailyCovid), by="1 day")
    Chart2Data<-cbind.data.frame(ForecastDate,CumDailyCovid,CumDailyDeaths)
    colnames(Chart2Data)<-c("ForecastDate","Total Cases","Total Fatalities")

    Chart2DataSub <- melt(data.table(Chart2Data), id=c("ForecastDate"))
}
