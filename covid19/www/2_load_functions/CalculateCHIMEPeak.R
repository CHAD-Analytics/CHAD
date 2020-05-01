CalculateCHIMEPeak<-function(IncludedCounties, ChosenBase, ChosenRadius, SocialDistance, ProjectedDays, StatisticType){
  if (StatisticType == "Hospitalizations") {

    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases","Maximum Daily Cases")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-ProjectedDays
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData<-DailyData[-1,]
    DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
    
    Peak<-which.max(DailyData$`Expected Daily Cases`)
    Peak<-DailyData[Peak,2]
    round(Peak)
  } else {
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidDeathsHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidDeathsHist[,5:length(CovidDeathsHist)])
    
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-ProjectedDays
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Fatalities")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData$`Expected Fatalities` <- round(DailyData$`Expected Fatalities`*(.25/5.5),0)
    DailyData<-DailyData[-1,]
    DailyData$`Expected Fatalities`<-cumsum(DailyData$`Expected Fatalities`)
    DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
    max(DailyData$`Expected Fatalities`)

  }
    
}
