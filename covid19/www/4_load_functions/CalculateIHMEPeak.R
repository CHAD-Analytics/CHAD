CalculateIHMEPeak<-function(ChosenBase, IncludedHospitals, radius, StatisticType){
    
    if (StatisticType == "Hospitalizations") {
      #Creating the stats and dataframes determined by the base we choose to look at.
      BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
      IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
      TotalBedsCounty <- sum(IncludedHospitals$BEDS)
      
      #Get regional and state populations
      CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
      IncludedCounties <- dplyr::filter(CountyInfo, DistanceMiles <= radius)
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
      RegPop <- sum(IncludedCounties$Population)
      StPop <- sum(StPopList$Population)
      
      # Use Population ratio to scale IHME
      PopRatio <- RegPop/StPop
      
      # Get total hospital bed number across state
      IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
      TotalBedsState <- sum(IncludedHospitalsST$BEDS)
      
      # Calculate bed ratio
      BedProp <- TotalBedsCounty/TotalBedsState
      
      # Apply ratio's to IHME data
      IHME_Region <- IHME_State
      IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean)
      
      PeakDate<-which.max(IHME_Data$IHME_Region.allbed_mean)
      Peak<-IHME_Data[PeakDate,2]
      round(Peak)
    } else {
      BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
      IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
      TotalBedsCounty <- sum(IncludedHospitals$BEDS)
      MyCounties<-GetCounties(ChosenBase, radius)
      #Get regional and state populations
      CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
      CovidDeathsHist<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
      HistoricalData<-colSums(CovidDeathsHist[,5:length(CovidDeathsHist)])
      HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
      HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
      colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
      
      
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
      RegPop <- sum(MyCounties$Population)
      StPop <- sum(StPopList$Population)
      
      # Use Population ratio to scale IHME
      PopRatio <- RegPop/StPop
      
      # Get total hospital bed number across state
      IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
      TotalBedsState <- sum(IncludedHospitalsST$BEDS)
      
      # Calculate bed ratio
      BedProp <- TotalBedsCounty/TotalBedsState
      
      # Apply ratio's to IHME data
      IHME_Region <- IHME_State
      IHME_Region$deaths_mean = round(IHME_State$totdea_mean*PopRatio)
      IHME_Region$deaths_lower = round(IHME_State$totdea_lower*PopRatio)
      IHME_Region$deaths_upper = round(IHME_State$totdea_upper*PopRatio)
      IHME_Region<-data.frame(IHME_Region$date, IHME_Region$deaths_mean, IHME_Region$deaths_lower, IHME_Region$deaths_upper)
      colnames(IHME_Region)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
      max(IHME_Region$`Expected Fatalities`)
    }
    
}
