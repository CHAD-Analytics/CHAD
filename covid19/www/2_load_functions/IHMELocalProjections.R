IHMELocalProjections<-function(MyCounties, IncludedHospitals, ChosenBase, StatisticType, DaysProjected){
    if (StatisticType == "Hospitalizations") {
      
        #Establish initial inputs such as base, counties, and filter IHME model
        BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        hospCounty <- subset(HospUtlzCounty, fips %in% MyCounties$FIPS)
        
        #Get covid cases and hospitalization rates for county
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% MyCounties$FIPS)
        
        #Get past data in daily hospital use
        #This will use a 5 day hospital stay as the average
        HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] -
          CovidCounties[,5:(length(CovidCounties)-5)]
        HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)
        
        #Create dataframe to hold daily hospitalizations
        HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        
        #Get population information to build scaling ratio
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
        RegPop <- sum(MyCounties$Population)
        StPop <- sum(StPopList$Population)
        
        #Use Population ratio to scale IHME
        PopRatio <- RegPop/StPop
      
        #Finds number of hospitals in radius
        TotalBeds<-sum(hospCounty$num_staffed_beds)
        #get historic utilization
        hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
        totalUsedBeds <- sum(hospCounty$bedsUsed)
        baseUtlz <- totalUsedBeds/TotalBeds
        TT <- sum(IncludedHospitals$BEDS)
        
        # Apply ratio's to IHME data
        IHME_Region <- IHME_State
        IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
        IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
        IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
        IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
        colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
        IHME_Region$ID<-rep("IHME", nrow(IHME_Region))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        
        IHME_Region<-rbind(HistoricalData,IHME_Region)
        IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
        
        r1 <- ggplot(IHME_Region, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
            geom_line() +
            scale_colour_manual(values=c("blue","black"))+
            scale_fill_manual(values = c("cadetblue", "gray"))+
            scale_linetype_manual(values = c("dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
            xlab('Date') +
            ylab('Daily Beds Needed') +
            ggtitle("IHME Projected Daily Hospital Bed Utilization") +
            theme_bw() + 
            theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
                  axis.title = element_text(face = "bold", size = 11, family = "sans"),
                  axis.text.x = element_text(angle = 60, hjust = 1), 
                  axis.line = element_line(color = "black"),
                  legend.position = "top",
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank()) +
            scale_x_date(date_breaks = "2 week")+
            labs(color='')+
            scale_y_continuous(labels = comma)
        
        r1<- ggplotly(r1)
        r1 <- r1 %>% config(displayModeBar = FALSE)
        r1
        
    } else {
        #Creating the stats and dataframes determined by the base we choose to look at.
        BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        TotalBedsCounty <- sum(IncludedHospitals$BEDS)
        
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
        IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
        IHME_Region$ID<-rep("IHME", nrow(IHME_Region))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        
        IHME_Region<-rbind(HistoricalData,IHME_Region)
        IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
        
        r1 <- ggplot(IHME_Region, aes(x=ForecastDate, y=`Expected Fatalities`, color = ID, fill = ID, linetype = ID)) +
            geom_line() +
            scale_colour_manual(values=c("blue","black"))+
            scale_fill_manual(values = c("cadetblue", "gray"))+
            scale_linetype_manual(values = c("dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
            xlab('Date') +
            ylab('Fatalities') +
            ggtitle("IHME Projected Fatalities") +
            theme_bw() + 
            theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
                  axis.title = element_text(face = "bold", size = 11, family = "sans"),
                  axis.text.x = element_text(angle = 60, hjust = 1), 
                  axis.line = element_line(color = "black"),
                  legend.position = "top",
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank()) +
            scale_x_date(date_breaks = "2 week")+
            labs(color='')+
            scale_y_continuous(labels = comma)
        
        r1 <- ggplotly(r1)
        r1 <- r1 %>% config(displayModeBar = FALSE)
        r1
    }
}
