#' #' Create charts for projecting local health data
#' @details EXTRA EXTRA Need to find a better way later to replace this 
#'          probably have the overlay function return a list with two objects. 
#'          need the data.frame from overlay in the report
PlotOverlay<-function(ChosenBase, IncludedCounties, IncludedHospitals, SocialDistance,ModelIDList, DaysProjected, StatisticType){
  
  # #####Uncomment to test plot function without running the app
  # i<-80
  # ChosenBase = AFBaseLocations$Base[i]
  # SocialDistance = 15
  # DaysProjected = 30
  # HospitalInfo$DistanceMiles = himd[,as.character(ChosenBase)]
  # IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= 50))
  # IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
  # CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
  # IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= 50)
  # #####
  # #####
  
  #Establish initial inputs such as base, counties, and filter IHME model
  BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
  IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
  hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
  TTBCounty <- sum(IncludedHospitals$BEDS)
  
  #Get regional and state populations
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
  

  if (StatisticType == "Hospitalizations") {
      LANL_State <- dplyr::filter(LANL_Data, State == toString(BaseState$State[1])) 
      #Get covid cases and hospitalization rates for county
      CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
      CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
      
      #Get past data in daily hospital use
      #This will use a 5 day hospital stay as the average
      HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] -
        CovidCounties[,5:(length(CovidCounties)-5)]
      HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)
      
      #Create dataframe to hold daily hospitalizations
      HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
      HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
      colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      
      currHosp = HistoricalData[nrow(HistoricalData),2]
  
      
      # Apply ratio's to IHME data
      IHME_Region <- IHME_State
      IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
      IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
      IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
      
      LANL_Region <- LANL_State
      LANL_Region$q.25 = round(LANL_Region$q.25*PopRatio)
      LANL_Region$q.50 = round(LANL_Region$q.50*PopRatio)
      LANL_Region$q.75 = round(LANL_Region$q.75*PopRatio)
      
      LANL_Region<-data.frame(LANL_Region$date,LANL_Region$q.50*.055,LANL_Region$q.25*.055,LANL_Region$q.75*.055)      
      colnames(LANL_Region)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      LANL_Region$ForecastDate<-as.Date(LANL_Region$ForecastDate)
      LANL_Region<-dplyr::filter(LANL_Region,ForecastDate >= Sys.Date())
      
      CU40_State<-dplyr::filter(CU40PSD,fips %in% IncludedCounties$FIPS)
      CU30_State<-dplyr::filter(CU30PSD,fips %in% IncludedCounties$FIPS)
      CU20_State<-dplyr::filter(CU20PSD,fips %in% IncludedCounties$FIPS)
      CU00_State<-dplyr::filter(CU00PSD,fips %in% IncludedCounties$FIPS)      
      CU40_State<-subset(CU40_State, select=-c(County,State,death_25,death_50,death_75))
      CU30_State<-subset(CU30_State, select=-c(County,State,death_25,death_50,death_75))
      CU20_State<-subset(CU20_State, select=-c(County,State,death_25,death_50,death_75))
      CU00_State<-subset(CU00_State, select=-c(County,State,death_25,death_50,death_75))      
      
      CU40_State$Date <- as.Date(CU40_State$Date, "%m/%d/%y")
      CU30_State$Date <- as.Date(CU30_State$Date, "%m/%d/%y")
      CU20_State$Date <- as.Date(CU20_State$Date, "%m/%d/%y")
      CU00_State$Date <- as.Date(CU00_State$Date, "%m/%d/%y")      
      CU40_State<-dplyr::filter(CU40_State,Date >= Sys.Date())
      CU30_State<-dplyr::filter(CU30_State,Date >= Sys.Date())
      CU20_State<-dplyr::filter(CU20_State,Date >= Sys.Date())
      CU00_State<-dplyr::filter(CU00_State,Date >= Sys.Date())      
      CU40_State<-aggregate(CU40_State[,sapply(CU40_State,is.numeric)],CU40_State["Date"],sum)
      CU30_State<-aggregate(CU30_State[,sapply(CU30_State,is.numeric)],CU30_State["Date"],sum)
      CU20_State<-aggregate(CU20_State[,sapply(CU20_State,is.numeric)],CU20_State["Date"],sum)
      CU00_State<-aggregate(CU00_State[,sapply(CU00_State,is.numeric)],CU00_State["Date"],sum)
      CU40_State<-CU40_State[1:DaysProjected,]      
      CU30_State<-CU30_State[1:DaysProjected,]
      CU20_State<-CU20_State[1:DaysProjected,]
      CU00_State<-CU00_State[1:DaysProjected,]
      
      CU40_State <- data.frame(CU40_State$Date,CU40_State$hosp_need_50,CU40_State$hosp_need_25,CU40_State$hosp_need_75)
      CU30_State <- data.frame(CU30_State$Date,CU30_State$hosp_need_50,CU30_State$hosp_need_25,CU30_State$hosp_need_75)
      CU20_State <- data.frame(CU20_State$Date,CU20_State$hosp_need_50,CU20_State$hosp_need_25,CU20_State$hosp_need_75)
      CU00_State <- data.frame(CU00_State$Date,CU00_State$hosp_need_50,CU00_State$hosp_need_25,CU00_State$hosp_need_75)
      
      colnames(CU40_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
      CU40_State$ID<-rep("CU_40%_SD",nrow(CU40_State))
      colnames(CU30_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
      CU30_State$ID<-rep("CU_30%_SD",nrow(CU30_State))
      colnames(CU20_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
      CU20_State$ID<-rep("CU_20%_SD",nrow(CU20_State))
      colnames(CU00_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
      CU00_State$ID<-rep("CU_No Intervention",nrow(CU00_State))      
      
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
      SIRinputs<-data.frame(currHosp,sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
      colnames(SIRinputs)<-c("cases","pop","doubling")
      
      colnames(IHME_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
      LANL_Region$ID<-rep("LANL",nrow(LANL_Region))      
      OverlayData<-rbind(IHME_Data,LANL_Region)
      OverlayData<-rbind(OverlayData,CU40_State)
      OverlayData<-rbind(OverlayData,CU30_State)
      OverlayData<-rbind(OverlayData,CU20_State)
      OverlayData<-rbind(OverlayData,CU00_State)              
      
      #Next we use the calculated values, along with estimated values from the Estimated Values. 
      #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
      cases<-SIRinputs$cases
      pop<-SIRinputs$pop
      doubling<-8
      
      SD <- c(4,8,12,15,19,23,27)
      sdrow<-length(SD)
      
      for (j in 1:sdrow){
        socialdistancing<-SD[j]      
        
        ####################################################################################
        #Mean Estimate
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        hospitalizationrate<-14
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-120
        
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
        
        
        ####################################################################################
        #Lower Estimate
        #Established Variables at the start for every county or populations
        Ro<-2.3
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        hospitalizationrate<-11
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
        
        ####################################################################################
        #Upper Estimate
        #Established Variables at the start for every county or populations
        Ro<-2.6
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        hospitalizationrate<-17
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-7
        icutime<-4
        ventilatortime<-7        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Estimate","Upper Estimate")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
        
        DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
        DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
        DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
        DailyData<-DailyData[-1,]
        
        chimelabel<-paste("CHIME_",socialdistancing,"%_SD",sep = "")
        DailyData$ID<-rep(chimelabel,nrow(DailyData))
        OverlayData<-rbind(OverlayData,DailyData)
      }
      
      HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
      HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
      OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
      
      OverlayData<- dplyr::filter(OverlayData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
      
      OverlayData<-rbind(HistoricalData, OverlayData)
      
      #########
      #########
      OverlayData<-subset(OverlayData, ID %in% ModelIDList)
      
      
      hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
      #Finds number of hospitals in radius
      TotalBeds<-sum(hospCounty$num_staffed_beds)
      #get historic utilization
      hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
      totalUsedBeds <- sum(hospCounty$bedsUsed)
      baseUtlz <- totalUsedBeds/TotalBeds
      
      projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
        geom_line(aes(linetype = ID, color = ID)) + 
        geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2) +
        #scale_colour_manual(values=c("tan", "blue", "black","red"))+
        #scale_fill_manual(values = c("tan4", "cadetblue", "gray","red"))+
        #scale_linetype_manual(values=c("dashed", "solid", "dashed", "solid"))+
        
        geom_hline(aes(yintercept = TotalBeds * (1-baseUtlz),
                       linetype = "Estimated COVID Patient Bed Capacity"),colour = "red") +
        ggtitle("Projected Daily Hospital Bed Utilization")+
        ylab("Daily Beds Needed")+
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
        scale_x_date(date_breaks = "1 week")+
        labs(color = "ID")
      
      
      projections <- ggplotly(projections)
      projections <- projections %>% config(displayModeBar = FALSE)
      projections
    
  } else {
    
    LANL_State <- dplyr::filter(LANL_Data, State == toString(BaseState$State[1])) 
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidDeathHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidDeathHist[,5:length(CovidDeathHist)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    
    # Apply ratio's to IHME data
    IHME_Region <- IHME_State
    IHME_Region$deaths_mean = round(IHME_State$totdea_mean*PopRatio)
    IHME_Region$deaths_lower = round(IHME_State$totdea_lower*PopRatio)
    IHME_Region$deaths_upper = round(IHME_State$totdea_upper*PopRatio)
    IHME_Data<-data.frame(IHME_Region$date,IHME_Region$deaths_mean, IHME_Region$deaths_lower, IHME_Region$deaths_upper)
    colnames(IHME_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    
    LANL_Region <- LANL_State
    LANL_Region$q.25 = round(LANL_Region$q.25*PopRatio)
    LANL_Region$q.50 = round(LANL_Region$q.50*PopRatio)
    LANL_Region$q.75 = round(LANL_Region$q.75*PopRatio)
    LANL_Data<-data.frame(LANL_Region$dates,LANL_Region$q.50,LANL_Region$q.25,LANL_Region$q.75)      
    colnames(LANL_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    LANL_Data$ForecastDate <- as.Date(LANL_Data$ForecastDate)
    LANL_Data<-dplyr::filter(LANL_Data,ForecastDate >= Sys.Date())
    
    CU40_State<-dplyr::filter(CU40PSD,fips %in% IncludedCounties$FIPS)
    CU30_State<-dplyr::filter(CU30PSD,fips %in% IncludedCounties$FIPS)
    CU20_State<-dplyr::filter(CU20PSD,fips %in% IncludedCounties$FIPS)
    CU00_State<-dplyr::filter(CU00PSD,fips %in% IncludedCounties$FIPS)              
    CU40_State<-subset(CU40_State, select=-c(hosp_need_25,hosp_need_50,hosp_need_75))
    CU30_State<-subset(CU30_State, select=-c(hosp_need_25,hosp_need_50,hosp_need_75))
    CU20_State<-subset(CU20_State, select=-c(hosp_need_25,hosp_need_50,hosp_need_75))
    CU00_State<-subset(CU00_State, select=-c(hosp_need_25,hosp_need_50,hosp_need_75))      
    
    CU40_State$Date <- as.Date(CU40_State$Date, "%m/%d/%y")
    CU30_State$Date <- as.Date(CU30_State$Date, "%m/%d/%y")
    CU20_State$Date <- as.Date(CU20_State$Date, "%m/%d/%y")
    CU00_State$Date <- as.Date(CU00_State$Date, "%m/%d/%y")      
    CU40_State<-dplyr::filter(CU40_State,Date >= Sys.Date())
    CU30_State<-dplyr::filter(CU30_State,Date >= Sys.Date())
    CU20_State<-dplyr::filter(CU20_State,Date >= Sys.Date())
    CU00_State<-dplyr::filter(CU00_State,Date >= Sys.Date())      
    CU40_State<-aggregate(CU40_State[,sapply(CU40_State,is.numeric)],CU40_State["Date"],sum)
    CU30_State<-aggregate(CU30_State[,sapply(CU30_State,is.numeric)],CU30_State["Date"],sum)
    CU20_State<-aggregate(CU20_State[,sapply(CU20_State,is.numeric)],CU20_State["Date"],sum)
    CU00_State<-aggregate(CU00_State[,sapply(CU00_State,is.numeric)],CU00_State["Date"],sum)
    CU40_State<-CU40_State[1:DaysProjected,]      
    CU30_State<-CU30_State[1:DaysProjected,]
    CU20_State<-CU20_State[1:DaysProjected,]
    CU00_State<-CU00_State[1:DaysProjected,]
    
    CU40_State <- data.frame(CU40_State$Date,CU40_State$death_50,CU40_State$death_25,CU40_State$death_75)
    CU30_State <- data.frame(CU30_State$Date,CU30_State$death_50,CU30_State$death_25,CU30_State$death_75)
    CU20_State <- data.frame(CU20_State$Date,CU20_State$death_50,CU20_State$death_25,CU20_State$death_75)
    CU00_State <- data.frame(CU00_State$Date,CU00_State$death_50,CU00_State$death_25,CU00_State$death_75)
    
    colnames(CU40_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
    CU40_State$ID<-rep("CU_40%_SD",nrow(CU40_State))
    colnames(CU30_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
    CU30_State$ID<-rep("CU_30%_SD",nrow(CU30_State))
    colnames(CU20_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
    CU20_State$ID<-rep("CU_20%_SD",nrow(CU20_State))
    colnames(CU00_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
    CU00_State$ID<-rep("CU_No Intervention",nrow(CU00_State))              
    
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
    
    IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
    LANL_Data$ID<-rep("LANL",nrow(LANL_Data))      
    OverlayData<-rbind(IHME_Data,LANL_Data)
    OverlayData<-rbind(OverlayData,CU40_State)
    OverlayData<-rbind(OverlayData,CU30_State)
    OverlayData<-rbind(OverlayData,CU20_State)
    OverlayData<-rbind(OverlayData,CU00_State)              
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    SD <- c(4,8,12,15,19,23,27)
    sdrow<-length(SD)
    for (j in 1:sdrow){
      socialdistancing<-SD[j]
      ####################################################################################
      #Mean Estimate
      #Established Variables at the start for every county or populations
      Ro<-2.5
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      hospitalizationrate<-5
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-3.5
      icutime<-4
      ventilatortime<-7
      daysforecasted<-120          
      #Established Variables at the start for every county or populations
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
      
      ####################################################################################
      #Lower Estimate
      #Established Variables at the start for every county or populations
      Ro<-2.5
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      hospitalizationrate<-5
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-3.5
      icutime<-4
      ventilatortime<-7          
      #Established Variables at the start for every county or populations
      #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
      #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
      SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                                 socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                 icutime,ventilatortime,daysforecasted,Ro, .5)
      
      DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
      TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
      colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
      colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
      
      ####################################################################################
      #Upper Estimate
      #Established Variables at the start for every county or populations
      Ro<-2.5
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      hospitalizationrate<-5.5
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-3.5
      icutime<-4
      ventilatortime<-7          
      #Next we use the calculated values, along with estimated values from the Estimated Values. 
      #Established Variables at the start for every county or populations
      
      #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
      #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
      SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                 socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                 icutime,ventilatortime,daysforecasted,Ro, .5)
      
      DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
      TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
      colnames(DailyData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
      colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
      
      DailyData$`Expected Fatalities` <- round(DailyData$`Expected Fatalities`*(.25/5.5),0)
      DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`*(.15/4),0)
      DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`*(1/8),0)
      DailyData<-DailyData[-1,]
      DailyData$`Expected Fatalities`<-cumsum(DailyData$`Expected Fatalities`)
      DailyData$`Lower Estimate`<-cumsum(DailyData$`Lower Estimate`)
      DailyData$`Upper Estimate`<-cumsum(DailyData$`Upper Estimate`)
      chimelabel<-paste("CHIME_",socialdistancing,"%_SD",sep = "")
      DailyData$ID<-rep(chimelabel,nrow(DailyData))
      OverlayData<-rbind(OverlayData,DailyData)
    }
    
    
    colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
    
    OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
    
    OverlayData<- dplyr::filter(OverlayData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
    
    OverlayData<-rbind(HistoricalData, OverlayData)
    
    #########
    #########
    OverlayData<-subset(OverlayData, ID %in% ModelIDList)
    
    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Fatalities`, color = ID, fill = ID, linetype = ID)) +
      geom_line(aes(linetype = ID, color = ID)) + 
      geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2) +
      #scale_colour_manual(values=c("tan", "blue", "black","red"))+
      #scale_fill_manual(values = c("tan4", "cadetblue", "gray","red"))+
      #scale_linetype_manual(values=c("dashed", "solid", "dashed", "solid"))+
      ggtitle("Projected Fatalities")+
      ylab("Fatalities")+
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
      labs(color = "ID")
    
    
    projections <- ggplotly(projections)
    projections <- projections %>% config(displayModeBar = FALSE)
    projections
  }
  
}
