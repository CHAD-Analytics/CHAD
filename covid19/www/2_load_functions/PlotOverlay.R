#' #' Create charts for projecting local health data
#' @details EXTRA EXTRA Need to find a better way later to replace this 
#'          probably have the overlay function return a list with two objects. 
#'          need the data.frame from overlay in the report
PlotOverlay<-function(ChosenBase, IncludedCounties, IncludedHospitals,ModelIDList, DaysProjected, StatisticType,CONUSSelect){
  
  ####Uncomment to test plot function without running the app
  #i<-80
  #ChosenBase = AFBaseLocations$Base[i]
  #CONUSSelect <- "CONUS"
  #ChosenBase = "Vandenberg Space Force Base"
  # CONUSSelect <- "OCONUS"
  # ChosenBase = "Kapaun ADM"
  # SocialDistance = 15
  # DaysProjected = 30
  # HospitalInfo$DistanceMiles = himd[,as.character(ChosenBase)]
  # IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= 50))
  # IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
  # CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
  # value = NULL
  # IncludedCounties<-GetCounties(ChosenBase,50,value,value)
  ####
  ####
  
  #Establish initial inputs such as base, counties, and filter IHME model
  BaseState<-dplyr::filter(AFBaseLocations, Base == toString(ChosenBase))
  if (CONUSSelect == "CONUS"){
      IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
      YYG_State <- dplyr::filter(YYG_ModelU, region == toString(BaseState$State[1])) 
      hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
      TTBCounty <- sum(IncludedHospitals$BEDS)
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))

      # Get total hospital bed number across state
      IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
      TotalBedsState <- sum(IncludedHospitalsST$BEDS)
      # Calculate bed ratio
      BedProp <- TotalBedsCounty/TotalBedsState    
  } else {
    if (BaseState$Country[1] == "South Korea"){
      IHME_State <- dplyr::filter(IHME_Model, location_name == "Republic of Korea")
    } else {
      IHME_State <- dplyr::filter(IHME_Model, location_name == toString(BaseState$"State Name"[1]))   
      if (nrow(IHME_State)==0){
        IHME_State <- dplyr::filter(IHME_Model, location_name == toString(BaseState$County[1]))   
      } 
      if (nrow(IHME_State)==0) {
        IHME_State <- dplyr::filter(IHME_Model, location_name == toString(BaseState$Country[1]))         
      }  
    }
      YYG_State <- dplyr::filter(YYG_ModelG, country == toString(BaseState$Country[1])) 
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$Country[1]))
  }

  #Get regional and state populations
  RegPop <- sum(IncludedCounties$Population)
  StPop <- sum(StPopList$Population)
  
  # Use Population ratio to scale IHME
  PopRatio <- RegPop/StPop

  if (CONUSSelect == "CONUS"){
      Army_State <- dplyr::filter(Army_Model, State == toString(BaseState$State[1]))  
      UT_State <- dplyr::filter(UT_Model, State == toString(BaseState$State[1]))   
      DPT1<-dplyr::filter(DP1,FIPS %in% IncludedCounties$FIPS)
      DPT2<-dplyr::filter(DP2,FIPS %in% IncludedCounties$FIPS)
      DPT1$ForecastDate <- strptime(as.character(DPT1$ForecastDate), "%m/%d/%Y")
      DPT2$ForecastDate <- strptime(as.character(DPT2$ForecastDate), "%m/%d/%Y")  
      DPT1$ForecastDate<-as.Date(DPT1$ForecastDate, "%Y-%m-%d")
      DPT2$ForecastDate<-as.Date(DPT2$ForecastDate, "%Y-%m-%d")
      DPT1<-dplyr::filter(DPT1,(ForecastDate >= (Sys.Date()-5)))        
      DPT2<-dplyr::filter(DPT2,(ForecastDate >= (Sys.Date()-5)))        
      DPT1<-aggregate(DPT1[,sapply(DPT1,is.numeric)],DPT1["ForecastDate"],sum)
      DPT2<-aggregate(DPT2[,sapply(DPT2,is.numeric)],DPT2["ForecastDate"],sum)
      DPT1<-DPT1[1:DaysProjected,]
      DPT2<-DPT2[1:DaysProjected,]
      DPT1$ID<-rep("DTRA1",nrow(DPT1))
      DPT2$ID<-rep("DTRA2",nrow(DPT2))
  } 

  
  if (StatisticType == "Hospitalizations") {

    if (CONUSSelect == "CONUS"){
        LANL_State <- dplyr::filter(LANLC_Data, State == toString(BaseState$State[1])) 
    } else {
        LANL_State <- dplyr::filter(LANLGC_Data, countries == toString(BaseState$Country[1])) 
    }
    #Get covid cases and hospitalization rates for county
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)

    #Get past data in daily hospital use
    #This will use a 5 day hospital stay as the average
    HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] - CovidCounties[,5:(length(CovidCounties)-5)]
    if (nrow(CovidCountiesHospRate) != 0){
        HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)

        #Create dataframe to hold daily hospitalizations
        HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
    } else {
      HistoricalDataHosp<-colSums(HistoricalDataDaily*.055)
      
      #Create dataframe to hold daily hospitalizations
      HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
      HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
      colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")      
    }
    
    currHosp = HistoricalData[nrow(HistoricalData),2]
    
    # Apply ratio's to YYG Data
    # Multiple cases by 5.5% to estimate number of hospitalizations
    YYG_Region <- YYG_State
    YYG_Region$predicted_new_infected_mean = round(YYG_State$predicted_new_infected_mean*PopRatio)
    YYG_Region$predicted_new_infected_lower = round(YYG_State$predicted_new_infected_lower*PopRatio)
    YYG_Region$predicted_new_infected_upper = round(YYG_State$predicted_new_infected_upper*PopRatio)
    YYG_Data<-data.frame(YYG_Region$date,YYG_Region$predicted_new_infected_mean*.055,YYG_Region$predicted_new_infected_lower*.055,YYG_Region$predicted_new_infected_upper*.055)
    colnames(YYG_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
    
    # Apply ratio's to LANL Data
    # Multiple cases by 5.5% to estimate number of hospitalizations
    LANL_Region <- LANL_State
    LANL_Region$q.25 = round(LANL_Region$q.25*PopRatio)
    LANL_Region$q.50 = round(LANL_Region$q.50*PopRatio)
    LANL_Region$q.75 = round(LANL_Region$q.75*PopRatio)
    LANL_Region<-data.frame(LANL_Region$dates,LANL_Region$q.50*.055,LANL_Region$q.25*.055,LANL_Region$q.75*.055)      
    colnames(LANL_Region)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
    LANL_Region$ForecastDate<-as.Date(LANL_Region$ForecastDate)
    
    LANL_Region<-LANL_Region[order(as.Date(LANL_Region$ForecastDate, format="%Y/%m/%d")),]
    LANL_Region$'Expected Hospitalizations'<-c(LANL_Region$'Expected Hospitalizations'[1],diff(LANL_Region$'Expected Hospitalizations'))
    LANL_Region$'Lower Estimate'<-c(LANL_Region$'Lower Estimate'[1],diff(LANL_Region$'Lower Estimate'))
    LANL_Region$'Upper Estimate'<-c(LANL_Region$'Upper Estimate'[1],diff(LANL_Region$'Upper Estimate'))

    YYG_Data$ID<-rep("YYG",nrow(YYG_Data)) 
    LANL_Region$ID<-rep("LANL",nrow(LANL_Region)) 

    if (nrow(IHME_State) !=0 ) {
      IHME_Region <- IHME_State
      IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
      IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
      IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
      colnames(IHME_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      IHME_Data$ID<-rep("IHME",nrow(IHME_Data))        
      OverlayData<-rbind(IHME_Data,LANL_Region)
      OverlayData<-rbind(OverlayData,YYG_Data)    
      
      #Calculate IHME Peak date, create data table of peak dates for hospitalizations 
      IHMEPeak<-round(max(IHME_Data$`Expected Hospitalizations`[1:DaysProjected]))
      IHMEDate<-which.max(IHME_Data$`Expected Hospitalizations`[1:DaysProjected])
      IHMEDate<-format(IHME_Data$ForecastDate[IHMEDate], format="%b-%d")
      YYGPeak<-round(max(YYG_Data$`Expected Hospitalizations`[1:DaysProjected]))
      PeakDate<-which.max(YYG_Data$`Expected Hospitalizations`[1:DaysProjected])
      PeakDate<-format(YYG_Data$ForecastDate[PeakDate], format="%b-%d")    
      PeakDates<-rbind(IHMEDate,PeakDate)
      PeakValues<-rbind(IHMEPeak,YYGPeak)
      LANLPeak<-round(max(LANL_Region$`Expected Hospitalizations`[1:DaysProjected]))
      PeakDate<-which.max(LANL_Region$`Expected Hospitalizations`[1:DaysProjected])
      PeakDate<-format(LANL_Region$ForecastDate[PeakDate], format="%b-%d")        
      PeakDates<-rbind(PeakDates,PeakDate)
      PeakValues<-rbind(PeakValues,LANLPeak)               
      
    } else {
      OverlayData<-rbind(LANL_Region,YYG_Data)
      
      YYGPeak<-round(max(YYG_Data$`Expected Hospitalizations`[1:DaysProjected]))
      PeakDate<-which.max(YYG_Data$`Expected Hospitalizations`[1:DaysProjected])
      PeakDateY<-format(YYG_Data$ForecastDate[PeakDate], format="%b-%d")    
      LANLPeak<-round(max(LANL_Region$`Expected Hospitalizations`[1:DaysProjected]))
      PeakDate<-which.max(LANL_Region$`Expected Hospitalizations`[1:DaysProjected])
      PeakDate<-format(LANL_Region$ForecastDate[PeakDate], format="%b-%d")        
      PeakDates<-rbind(PeakDateY,PeakDate)
      PeakValues<-rbind(YYGPeak,LANLPeak)
    }        
    

    
    
    if (CONUSSelect == "CONUS"){
        # Apply ratio's to UT data
        # Multiple by 16 to reflect hospitalizations at 8% from death rate of 0.5%
        UT_Region <- UT_State
        UT_Region$daily_deaths_est = round(UT_State$daily_deaths_est*PopRatio*16)
        UT_Region$daily_deaths_90CI_lower = round(UT_State$daily_deaths_95CI_lower*PopRatio*16)
        UT_Region$daily_deaths_90CI_upper = round(UT_State$daily_deaths_95CI_upper*PopRatio*16)
        UT_Data<-data.frame(UT_Region$date,UT_Region$daily_deaths_est, UT_Region$daily_deaths_95CI_lower, UT_Region$daily_deaths_95CI_upper)    
        
        #For DTRA Data, multiply number of cases by projected hospitalization rate
        DPT1<-data.frame(DPT1$ForecastDate,DPT1$'Expected Hospitalizations'*.055,DPT1$'Lower Estimate'*.055,DPT1$'Upper Estimate'*.055,DPT1$ID)
        DPT2<-data.frame(DPT2$ForecastDate,DPT2$'Expected Hospitalizations'*.055,DPT2$'Lower Estimate'*.055,DPT2$'Upper Estimate'*.055,DPT2$ID)  
        colnames(DPT1)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate", "ID")
        colnames(DPT2)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate", "ID")    
        
        Army_State<-dplyr::filter(Army_State,FIPS %in% IncludedCounties$FIPS)    
        Army_State<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State,number))    
        Army_State$Date <- as.Date(Army_State$ForecastDate, "%m/%d/%y")
        Army_State<-dplyr::filter(Army_State,ForecastDate >= Sys.Date())
        Army_State<-aggregate(Army_State[,sapply(Army_State,is.numeric)],Army_State["ForecastDate"],sum)
        Army_State<-Army_State[1:DaysProjected,]
        Army_State<-data.frame(Army_State$ForecastDate,Army_State$Infected*.055,Army_State$Infected*.055*.75,Army_State$Infected*.055*1.25)
        colnames(Army_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        Army_State$ID<-rep("CAA",nrow(Army_State))
    
        CU20x10PSD_State<-dplyr::filter(CU20_1x10PSD,fips %in% IncludedCounties$FIPS)
        CU20x5PSD_State<-dplyr::filter(CU20_1x5PSD,fips %in% IncludedCounties$FIPS)
        CU20w10PSD_State<-dplyr::filter(CU20_w10PSD,fips %in% IncludedCounties$FIPS)
        CU20w5PSD_State<-dplyr::filter(CU20_w5PSD,fips %in% IncludedCounties$FIPS) 
        
        CU20x10PSD_State<-subset(CU20x10PSD_State, select=-c(County,State,death_25,death_50,death_75))
        CU20x5PSD_State<-subset(CU20x5PSD_State, select=-c(County,State,death_25,death_50,death_75))    
        CU20w10PSD_State<-subset(CU20w10PSD_State, select=-c(County,State,death_25,death_50,death_75))
        CU20w5PSD_State<-subset(CU20w5PSD_State, select=-c(County,State,death_25,death_50,death_75))    
        
        CU20x10PSD_State$Date <- as.Date(CU20x10PSD_State$Date, "%m/%d/%y")
        CU20x5PSD_State$Date <- as.Date(CU20x5PSD_State$Date, "%m/%d/%y")
        CU20w10PSD_State$Date <- as.Date(CU20w10PSD_State$Date, "%m/%d/%y")
        CU20w5PSD_State$Date <- as.Date(CU20w5PSD_State$Date, "%m/%d/%y") 
        
        CU20x10PSD_State<-dplyr::filter(CU20x10PSD_State,Date >= Sys.Date())
        CU20x5PSD_State<-dplyr::filter(CU20x5PSD_State,Date >= Sys.Date())      
        CU20w10PSD_State<-dplyr::filter(CU20w10PSD_State,Date >= Sys.Date())
        CU20w5PSD_State<-dplyr::filter(CU20w5PSD_State,Date >= Sys.Date())
        
        CU20x10PSD_State<-aggregate(CU20x10PSD_State[,sapply(CU20x10PSD_State,is.numeric)],CU20x10PSD_State["Date"],sum)
        CU20x5PSD_State<-aggregate(CU20x5PSD_State[,sapply(CU20x5PSD_State,is.numeric)],CU20x5PSD_State["Date"],sum)
        CU20w10PSD_State<-aggregate(CU20w10PSD_State[,sapply(CU20w10PSD_State,is.numeric)],CU20w10PSD_State["Date"],sum)
        CU20w5PSD_State<-aggregate(CU20w5PSD_State[,sapply(CU20w5PSD_State,is.numeric)],CU20w5PSD_State["Date"],sum)  
        
        CU20x10PSD_State<-CU20x10PSD_State[1:DaysProjected,]
        CU20x5PSD_State<-CU20x5PSD_State[1:DaysProjected,]
        CU20w10PSD_State<-CU20w10PSD_State[1:DaysProjected,]
        CU20w5PSD_State<-CU20w5PSD_State[1:DaysProjected,]
        
        CU20x10PSD_State <- data.frame(CU20x10PSD_State$Date,CU20x10PSD_State$hosp_need_50,CU20x10PSD_State$hosp_need_25,CU20x10PSD_State$hosp_need_75)
        CU20x5PSD_State <- data.frame(CU20x5PSD_State$Date,CU20x5PSD_State$hosp_need_50,CU20x5PSD_State$hosp_need_25,CU20x5PSD_State$hosp_need_75)
        CU20w10PSD_State <- data.frame(CU20w10PSD_State$Date,CU20w10PSD_State$hosp_need_50,CU20w10PSD_State$hosp_need_25,CU20w10PSD_State$hosp_need_75)
        CU20w5PSD_State <- data.frame(CU20w5PSD_State$Date,CU20w5PSD_State$hosp_need_50,CU20w5PSD_State$hosp_need_25,CU20w5PSD_State$hosp_need_75)      
        
        colnames(CU20x10PSD_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        CU20x10PSD_State$ID<-rep("CU20SCx10",nrow(CU20x10PSD_State))
        colnames(CU20x5PSD_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        CU20x5PSD_State$ID<-rep("CU20SCx5",nrow(CU20x5PSD_State))
        colnames(CU20w10PSD_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        CU20w10PSD_State$ID<-rep("CU20SCw10",nrow(CU20w10PSD_State))
        colnames(CU20w5PSD_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        CU20w5PSD_State$ID<-rep("CU20SCw5",nrow(CU20w5PSD_State))      
    
        colnames(UT_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        UT_Data$ID<-rep("UT",nrow(UT_Data))
        DPT1$ID <- rep("DTRA1",nrow(DPT1)) 
        DPT2$ID <- rep("DTRA2",nrow(DPT2))
        
        OverlayData<-rbind(OverlayData,UT_Data)      
        OverlayData<-rbind(OverlayData,CU20x10PSD_State)
        OverlayData<-rbind(OverlayData,CU20x5PSD_State)
        OverlayData<-rbind(OverlayData,CU20w10PSD_State)
        OverlayData<-rbind(OverlayData,CU20w5PSD_State)
        OverlayData<-rbind(OverlayData,DPT1)
        OverlayData<-rbind(OverlayData,DPT2)
        OverlayData<-rbind(OverlayData,Army_State)            
        
        UTPeak<-round(max(UT_Data$`Expected Hospitalizations`[1:DaysProjected]))
        UTDate<-which.max(UT_Data$`Expected Hospitalizations`[1:DaysProjected])
        UTDate<-format(UT_Data$ForecastDate[UTDate], format="%b-%d")    
        PeakDates<-rbind(PeakDates,UTDate)
        PeakValues<-rbind(PeakValues,UTPeak)    
        CU1Peak<-round(max(CU20x10PSD_State$`Expected Hospitalizations`[1:DaysProjected]))
        PeakDate<-which.max(CU20x10PSD_State$`Expected Hospitalizations`[1:DaysProjected])
        PeakDate<-format(CU20x10PSD_State$ForecastDate[PeakDate], format="%b-%d")       
        PeakDates<-rbind(PeakDates,PeakDate)
        PeakValues<-rbind(PeakValues,CU1Peak)           
        CU2Peak<-round(max(CU20x5PSD_State$`Expected Hospitalizations`[1:DaysProjected]))
        PeakDate<-which.max(CU20x5PSD_State$`Expected Hospitalizations`[1:DaysProjected])
        PeakDate<-format(CU20x5PSD_State$ForecastDate[PeakDate], format="%b-%d")         
        PeakDates<-rbind(PeakDates,PeakDate)
        PeakValues<-rbind(PeakValues,CU2Peak)          
        CU3Peak<-round(max(CU20w10PSD_State$`Expected Hospitalizations`[1:DaysProjected]))
        PeakDate<-which.max(CU20w10PSD_State$`Expected Hospitalizations`[1:DaysProjected])
        PeakDate<-format(CU20w10PSD_State$ForecastDate[PeakDate], format="%b-%d")      
        PeakDates<-rbind(PeakDates,PeakDate)
        PeakValues<-rbind(PeakValues,CU3Peak)
        CU4Peak<-round(max(CU20w5PSD_State$`Expected Hospitalizations`[1:DaysProjected]))
        PeakDate<-which.max(CU20w5PSD_State$`Expected Hospitalizations`[1:DaysProjected])
        PeakDate<-format(CU20w5PSD_State$ForecastDate[PeakDate], format="%b-%d")      
        PeakDates<-rbind(PeakDates,PeakDate)
        PeakValues<-rbind(PeakValues,CU4Peak)         
    }
    
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
    
    
    #Next we use the calculated values, along with estimated values from the Estimated Values.
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    daysforecasted<- 120
    
    SD <- c(27,23,19,15,12,8,4)
    sdrow<-length(SD)

    for (j in 1:sdrow){
      socialdistancing<-SD[j]

      ####################################################################################
      #Mean Estimate
      #Established Variables at the start for every county or populations

      doubling<-as.integer(CaseDblRate(MyCounties))
      if (doubling == 0) {
        doubling <- as.integer(40)
      }
      
      #for the OCONUS locations, use the Rt from YYG files
      Ro<-Estimate_Rt(MyCounties)
      if (Ro == "Undefined for Region"){
        Ro<-as.integer(1)
      } else if (Ro < 1){
        Ro<-as.integer(1)
      }

      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      hospitalizationrate<-5
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-3.5
      icutime<-4
      ventilatortime<-7
      #doubling<-8
      #Ro<-2.5

      #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
      #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
      SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                 socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                                 ventilatortime,daysforecasted,Ro, .5)

      MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
      DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
      TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
      colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations")
      colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")

      CHIMEPeak<-round(max(DailyData$`Expected Hospitalizations`[1:DaysProjected]))
      PeakDate<-which.max(DailyData$`Expected Hospitalizations`[1:DaysProjected])
      PeakDate<-format(DailyData$ForecastDate[PeakDate], format="%b-%d")
      PeakDates<-rbind(PeakDates,PeakDate)
      PeakValues<-rbind(PeakValues,CHIMEPeak)
      ####################################################################################
      #Lower Estimate
      #Established Variables at the start for every county or populations
      doubling<-doubling*.75
      Ro<-Ro*.75
      hospitalizationrate<-hospitalizationrate*.75
      hospitaltime<-hospitalizationrate*.75

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
      doubling<-doubling*1.25
      Ro<-Ro*1.25
      hospitalizationrate<-hospitalizationrate*1.25
      hospitaltime<-hospitalizationrate*1.25
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
    bcap = TotalBeds * (1-baseUtlz)

 
    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
      geom_line(aes(linetype = ID, color = ID)) + 
      geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2) +
      #scale_colour_manual(values=c("tan", "blue", "black","red"))+
      #scale_fill_manual(values = c("tan4", "cadetblue", "gray","red"))+
      #scale_linetype_manual(values=c("dashed", "solid", "dashed", "solid"))+
      #geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2)+
      
      geom_hline(aes(yintercept = bcap,linetype = "Estimated COVID Patient Bed Capacity"),colour = "red")+
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
    # projections <- projections %>% config(displayModeBar = FALSE)
    projections <- projections %>% config(toImageButtonOptions = list(format = "png",
                                                                      width = 1100,
                                                                      height = 500))
    
  } else {
    
    if (CONUSSelect == "CONUS"){
      LANL_State <- dplyr::filter(LANLD_Data, State == toString(BaseState$State[1])) 
    } else {
      LANL_State <- dplyr::filter(LANLGD_Data, countries == toString(BaseState$Country[1])) 
    }    
    

    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    
    
    #############  This looks like repetitive code ################
    
    # CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    # CovidDeathHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    
    # if (nrow(CovidDeathHist) != 0){
    #   HistoricalData<-colSums(CovidDeathHist[,5:length(CovidDeathHist)])
    #   HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    #   HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
    #   colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    # } else {
    #   HistoricalDataHosp<-colSums(HistoricalDataDaily*.0025)
    #   #Create dataframe to hold daily hospitalizations
    #   HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    #   HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
    #   colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    # }    
    
    ###############################################################
         
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

    # Apply ratio's to YYG Data
    # Multiple cases by 5.5% to estimate number of hospitalizations
    YYG_Region <- YYG_State
    YYG_Region$predicted_deaths_mean = round(YYG_State$predicted_deaths_mean*PopRatio)
    YYG_Region$predicted_deaths_lower = round(YYG_State$predicted_deaths_lower*PopRatio)
    YYG_Region$predicted_deaths_upper = round(YYG_State$predicted_deaths_upper*PopRatio)
    YYG_Data<-data.frame(YYG_Region$date,YYG_Region$predicted_deaths_mean,YYG_Region$predicted_deaths_lower,YYG_Region$predicted_deaths_upper)
    colnames(YYG_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    
    YYG_Data<-YYG_Data[order(as.Date(YYG_Data$ForecastDate, format="%Y/%m/%d")),]
    YYG_Data$'Expected Fatalities'<-c(YYG_Data$'Expected Fatalities'[1],diff(YYG_Data$'Expected Fatalities'))
    YYG_Data$'Lower Estimate'<-c(YYG_Data$'Lower Estimate'[1],diff(YYG_Data$'Lower Estimate'))
    YYG_Data$'Upper Estimate'<-c(YYG_Data$'Upper Estimate'[1],diff(YYG_Data$'Upper Estimate'))      
    
    LANL_Region <- LANL_State
    LANL_Region$q.25 = round(LANL_Region$q.25*PopRatio)
    LANL_Region$q.50 = round(LANL_Region$q.50*PopRatio)
    LANL_Region$q.75 = round(LANL_Region$q.75*PopRatio)
    LANL_Data<-data.frame(LANL_Region$dates,LANL_Region$q.50,LANL_Region$q.25,LANL_Region$q.75)      
    colnames(LANL_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    LANL_Data$ForecastDate <- as.Date(LANL_Data$ForecastDate)
    LANL_Data<-dplyr::filter(LANL_Data,ForecastDate >= Sys.Date())
    
    LANL_Data<-LANL_Data[order(as.Date(LANL_Data$ForecastDate, format="%Y/%m/%d")),]
    LANL_Data$'Expected Hospitalizations'<-c(LANL_Data$'Expected Hospitalizations'[1],diff(LANL_Data$'Expected Hospitalizations'))
    LANL_Data$'Lower Estimate'<-c(LANL_Data$'Lower Estimate'[1],diff(LANL_Data$'Lower Estimate'))
    LANL_Data$'Upper Estimate'<-c(LANL_Data$'Upper Estimate'[1],diff(LANL_Data$'Upper Estimate'))  

    colnames(IHME_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
    YYG_Data$ID<-rep("YYG",nrow(YYG_Data)) 
    LANL_Data$ID<-rep("LANL",nrow(LANL_Data))    
    OverlayData<-rbind(IHME_Data,LANL_Data)
    OverlayData<-rbind(OverlayData,YYG_Data)
    
    
    if (CONUSSelect == "CONUS"){     
        
        # Apply ratio's to UT data
        UT_Region <- UT_State
        UT_Region$daily_deaths_est = round(UT_State$daily_deaths_est*PopRatio)
        UT_Region$daily_deaths_90CI_lower = round(UT_State$daily_deaths_95CI_lower*PopRatio)
        UT_Region$daily_deaths_90CI_upper = round(UT_State$daily_deaths_95CI_upper*PopRatio)
        UT_Data<-data.frame(UT_Region$date,UT_Region$daily_deaths_est, UT_Region$daily_deaths_95CI_lower, UT_Region$daily_deaths_95CI_upper)
      
        #For DTRA Data, multiply number of cases by projected hospitalization rate
        DPT1<-data.frame(DPT1$ForecastDate,DPT1$'Expected Hospitalizations'*.0025,DPT1$'Lower Estimate'*.0025,DPT1$'Upper Estimate'*.0025,DPT1$ID)
        DPT2<-data.frame(DPT2$ForecastDate,DPT2$'Expected Hospitalizations'*.0025,DPT2$'Lower Estimate'*.0025,DPT2$'Upper Estimate'*.0025,DPT2$ID)       
        colnames(DPT1)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate","ID")
        colnames(DPT2)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate","ID")   
        
        Army_State<-dplyr::filter(Army_State,FIPS %in% IncludedCounties$FIPS)    
        Army_State<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Infected,State,number))    
        Army_State$Date <- as.Date(Army_State$ForecastDate, "%m/%d/%y")
        Army_State<-dplyr::filter(Army_State,ForecastDate >= Sys.Date())
        Army_State<-aggregate(Army_State[,sapply(Army_State,is.numeric)],Army_State["ForecastDate"],sum)
        Army_State<-Army_State[1:DaysProjected,]
        Army_State<-data.frame(Army_State$ForecastDate,Army_State$Fatalities*.0025,Army_State$Fatalities*.0025*.75,Army_State$Fatalities*.0025*1.25)
        colnames(Army_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
        Army_State$ID<-rep("CAA",nrow(Army_State))    
        
        CU20x10PSD_State<-dplyr::filter(CU20_1x10PSD,fips %in% IncludedCounties$FIPS)
        CU20x5PSD_State<-dplyr::filter(CU20_1x5PSD,fips %in% IncludedCounties$FIPS)
        CU20w10PSD_State<-dplyr::filter(CU20_w10PSD,fips %in% IncludedCounties$FIPS)
        CU20w5PSD_State<-dplyr::filter(CU20_w5PSD,fips %in% IncludedCounties$FIPS) 
        
        CU20x10PSD_State<-subset(CU20x10PSD_State, select=-c(hosp_need_25,hosp_need_50,hosp_need_75))
        CU20x5PSD_State<-subset(CU20x5PSD_State, select=-c(hosp_need_25,hosp_need_50,hosp_need_75))   
        CU20w10PSD_State<-subset(CU20w10PSD_State, select=-c(hosp_need_25,hosp_need_50,hosp_need_75))
        CU20w5PSD_State<-subset(CU20w5PSD_State, select=-c(hosp_need_25,hosp_need_50,hosp_need_75))   
        
        CU20x10PSD_State$Date <- as.Date(CU20x10PSD_State$Date, "%m/%d/%y")
        CU20x5PSD_State$Date <- as.Date(CU20x5PSD_State$Date, "%m/%d/%y")
        CU20w10PSD_State$Date <- as.Date(CU20w10PSD_State$Date, "%m/%d/%y")
        CU20w5PSD_State$Date <- as.Date(CU20w5PSD_State$Date, "%m/%d/%y") 
        
        CU20x10PSD_State<-dplyr::filter(CU20x10PSD_State,Date >= Sys.Date())
        CU20x5PSD_State<-dplyr::filter(CU20x5PSD_State,Date >= Sys.Date())      
        CU20w10PSD_State<-dplyr::filter(CU20w10PSD_State,Date >= Sys.Date())
        CU20w5PSD_State<-dplyr::filter(CU20w5PSD_State,Date >= Sys.Date())
        
        CU20x10PSD_State<-aggregate(CU20x10PSD_State[,sapply(CU20x10PSD_State,is.numeric)],CU20x10PSD_State["Date"],sum)
        CU20x5PSD_State<-aggregate(CU20x5PSD_State[,sapply(CU20x5PSD_State,is.numeric)],CU20x5PSD_State["Date"],sum)
        CU20w10PSD_State<-aggregate(CU20w10PSD_State[,sapply(CU20w10PSD_State,is.numeric)],CU20w10PSD_State["Date"],sum)
        CU20w5PSD_State<-aggregate(CU20w5PSD_State[,sapply(CU20w5PSD_State,is.numeric)],CU20w5PSD_State["Date"],sum)  
        
        CU20x10PSD_State<-CU20x10PSD_State[1:DaysProjected,]
        CU20x5PSD_State<-CU20x5PSD_State[1:DaysProjected,]
        CU20w10PSD_State<-CU20w10PSD_State[1:DaysProjected,]
        CU20w5PSD_State<-CU20w5PSD_State[1:DaysProjected,]
        
        CU20x10PSD_State <- data.frame(CU20x10PSD_State$Date,CU20x10PSD_State$death_50,CU20x10PSD_State$death_25,CU20x10PSD_State$death_75)
        CU20x5PSD_State <- data.frame(CU20x5PSD_State$Date,CU20x5PSD_State$death_50,CU20x5PSD_State$death_25,CU20x5PSD_State$death_75)
        CU20w10PSD_State <- data.frame(CU20w10PSD_State$Date,CU20w10PSD_State$death_50,CU20w10PSD_State$death_25,CU20w10PSD_State$death_75)
        CU20w5PSD_State <- data.frame(CU20w5PSD_State$Date,CU20w5PSD_State$death_50,CU20w5PSD_State$death_25,CU20w5PSD_State$death_75)      
        
        colnames(CU20x10PSD_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
        CU20x10PSD_State$ID<-rep("CU20SCx10",nrow(CU20x10PSD_State))
        colnames(CU20x5PSD_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
        CU20x5PSD_State$ID<-rep("CU20SCx5",nrow(CU20x5PSD_State))
        colnames(CU20w10PSD_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
        CU20w10PSD_State$ID<-rep("CU20SCw10",nrow(CU20w10PSD_State))
        colnames(CU20w5PSD_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
        CU20w5PSD_State$ID<-rep("CU20SCw5",nrow(CU20w5PSD_State))      
        
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
        
        colnames(UT_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        UT_Data$ID<-rep("UT",nrow(UT_Data))
    
        OverlayData<-rbind(OverlayData,UT_Data)    
        OverlayData<-rbind(OverlayData,CU20x10PSD_State)
        OverlayData<-rbind(OverlayData,CU20x5PSD_State)
        OverlayData<-rbind(OverlayData,CU20w10PSD_State)
        OverlayData<-rbind(OverlayData,CU20w5PSD_State)
        OverlayData<-rbind(OverlayData,DPT1)
        OverlayData<-rbind(OverlayData,DPT2)    
        OverlayData<-rbind(OverlayData,Army_State)      
    }
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    daysforecasted<- 120
    
    SD <- c(27,23,19,15,12,8,4)
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
    # projections <- projections %>% config(displayModeBar = FALSE)
    projections <- projections %>% config(toImageButtonOptions = list(format = "png",
                                                                      width = 1100,
                                                                      height = 500))
  }
  
}