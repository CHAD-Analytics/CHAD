#' #' Create charts for projecting local health data
#' @details EXTRA EXTRA Need to find a better way later to replace this 
#'          probably have the overlay function return a list with two objects. 
#'          need the data.frame from overlay in the report
PlotOverlay<-function(ChosenBase, IncludedCounties, IncludedHospitals,ModelIDList, DaysProjected, StatisticType,CONUSSelect,RedLine){
  
  HRate <- .034
  ICRate <- .00917375
  IHRate <- .26982  
  VCRate <- .02576175
  VHRate <- .7577  
  FCRate <- .004
  FHRate <- .1176  
  
  # ##Uncomment to test plot function without running the app
  # i<-80
  # ChosenBase = AFBaseLocations$Base[i]
  # CONUSSelect <- "CONUS"
  # ChosenBase = "Hanscom AFB"
  # #CONUSSelect <- "OCONUS"
  # #ChosenBase = "Andersen AFB"
  # SocialDistance = 15
  # DaysProjected = 30
  # HospitalInfo$DistanceMiles = himd[,as.character(ChosenBase)]
  # IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= 50))
  # IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
  # CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
  # value = NULL
  # IncludedCounties<-GetCounties(ChosenBase,50,value,value)
  # ###
  # ###
  
  #Establish initial inputs such as base, counties, and filter IHME model
  BaseState<-dplyr::filter(AFBaseLocations, Base == toString(ChosenBase))
  if (CONUSSelect == "CONUS"){
      IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
      YYG_State <- dplyr::filter(YYG_ModelU, region == toString(BaseState$State[1])) 
      hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
      TTBCounty <- sum(IncludedHospitals$BEDS)
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))

      # # Get total hospital bed number across state
      # IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
      # TotalBedsState <- sum(IncludedHospitalsST$BEDS)
      # # Calculate bed ratio
      # BedProp <- TotalBedsCounty/TotalBedsState    
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
      #Army_State <- dplyr::filter(Army_Model, State == toString(BaseState$State[1]))  
      Army_State<-dplyr::filter(Army_Model,FIPS %in% IncludedCounties$FIPS)
      Torch_State<-dplyr::filter(Torch_Model,FIP %in% IncludedCounties$FIPS) 
      
      Torch_HospAvail <- unique(Torch_State$EstHospBedsAvail)
      Torch_HospAvail <- sum(Torch_HospAvail) 
      
      Torch_ICUAvail <- unique(Torch_State$EstICUBedsAvail)
      Torch_ICUAvail <- sum(Torch_ICUAvail)
      
      UT_State <- dplyr::filter(UT_Model, State == toString(BaseState$State[1]))  
      DPT1<-dplyr::filter(DP1,FIPS %in% IncludedCounties$FIPS)
      DPT2<-dplyr::filter(DP2,FIPS %in% IncludedCounties$FIPS)
      DPT3<-dplyr::filter(DP3,FIPS %in% IncludedCounties$FIPS)      
      #DPT1$ForecastDate <- strptime(as.character(DPT1$ForecastDate), "%m/%d/%Y")
      #DPT2$ForecastDate <- strptime(as.character(DPT2$ForecastDate), "%m/%d/%Y")  
      #DPT3$ForecastDate <- strptime(as.character(DPT3$ForecastDate), "%m/%d/%Y")        
      DPT1$ForecastDate<-as.Date(DPT1$ForecastDate, "%Y-%m-%d")
      DPT2$ForecastDate<-as.Date(DPT2$ForecastDate, "%Y-%m-%d")
      DPT3$ForecastDate<-as.Date(DPT3$ForecastDate, "%Y-%m-%d")      
      startdate <- "2020-05-22"
      startdate <-as.Date(startdate, "%Y-%m-%d")
      datediff <- as.numeric(Sys.Date()-startdate)
      DPT1<-dplyr::filter(DPT1,(ForecastDate >= (Sys.Date()-datediff)))        
      DPT2<-dplyr::filter(DPT2,(ForecastDate >= (Sys.Date()-datediff)))        
      DPT3<-dplyr::filter(DPT3,(ForecastDate >= (Sys.Date()-datediff)))              
      DPT1<-aggregate(DPT1[,sapply(DPT1,is.numeric)],DPT1["ForecastDate"],sum)
      DPT2<-aggregate(DPT2[,sapply(DPT2,is.numeric)],DPT2["ForecastDate"],sum)
      DPT3<-aggregate(DPT3[,sapply(DPT3,is.numeric)],DPT3["ForecastDate"],sum)      
      DPT1<-DPT1[1:DaysProjected,]
      DPT2<-DPT2[1:DaysProjected,]
      DPT3<-DPT3[1:DaysProjected,]      
      DPT1$ID<-rep("DTRA1",nrow(DPT1))
      DPT2$ID<-rep("DTRA2",nrow(DPT2))
      DPT3$ID<-rep("DTRA3",nrow(DPT3))      
  } 

    
  if (StatisticType == "Hospitalizations") {

    OverlayData <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate","ID"))
    
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
        #HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
        HistoricalDates = as.Date(names(HistoricalDataHosp), "%m/%d/%y")
        HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
    } else {
      HistoricalDataHosp<-colSums(HistoricalDataDaily*HRate)
      
      #Create dataframe to hold daily hospitalizations
      #HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
      HistoricalDates = as.Date(names(HistoricalDataHosp), "%m/%d/%y")
      HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
      colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")      
    }
    
    currHosp = HistoricalData[nrow(HistoricalData),2]

    if (nrow(IHME_State) !=0 ) {
      IHME_Region <- IHME_State
      IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
      IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
      IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
      colnames(IHME_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      IHME_Data$ID<-rep("IHME",nrow(IHME_Data))    
      OverlayData<-rbind(OverlayData,IHME_Data)
    }            
    
    if (nrow(YYG_State) !=0 ) {        
        # Apply ratio's to YYG Data
        # Multiple cases by 5.5% to estimate number of hospitalizations
        YYG_Region <- YYG_State
        YYG_Region$predicted_new_infected_mean = round(YYG_State$predicted_new_infected_mean*PopRatio)
        YYG_Region$predicted_new_infected_lower = round(YYG_State$predicted_new_infected_lower*PopRatio)
        YYG_Region$predicted_new_infected_upper = round(YYG_State$predicted_new_infected_upper*PopRatio)
        YYG_Data<-data.frame(YYG_Region$date,YYG_Region$predicted_new_infected_mean*HRate,YYG_Region$predicted_new_infected_lower*HRate,YYG_Region$predicted_new_infected_upper*HRate)
        colnames(YYG_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        YYG_Data$ID<-rep("YYG",nrow(YYG_Data)) 
        OverlayData<-rbind(OverlayData,YYG_Data)        
    }
    
    if (nrow(LANL_State) !=0 ) {
        # Apply ratio's to LANL Data
        # Multiple cases by 5.5% to estimate number of hospitalizations
        LANL_Region <- LANL_State
        LANL_Region$q.25 = round(LANL_Region$q.25*PopRatio)
        LANL_Region$q.50 = round(LANL_Region$q.50*PopRatio)
        LANL_Region$q.75 = round(LANL_Region$q.75*PopRatio)
        LANL_Region<-data.frame(LANL_Region$dates,LANL_Region$q.50*HRate,LANL_Region$q.25*HRate,LANL_Region$q.75*HRate)      
        colnames(LANL_Region)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        LANL_Region$ForecastDate<-as.Date(LANL_Region$ForecastDate)
        
        LANL_Region<-LANL_Region[order(as.Date(LANL_Region$ForecastDate, format="%Y/%m/%d")),]
        LANL_Region$'Expected Hospitalizations'<-c(LANL_Region$'Expected Hospitalizations'[1],diff(LANL_Region$'Expected Hospitalizations'))
        LANL_Region$'Lower Estimate'<-c(LANL_Region$'Lower Estimate'[1],diff(LANL_Region$'Lower Estimate'))
        LANL_Region$'Upper Estimate'<-c(LANL_Region$'Upper Estimate'[1],diff(LANL_Region$'Upper Estimate'))
        LANL_Region$ID<-rep("LANL",nrow(LANL_Region)) 
        OverlayData<-rbind(OverlayData,LANL_Region)
    }        
    # #Calculate IHME Peak date, create data table of peak dates for hospitalizations 
    # IHMEPeak<-round(max(IHME_Data$`Expected Hospitalizations`[1:DaysProjected]))
    # IHMEDate<-which.max(IHME_Data$`Expected Hospitalizations`[1:DaysProjected])
    # IHMEDate<-format(IHME_Data$ForecastDate[IHMEDate], format="%b-%d")
    # YYGPeak<-round(max(YYG_Data$`Expected Hospitalizations`[1:DaysProjected]))
    # PeakDate<-which.max(YYG_Data$`Expected Hospitalizations`[1:DaysProjected])
    # PeakDate<-format(YYG_Data$ForecastDate[PeakDate], format="%b-%d")    
    # PeakDates<-rbind(IHMEDate,PeakDate)
    # PeakValues<-rbind(IHMEPeak,YYGPeak)
    # LANLPeak<-round(max(LANL_Region$`Expected Hospitalizations`[1:DaysProjected]))
    # PeakDate<-which.max(LANL_Region$`Expected Hospitalizations`[1:DaysProjected])
    # PeakDate<-format(LANL_Region$ForecastDate[PeakDate], format="%b-%d")        
    # PeakDates<-rbind(PeakDates,PeakDate)
    # PeakValues<-rbind(PeakValues,LANLPeak)                   
    
    
    if (CONUSSelect == "CONUS"){
        # Apply ratio's to UT data
        # Multiple by 16 to reflect hospitalizations at 8% from death rate of 0.5%
        UT_Region <- UT_State
        UT_Region$daily_deaths_est = round(UT_State$daily_deaths_est*PopRatio*16)
        UT_Region$daily_deaths_90CI_lower = round(UT_State$daily_deaths_95CI_lower*PopRatio*16)
        UT_Region$daily_deaths_90CI_upper = round(UT_State$daily_deaths_95CI_upper*PopRatio*16)
        UT_Data<-data.frame(UT_Region$date,UT_Region$daily_deaths_est, UT_Region$daily_deaths_95CI_lower, UT_Region$daily_deaths_95CI_upper)    
        
        #For DTRA Data, multiply number of cases by projected hospitalization rate
        DPT1<-data.frame(DPT1$ForecastDate,DPT1$'Expected Hospitalizations'*HRate,DPT1$'Lower Estimate'*HRate,DPT1$'Upper Estimate'*HRate,DPT1$ID)
        DPT2<-data.frame(DPT2$ForecastDate,DPT2$'Expected Hospitalizations'*HRate,DPT2$'Lower Estimate'*HRate,DPT2$'Upper Estimate'*HRate,DPT2$ID)  
        DPT3<-data.frame(DPT3$ForecastDate,DPT3$'Expected Hospitalizations'*HRate,DPT3$'Lower Estimate'*HRate,DPT3$'Upper Estimate'*HRate,DPT3$ID)          
        colnames(DPT1)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate", "ID")
        colnames(DPT2)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate", "ID")    
        colnames(DPT3)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate", "ID")         

        Torch_State$Date <- as.Date(Torch_State$Date, "%m/%d/%Y")
        Torch_State<-dplyr::filter(Torch_State,Date >= Sys.Date())
        Torch_State<-aggregate(Torch_State[,sapply(Torch_State,is.numeric)],Torch_State["Date"],sum)
        Torch_State<-Torch_State[1:DaysProjected,]
        Torch_State<-data.frame(Torch_State$Date,Torch_State$HCasesEst,Torch_State$HCasesEstLow,Torch_State$HCasesEstUp)
        colnames(Torch_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        Torch_State$ID<-rep("Torch",nrow(Torch_State))
        OverlayData<-rbind(OverlayData,Torch_State)        
                
        Army_State<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State))    
        Army_State$Date <- as.Date(Army_State$ForecastDate, "%m/%d/%y")
        Army_State<-dplyr::filter(Army_State,ForecastDate >= Sys.Date())
        Army_State<-aggregate(Army_State[,sapply(Army_State,is.numeric)],Army_State["ForecastDate"],sum)
        Army_State<-Army_State[1:DaysProjected,]
        Army_State<-data.frame(Army_State$ForecastDate,Army_State$Infected*HRate,Army_State$Infected*HRate*.75,Army_State$Infected*HRate*1.25)
        colnames(Army_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        Army_State$ID<-rep("CAA",nrow(Army_State))
        OverlayData<-rbind(OverlayData,Army_State)
            
        CUM1_State<-dplyr::filter(CUM1,fips %in% IncludedCounties$FIPS)
        CUM2_State<-dplyr::filter(CUM2,fips %in% IncludedCounties$FIPS)
        CUM3_State<-dplyr::filter(CUM3,fips %in% IncludedCounties$FIPS)
        CUM4_State<-dplyr::filter(CUM4,fips %in% IncludedCounties$FIPS) 
        
        CUM1_State<-subset(CUM1_State, select=-c(County,State)) 
        CUM2_State<-subset(CUM2_State, select=-c(County,State))     
        CUM3_State<-subset(CUM3_State, select=-c(County,State)) 
        CUM4_State<-subset(CUM4_State, select=-c(County,State))    
        
        CUM1_State$Date <- as.Date(CUM1_State$Date, "%m/%d/%y")
        CUM2_State$Date <- as.Date(CUM2_State$Date, "%m/%d/%y")
        CUM3_State$Date <- as.Date(CUM3_State$Date, "%m/%d/%y")
        CUM4_State$Date <- as.Date(CUM4_State$Date, "%m/%d/%y") 
        
        CUM1_State<-dplyr::filter(CUM1_State,Date >= Sys.Date())
        CUM2_State<-dplyr::filter(CUM2_State,Date >= Sys.Date())      
        CUM3_State<-dplyr::filter(CUM3_State,Date >= Sys.Date())
        CUM4_State<-dplyr::filter(CUM4_State,Date >= Sys.Date())
        
        CUM1_State<-aggregate(CUM1_State[,sapply(CUM1_State,is.numeric)],CUM1_State["Date"],sum)
        CUM2_State<-aggregate(CUM2_State[,sapply(CUM2_State,is.numeric)],CUM2_State["Date"],sum)
        CUM3_State<-aggregate(CUM3_State[,sapply(CUM3_State,is.numeric)],CUM3_State["Date"],sum)
        CUM4_State<-aggregate(CUM4_State[,sapply(CUM4_State,is.numeric)],CUM4_State["Date"],sum)  
        
        CUM1_State<-CUM1_State[1:DaysProjected,]
        CUM2_State<-CUM2_State[1:DaysProjected,]
        CUM3_State<-CUM3_State[1:DaysProjected,]
        CUM4_State<-CUM4_State[1:DaysProjected,]
        
        CUM1_State <- data.frame(CUM1_State$Date,CUM1_State$hosp_need_50,CUM1_State$hosp_need_25,CUM1_State$hosp_need_75)
        CUM2_State <- data.frame(CUM2_State$Date,CUM2_State$hosp_need_50,CUM2_State$hosp_need_25,CUM2_State$hosp_need_75)
        CUM3_State <- data.frame(CUM3_State$Date,CUM3_State$hosp_need_50,CUM3_State$hosp_need_25,CUM3_State$hosp_need_75)
        CUM4_State <- data.frame(CUM4_State$Date,CUM4_State$hosp_need_50,CUM4_State$hosp_need_25,CUM4_State$hosp_need_75)      
        
        colnames(CUM1_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        CUM1_State$ID<-rep("CUM1",nrow(CUM1_State))
        colnames(CUM2_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        CUM2_State$ID<-rep("CUM2",nrow(CUM2_State))
        colnames(CUM3_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        CUM3_State$ID<-rep("CUM3",nrow(CUM3_State))
        colnames(CUM4_State)<-c("ForecastDate","Expected Hospitalizations","Lower Estimate","Upper Estimate")
        CUM4_State$ID<-rep("CUM4",nrow(CUM4_State))      
    
        colnames(UT_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        UT_Data$ID<-rep("UT",nrow(UT_Data))

        OverlayData<-rbind(OverlayData,UT_Data)      
        OverlayData<-rbind(OverlayData,CUM1_State)
        OverlayData<-rbind(OverlayData,CUM2_State)
        OverlayData<-rbind(OverlayData,CUM3_State)
        OverlayData<-rbind(OverlayData,CUM4_State)
        OverlayData<-rbind(OverlayData,DPT1)
        OverlayData<-rbind(OverlayData,DPT2)
        OverlayData<-rbind(OverlayData,DPT3)        
        
        # UTPeak<-round(max(UT_Data$`Expected Hospitalizations`[1:DaysProjected]))
        # UTDate<-which.max(UT_Data$`Expected Hospitalizations`[1:DaysProjected])
        # UTDate<-format(UT_Data$ForecastDate[UTDate], format="%b-%d")    
        # PeakDates<-rbind(PeakDates,UTDate)
        # PeakValues<-rbind(PeakValues,UTPeak)    
        # CU1Peak<-round(max(CUM1_State$`Expected Hospitalizations`[1:DaysProjected]))
        # PeakDate<-which.max(CUM1_State$`Expected Hospitalizations`[1:DaysProjected])
        # PeakDate<-format(CUM1_State$ForecastDate[PeakDate], format="%b-%d")       
        # PeakDates<-rbind(PeakDates,PeakDate)
        # PeakValues<-rbind(PeakValues,CU1Peak)           
        # CU2Peak<-round(max(CUM2_State$`Expected Hospitalizations`[1:DaysProjected]))
        # PeakDate<-which.max(CUM2_State$`Expected Hospitalizations`[1:DaysProjected])
        # PeakDate<-format(CUM2_State$ForecastDate[PeakDate], format="%b-%d")         
        # PeakDates<-rbind(PeakDates,PeakDate)
        # PeakValues<-rbind(PeakValues,CU2Peak)          
        # CU3Peak<-round(max(CUM3_State$`Expected Hospitalizations`[1:DaysProjected]))
        # PeakDate<-which.max(CUM3_State$`Expected Hospitalizations`[1:DaysProjected])
        # PeakDate<-format(CUM3_State$ForecastDate[PeakDate], format="%b-%d")      
        # PeakDates<-rbind(PeakDates,PeakDate)
        # PeakValues<-rbind(PeakValues,CU3Peak)
        # CU4Peak<-round(max(CUM4_State$`Expected Hospitalizations`[1:DaysProjected]))
        # PeakDate<-which.max(CUM4_State$`Expected Hospitalizations`[1:DaysProjected])
        # PeakDate<-format(CUM4_State$ForecastDate[PeakDate], format="%b-%d")      
        # PeakDates<-rbind(PeakDates,PeakDate)
        # PeakValues<-rbind(PeakValues,CU4Peak)         
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
    
    # SD <- c(27,23,19,15,12,8,4)
    # sdrow<-length(SD)
    # 
    # for (j in 1:sdrow){
    #   socialdistancing<-SD[j]
    # 
    #   ####################################################################################
    #   #Mean Estimate
    #   #Established Variables at the start for every county or populations
    # 
    #   doubling<-as.integer(CaseDblRate(MyCounties))
    #    if (doubling == 0) {
    #      doubling <- as.integer(40)
    #    }
    # 
    #   #for the OCONUS locations, use the Rt from YYG files
    #   Ro<-Estimate_Rt(MyCounties)
    #   if (Ro == "Undefined for Region"){
    #     Ro<-as.integer(1)
    #   } else if (Ro < 1){
    #     Ro<-as.integer(1)
    #   }
    # 
    #   incubationtime<-5
    #   latenttime<-2
    #   recoverydays<-14
    #   hospitalizationrate<-3.4
    #   icurate<-.92
    #   ventilatorrate<-2.58
    #   hospitaltime<-3.5
    #   icutime<-4
    #   ventilatortime<-7
    #   #doubling<-8
    #   #Ro<-2.5
    # 
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
    #                              ventilatortime,daysforecasted,Ro, .5)
    # 
    #   MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    #   DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    #   TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    # 
    #   # CHIMEPeak<-round(max(DailyData$`Expected Hospitalizations`[1:DaysProjected]))
    #   # PeakDate<-which.max(DailyData$`Expected Hospitalizations`[1:DaysProjected])
    #   # PeakDate<-format(DailyData$ForecastDate[PeakDate], format="%b-%d")
    #   # PeakDates<-rbind(PeakDates,PeakDate)
    #   # PeakValues<-rbind(PeakValues,CHIMEPeak)
    #   ####################################################################################
    #   #Lower Estimate
    #   #Established Variables at the start for every county or populations
    #   doubling<-doubling*.75
    #   Ro<-Ro*.75
    #   hospitalizationrate<-hospitalizationrate*.75
    #   hospitaltime<-hospitalizationrate*.75
    # 
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
    #                              icutime,ventilatortime,daysforecasted,Ro, .5)
    # 
    #   DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    #   TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
    # 
    #   ####################################################################################
    #   #Upper Estimate
    #   #Established Variables at the start for every county or populations
    #   doubling<-doubling*1.25
    #   Ro<-Ro*1.25
    #   hospitalizationrate<-hospitalizationrate*1.25
    #   hospitaltime<-hospitalizationrate*1.25
    #   #Next we use the calculated values, along with estimated values from the Estimated Values.
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
    #                              icutime,ventilatortime,daysforecasted,Ro, .5)
    # 
    #   DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    #   TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Estimate","Upper Estimate")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
    # 
    #   DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
    #   DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
    #   DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
    #   DailyData<-DailyData[-1,]
    # 
    #   chimelabel<-paste("CHIME_",socialdistancing,"%_SD",sep = "")
    #   DailyData$ID<-rep(chimelabel,nrow(DailyData))
    #   OverlayData<-rbind(OverlayData,DailyData)
    # }
    

    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30 & ForecastDate <= Sys.Date())

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
  
      #geom_hline(aes(yintercept = bcap,linetype = "Estimated COVID Patient Bed Capacity"),colour = "red")+
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
    
    if (RedLine == "ShowLine"){
      projections = projections + geom_hline(aes(yintercept = bcap,linetype = "Estimated COVID Patient Bed Capacity - AHA"),colour = "red")
      projections = projections + geom_hline(aes(yintercept = Torch_HospAvail,linetype = "Est COVID Patient Bed Capacity - Torch Insight"),colour = "blue")
    }
    
    # projections <- projections +
    #   ggplot(HistoricalData)+
    #   geom_line(aes(linetype = ID, color = ID)) +
    #   geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2)+
    #   scale_colour_manual(values=c("black"))+
    #   scale_fill_manual(values = c("gray"))+
    #   scale_linetype_manual(values=c("solid"))
    
    
    projections <- ggplotly(projections)
    # projections <- projections %>% config(displayModeBar = FALSE)
    projections <- projections %>% config(toImageButtonOptions = list(format = "png",width = 1100,height = 500))
    
  } else if (StatisticType == "ICUPatients"){
    
    OverlayData <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate","ID"))
    
    if (CONUSSelect == "CONUS"){
      LANL_State <- dplyr::filter(LANLC_Data, State == toString(BaseState$State[1]))
    } else {
      LANL_State <- dplyr::filter(LANLGC_Data, countries == toString(BaseState$Country[1]))
    }
    #Get covid cases and hospitalization rates for county
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
    
    # #Get past data in daily hospital use
    # #This will use a 5 day hospital stay as the average
    # HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] - CovidCounties[,5:(length(CovidCounties)-5)]
    # if (nrow(CovidCountiesHospRate) != 0){
    #   HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)
    #   
    #   #Create dataframe to hold daily hospitalizations
    #   #HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
    #   HistoricalDates = as.Date(names(HistoricalDataHosp), "%m/%d/%y")
    #   HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
    #   colnames(HistoricalData)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate")
    # } else {
    #   HistoricalDataHosp<-colSums(HistoricalDataDaily*HRate)
    #   
    #   #Create dataframe to hold daily hospitalizations
    #   #HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
    #   HistoricalDates = as.Date(names(HistoricalDataHosp), "%m/%d/%y")
    #   HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
    #   colnames(HistoricalData)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate")      
    # }
  
    
    #IHME has their own ICU data
    if (nrow(IHME_State) !=0 ) {
      IHME_Region <- IHME_State
      IHME_Region$newICU_mean = round(IHME_State$newICU_mean*PopRatio)
      IHME_Region$newICU_lower = round(IHME_State$newICU_lower*PopRatio)
      IHME_Region$newICU_upper = round(IHME_State$newICU_upper*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$newICU_mean, IHME_Region$newICU_lower, IHME_Region$newICU_upper)
      colnames(IHME_Data)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate")

      HistoricalData <- dplyr::filter(IHME_Data, ForecastDate >= as.Date("2020-01-27") + 30)      
      HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))

      IHME_Data$ID<-rep("IHME",nrow(IHME_Data))    
      OverlayData<-rbind(OverlayData,IHME_Data)
    }            

    
    currHosp = HistoricalData[nrow(HistoricalData),2]
        
    #YYG only has case data
    if (nrow(YYG_State) !=0 ) {        
      # Apply ratio's to YYG Data
      # Multiple cases by 5.5% to estimate number of ICU Patients
      YYG_Region <- YYG_State
      YYG_Region$predicted_new_infected_mean = round(YYG_State$predicted_new_infected_mean*PopRatio)
      YYG_Region$predicted_new_infected_lower = round(YYG_State$predicted_new_infected_lower*PopRatio)
      YYG_Region$predicted_new_infected_upper = round(YYG_State$predicted_new_infected_upper*PopRatio)
      YYG_Data<-data.frame(YYG_Region$date,YYG_Region$predicted_new_infected_mean*ICRate,YYG_Region$predicted_new_infected_lower*ICRate,YYG_Region$predicted_new_infected_upper*ICRate)
      colnames(YYG_Data)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate")
      YYG_Data$ID<-rep("YYG",nrow(YYG_Data)) 
      OverlayData<-rbind(OverlayData,YYG_Data)        
    }
    
    if (nrow(LANL_State) !=0 ) {
      # Apply ratio's to LANL Data
      # Multiple cases by 5.5% to estimate number of ICU Patients
      LANL_Region <- LANL_State
      LANL_Region$q.25 = round(LANL_Region$q.25*PopRatio)
      LANL_Region$q.50 = round(LANL_Region$q.50*PopRatio)
      LANL_Region$q.75 = round(LANL_Region$q.75*PopRatio)
      LANL_Region<-data.frame(LANL_Region$dates,LANL_Region$q.50*ICRate,LANL_Region$q.25*ICRate,LANL_Region$q.75*ICRate)      
      colnames(LANL_Region)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate")
      LANL_Region$ForecastDate<-as.Date(LANL_Region$ForecastDate)
      
      LANL_Region<-LANL_Region[order(as.Date(LANL_Region$ForecastDate, format="%Y/%m/%d")),]
      LANL_Region$'Expected Hospitalizations'<-c(LANL_Region$'Expected Hospitalizations'[1],diff(LANL_Region$'Expected Hospitalizations'))
      LANL_Region$'Lower Estimate'<-c(LANL_Region$'Lower Estimate'[1],diff(LANL_Region$'Lower Estimate'))
      LANL_Region$'Upper Estimate'<-c(LANL_Region$'Upper Estimate'[1],diff(LANL_Region$'Upper Estimate'))
      LANL_Region$ID<-rep("LANL",nrow(LANL_Region)) 
      OverlayData<-rbind(OverlayData,LANL_Region)
    }        
         
    if (CONUSSelect == "CONUS"){
      # Apply ratio's to UT data
      # Multiple by 16 to reflect hospitalizations at 8% from death rate of 0.5%
      UT_Region <- UT_State
      UT_Region$daily_deaths_est = round(UT_State$daily_deaths_est*PopRatio*(1/FCRate)*ICRate)
      UT_Region$daily_deaths_90CI_lower = round(UT_State$daily_deaths_95CI_lower*(1/FCRate)*ICRate)
      UT_Region$daily_deaths_90CI_upper = round(UT_State$daily_deaths_95CI_upper*PopRatio*(1/FCRate)*ICRate)
      UT_Data<-data.frame(UT_Region$date,UT_Region$daily_deaths_est, UT_Region$daily_deaths_95CI_lower, UT_Region$daily_deaths_95CI_upper)    
      
      #For DTRA Data, multiply number of cases by projected hospitalization rate
      DPT1<-data.frame(DPT1$ForecastDate,DPT1$'Expected Hospitalizations'*IHRate,DPT1$'Lower Estimate'*IHRate,DPT1$'Upper Estimate'*IHRate,DPT1$ID)
      DPT2<-data.frame(DPT2$ForecastDate,DPT2$'Expected Hospitalizations'*IHRate,DPT2$'Lower Estimate'*IHRate,DPT2$'Upper Estimate'*IHRate,DPT2$ID)  
      DPT3<-data.frame(DPT3$ForecastDate,DPT3$'Expected Hospitalizations'*IHRate,DPT3$'Lower Estimate'*IHRate,DPT3$'Upper Estimate'*IHRate,DPT3$ID)          
      colnames(DPT1)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate", "ID")
      colnames(DPT2)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate", "ID")    
      colnames(DPT3)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate", "ID")         
      
      Torch_State$Date <- as.Date(Torch_State$Date, "%m/%d/%Y")
      Torch_State<-dplyr::filter(Torch_State,Date >= Sys.Date())
      Torch_State<-aggregate(Torch_State[,sapply(Torch_State,is.numeric)],Torch_State["Date"],sum)
      Torch_State<-Torch_State[1:DaysProjected,]
      Torch_State<-data.frame(Torch_State$Date,Torch_State$ICUCasesEst,Torch_State$ICUCasesEstLow,Torch_State$ICUCasesEstUp)
      colnames(Torch_State)<-c("ForecastDate","Expected ICU Patients","Lower Estimate","Upper Estimate")
      Torch_State$ID<-rep("Torch",nrow(Torch_State))
      OverlayData<-rbind(OverlayData,Torch_State)        
      
      Army_State<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State))    
      Army_State$Date <- as.Date(Army_State$ForecastDate, "%m/%d/%y")
      Army_State<-dplyr::filter(Army_State,ForecastDate >= Sys.Date())
      Army_State<-aggregate(Army_State[,sapply(Army_State,is.numeric)],Army_State["ForecastDate"],sum)
      Army_State<-Army_State[1:DaysProjected,]
      Army_State<-data.frame(Army_State$ForecastDate,Army_State$Infected*ICRate,Army_State$Infected*ICRate*.75,Army_State$Infected*ICRate*1.25)
      colnames(Army_State)<-c("ForecastDate","Expected ICU Patients","Lower Estimate","Upper Estimate")
      Army_State$ID<-rep("CAA",nrow(Army_State))
      OverlayData<-rbind(OverlayData,Army_State)
      
      CUM1_State<-dplyr::filter(CUM1,fips %in% IncludedCounties$FIPS)
      CUM2_State<-dplyr::filter(CUM2,fips %in% IncludedCounties$FIPS)
      CUM3_State<-dplyr::filter(CUM3,fips %in% IncludedCounties$FIPS)
      CUM4_State<-dplyr::filter(CUM4,fips %in% IncludedCounties$FIPS) 
      
      CUM1_State<-subset(CUM1_State, select=-c(County,State)) #,death_25,death_50,death_75))
      CUM2_State<-subset(CUM2_State, select=-c(County,State)) #,death_25,death_50,death_75))    
      CUM3_State<-subset(CUM3_State, select=-c(County,State)) #,death_25,death_50,death_75))
      CUM4_State<-subset(CUM4_State, select=-c(County,State)) #,death_25,death_50,death_75))    
      
      CUM1_State$Date <- as.Date(CUM1_State$Date, "%m/%d/%y")
      CUM2_State$Date <- as.Date(CUM2_State$Date, "%m/%d/%y")
      CUM3_State$Date <- as.Date(CUM3_State$Date, "%m/%d/%y")
      CUM4_State$Date <- as.Date(CUM4_State$Date, "%m/%d/%y") 
      
      CUM1_State<-dplyr::filter(CUM1_State,Date >= Sys.Date())
      CUM2_State<-dplyr::filter(CUM2_State,Date >= Sys.Date())      
      CUM3_State<-dplyr::filter(CUM3_State,Date >= Sys.Date())
      CUM4_State<-dplyr::filter(CUM4_State,Date >= Sys.Date())
      
      CUM1_State<-aggregate(CUM1_State[,sapply(CUM1_State,is.numeric)],CUM1_State["Date"],sum)
      CUM2_State<-aggregate(CUM2_State[,sapply(CUM2_State,is.numeric)],CUM2_State["Date"],sum)
      CUM3_State<-aggregate(CUM3_State[,sapply(CUM3_State,is.numeric)],CUM3_State["Date"],sum)
      CUM4_State<-aggregate(CUM4_State[,sapply(CUM4_State,is.numeric)],CUM4_State["Date"],sum)  
      
      CUM1_State<-CUM1_State[1:DaysProjected,]
      CUM2_State<-CUM2_State[1:DaysProjected,]
      CUM3_State<-CUM3_State[1:DaysProjected,]
      CUM4_State<-CUM4_State[1:DaysProjected,]
      
      CUM1_State <- data.frame(CUM1_State$Date,CUM1_State$ICU_need_50,CUM1_State$ICU_need_25,CUM1_State$ICU_need_75)
      CUM2_State <- data.frame(CUM2_State$Date,CUM2_State$ICU_need_50,CUM2_State$ICU_need_25,CUM2_State$ICU_need_75)
      CUM3_State <- data.frame(CUM3_State$Date,CUM3_State$ICU_need_50,CUM3_State$ICU_need_25,CUM3_State$ICU_need_75)
      CUM4_State <- data.frame(CUM4_State$Date,CUM4_State$ICU_need_50,CUM4_State$ICU_need_25,CUM4_State$ICU_need_75)      
      
      colnames(CUM1_State)<-c("ForecastDate","Expected ICU Patients","Lower Estimate","Upper Estimate")
      CUM1_State$ID<-rep("CUM1",nrow(CUM1_State))
      colnames(CUM2_State)<-c("ForecastDate","Expected ICU Patients","Lower Estimate","Upper Estimate")
      CUM2_State$ID<-rep("CUM2",nrow(CUM2_State))
      colnames(CUM3_State)<-c("ForecastDate","Expected ICU Patients","Lower Estimate","Upper Estimate")
      CUM3_State$ID<-rep("CUM3",nrow(CUM3_State))
      colnames(CUM4_State)<-c("ForecastDate","Expected ICU Patients","Lower Estimate","Upper Estimate")
      CUM4_State$ID<-rep("CUM4",nrow(CUM4_State))      
      
      colnames(UT_Data)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate")
      UT_Data$ID<-rep("UT",nrow(UT_Data))
      
      OverlayData<-rbind(OverlayData,UT_Data)      
      OverlayData<-rbind(OverlayData,CUM1_State)
      OverlayData<-rbind(OverlayData,CUM2_State)
      OverlayData<-rbind(OverlayData,CUM3_State)
      OverlayData<-rbind(OverlayData,CUM4_State)
      OverlayData<-rbind(OverlayData,DPT1)
      OverlayData<-rbind(OverlayData,DPT2)
      OverlayData<-rbind(OverlayData,DPT3)        
    
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
    
    # SD <- c(27,23,19,15,12,8,4)
    # sdrow<-length(SD)
    # 
    # for (j in 1:sdrow){
    #   socialdistancing<-SD[j]
    #   
    #   ####################################################################################
    #   #Mean Estimate
    #   #Established Variables at the start for every county or populations
    #   
    #   doubling<-as.integer(CaseDblRate(MyCounties))
    #   if (doubling == 0) {
    #     doubling <- as.integer(40)
    #   }
    #   
    #   #for the OCONUS locations, use the Rt from YYG files
    #   Ro<-Estimate_Rt(MyCounties)
    #   if (Ro == "Undefined for Region"){
    #     Ro<-as.integer(1)
    #   } else if (Ro < 1){
    #     Ro<-as.integer(1)
    #   }
    #   
    #   incubationtime<-5
    #   latenttime<-2
    #   recoverydays<-14
    #   hospitalizationrate<-3.4
    #   icurate<-.92
    #   ventilatorrate<-2.58
    #   hospitaltime<-3.5
    #   icutime<-4
    #   ventilatortime<-7
    #   #doubling<-8
    #   #Ro<-2.5
    #   
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
    #                              ventilatortime,daysforecasted,Ro, .5)
    #   
    #   MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    #   DailyData<-data.frame(MyDates, SEIARProj$sir$icu_add)
    #   TotalData<-data.frame(MyDates, SEIARProj$sir$icu_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    #   
    #   # CHIMEPeak<-round(max(DailyData$`Expected Hospitalizations`[1:DaysProjected]))
    #   # PeakDate<-which.max(DailyData$`Expected Hospitalizations`[1:DaysProjected])
    #   # PeakDate<-format(DailyData$ForecastDate[PeakDate], format="%b-%d")
    #   # PeakDates<-rbind(PeakDates,PeakDate)
    #   # PeakValues<-rbind(PeakValues,CHIMEPeak)
    #   ####################################################################################
    #   #Lower Estimate
    #   #Established Variables at the start for every county or populations
    #   doubling<-doubling*.75
    #   Ro<-Ro*.75
    #   hospitalizationrate<-hospitalizationrate*.75
    #   hospitaltime<-hospitalizationrate*.75
    #   
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
    #                              icutime,ventilatortime,daysforecasted,Ro, .5)
    #   
    #   DailyData<-data.frame(DailyData, SEIARProj$sir$icu_add)
    #   TotalData<-data.frame(TotalData, SEIARProj$sir$icu_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
    #   
    #   ####################################################################################
    #   #Upper Estimate
    #   #Established Variables at the start for every county or populations
    #   doubling<-doubling*1.25
    #   Ro<-Ro*1.25
    #   hospitalizationrate<-hospitalizationrate*1.25
    #   hospitaltime<-hospitalizationrate*1.25
    #   #Next we use the calculated values, along with estimated values from the Estimated Values.
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
    #                              icutime,ventilatortime,daysforecasted,Ro, .5)
    #   
    #   DailyData<-data.frame(DailyData, SEIARProj$sir$icu_add)
    #   TotalData<-data.frame(TotalData, SEIARProj$sir$icu_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected ICU Patients","Lower Estimate","Upper Estimate")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
    #   
    #   DailyData$`Expected ICU Patients` <- round(DailyData$`Expected ICU Patients`,0)
    #   DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
    #   DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
    #   DailyData<-DailyData[-1,]
    #   
    #   chimelabel<-paste("CHIME_",socialdistancing,"%_SD",sep = "")
    #   DailyData$ID<-rep(chimelabel,nrow(DailyData))
    #   OverlayData<-rbind(OverlayData,DailyData)
    # }
    
  
    OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
    
    HistoricalData<-dplyr::filter(HistoricalData, ForecastDate <= Sys.Date())
    
    OverlayData<- dplyr::filter(OverlayData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
    
    OverlayData<-rbind(HistoricalData, OverlayData)
    
    #########
    #########
    OverlayData<-subset(OverlayData, ID %in% ModelIDList)
    
    
    hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
    #Finds number of hospitals in radius
    #TotalBeds<-sum(hospCounty$num_staffed_beds)
    #get historic utilization
    hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
    #totalUsedBeds <- sum(hospCounty$bedsUsed)
    #baseUtlz <- totalUsedBeds/TotalBeds
    #bcap = TotalBeds * (1-baseUtlz)

    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected ICU Patients`, color = ID, fill = ID, linetype = ID)) +
      geom_line(aes(linetype = ID, color = ID)) + 
      geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2) +

      ggtitle("Projected Daily ICU Patient Bed Utilization")+
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
    
    if (RedLine == "ShowLine"){
      projections = projections + geom_hline(aes(yintercept = Torch_ICUAvail,linetype = "Estimated COVID ICU Patient Bed Capacity"),colour = "blue")
    }

    projections <- ggplotly(projections)
    # projections <- projections %>% config(displayModeBar = FALSE)
    projections <- projections %>% config(toImageButtonOptions = list(format = "png",width = 1100,height = 500))
    
  } else if (StatisticType == "VentPatients"){
    
    OverlayData <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate","ID"))
    
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
    # HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] - CovidCounties[,5:(length(CovidCounties)-5)]
    # if (nrow(CovidCountiesHospRate) != 0){
    #   HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)
    #   
    #   #Create dataframe to hold daily hospitalizations
    #   #HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
    #   HistoricalDates = as.Date(names(HistoricalDataHosp), "%m/%d/%y")
    #   HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
    #   colnames(HistoricalData)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate")
    # } else {
    #   HistoricalDataHosp<-colSums(HistoricalDataDaily*HRate)
    #   
    #   #Create dataframe to hold daily hospitalizations
    #   #HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
    #   HistoricalDates = as.Date(names(HistoricalDataHosp), "%m/%d/%y")
    #   HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
    #   colnames(HistoricalData)<-c("ForecastDate", "Expected ICU Patients", "Lower Estimate","Upper Estimate")      
    # }
    
    #IHME has their own ICU data
    if (nrow(IHME_State) !=0 ) {
      IHME_Region <- IHME_State
      IHME_Region$newICU_mean = round(IHME_State$InvVen_mean*PopRatio)
      IHME_Region$newICU_lower = round(IHME_State$InvVen_lower*PopRatio)
      IHME_Region$newICU_upper = round(IHME_State$InvVen_upper*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$InvVen_mean, IHME_Region$InvVen_lower, IHME_Region$InvVen_upper)
      colnames(IHME_Data)<-c("ForecastDate", "Expected Ventilator Patients", "Lower Estimate","Upper Estimate")
      
      HistoricalData <- dplyr::filter(IHME_Data, ForecastDate >= as.Date("2020-01-27") + 30)      
      HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
      
      IHME_Data$ID<-rep("IHME",nrow(IHME_Data))    
      OverlayData<-rbind(OverlayData,IHME_Data)
    }            
    
    currHosp = HistoricalData[nrow(HistoricalData),2]
        
    #YYG only has case data
    if (nrow(YYG_State) !=0 ) {        
      # Apply ratio's to YYG Data
      # Multiple cases by 5.5% to estimate number of ICU Patients
      YYG_Region <- YYG_State
      YYG_Region$predicted_new_infected_mean = round(YYG_State$predicted_new_infected_mean*PopRatio)
      YYG_Region$predicted_new_infected_lower = round(YYG_State$predicted_new_infected_lower*PopRatio)
      YYG_Region$predicted_new_infected_upper = round(YYG_State$predicted_new_infected_upper*PopRatio)
      YYG_Data<-data.frame(YYG_Region$date,YYG_Region$predicted_new_infected_mean*VCRate,YYG_Region$predicted_new_infected_lower*VCRate,YYG_Region$predicted_new_infected_upper*VCRate)
      colnames(YYG_Data)<-c("ForecastDate", "Expected Ventilator Patients", "Lower Estimate","Upper Estimate")
      YYG_Data$ID<-rep("YYG",nrow(YYG_Data)) 
      OverlayData<-rbind(OverlayData,YYG_Data)        
    }
    
    if (nrow(LANL_State) !=0 ) {
      # Apply ratio's to LANL Data
      # Multiple cases by 5.5% to estimate number of ICU Patients
      LANL_Region <- LANL_State
      LANL_Region$q.25 = round(LANL_Region$q.25*PopRatio)
      LANL_Region$q.50 = round(LANL_Region$q.50*PopRatio)
      LANL_Region$q.75 = round(LANL_Region$q.75*PopRatio)
      LANL_Region<-data.frame(LANL_Region$dates,LANL_Region$q.50*VCRate,LANL_Region$q.25*VCRate,LANL_Region$q.75*VCRate)      
      colnames(LANL_Region)<-c("ForecastDate", "Expected Ventilator Patients", "Lower Estimate","Upper Estimate")
      LANL_Region$ForecastDate<-as.Date(LANL_Region$ForecastDate)
      
      LANL_Region<-LANL_Region[order(as.Date(LANL_Region$ForecastDate, format="%Y/%m/%d")),]
      LANL_Region$'Expected Hospitalizations'<-c(LANL_Region$'Expected Hospitalizations'[1],diff(LANL_Region$'Expected Hospitalizations'))
      LANL_Region$'Lower Estimate'<-c(LANL_Region$'Lower Estimate'[1],diff(LANL_Region$'Lower Estimate'))
      LANL_Region$'Upper Estimate'<-c(LANL_Region$'Upper Estimate'[1],diff(LANL_Region$'Upper Estimate'))
      LANL_Region$ID<-rep("LANL",nrow(LANL_Region)) 
      OverlayData<-rbind(OverlayData,LANL_Region)
    }        
    
    if (CONUSSelect == "CONUS"){
      # Apply ratio's to UT data
      # Multiple by 16 to reflect hospitalizations at 8% from death rate of 0.5%
      UT_Region <- UT_State
      UT_Region$daily_deaths_est = round(UT_State$daily_deaths_est*PopRatio*(1/FCRate)*VCRate)
      UT_Region$daily_deaths_90CI_lower = round(UT_State$daily_deaths_95CI_lower*(1/FCRate)*VCRate)
      UT_Region$daily_deaths_90CI_upper = round(UT_State$daily_deaths_95CI_upper*PopRatio*(1/FCRate)*VCRate)
      UT_Data<-data.frame(UT_Region$date,UT_Region$daily_deaths_est, UT_Region$daily_deaths_95CI_lower, UT_Region$daily_deaths_95CI_upper)    
      
      #For DTRA Data, multiply number of cases by projected hospitalization rate
      DPT1<-data.frame(DPT1$ForecastDate,DPT1$'Expected Hospitalizations'*VHRate,DPT1$'Lower Estimate'*VHRate,DPT1$'Upper Estimate'*VHRate,DPT1$ID)
      DPT2<-data.frame(DPT2$ForecastDate,DPT2$'Expected Hospitalizations'*VHRate,DPT2$'Lower Estimate'*VHRate,DPT2$'Upper Estimate'*VHRate,DPT2$ID)  
      DPT3<-data.frame(DPT3$ForecastDate,DPT3$'Expected Hospitalizations'*VHRate,DPT3$'Lower Estimate'*VHRate,DPT3$'Upper Estimate'*VHRate,DPT3$ID)          
      colnames(DPT1)<-c("ForecastDate", "Expected Ventilator Patients", "Lower Estimate","Upper Estimate", "ID")
      colnames(DPT2)<-c("ForecastDate", "Expected Ventilator Patients", "Lower Estimate","Upper Estimate", "ID")    
      colnames(DPT3)<-c("ForecastDate", "Expected Ventilator Patients", "Lower Estimate","Upper Estimate", "ID")         
      
      Torch_State$Date <- as.Date(Torch_State$Date, "%m/%d/%Y")
      Torch_State<-dplyr::filter(Torch_State,Date >= Sys.Date())
      Torch_State<-aggregate(Torch_State[,sapply(Torch_State,is.numeric)],Torch_State["Date"],sum)
      Torch_State<-Torch_State[1:DaysProjected,]
      Torch_State<-data.frame(Torch_State$Date,Torch_State$HCasesEst*VHRate,Torch_State$HCasesEstLow*VHRate,Torch_State$HCasesEstUp*VHRate)
      colnames(Torch_State)<-c("ForecastDate","Expected Ventilator Patients","Lower Estimate","Upper Estimate")
      Torch_State$ID<-rep("Torch",nrow(Torch_State))
      OverlayData<-rbind(OverlayData,Torch_State)        
      
      Army_State<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State))    
      Army_State$Date <- as.Date(Army_State$ForecastDate, "%m/%d/%y")
      Army_State<-dplyr::filter(Army_State,ForecastDate >= Sys.Date())
      Army_State<-aggregate(Army_State[,sapply(Army_State,is.numeric)],Army_State["ForecastDate"],sum)
      Army_State<-Army_State[1:DaysProjected,]
      Army_State<-data.frame(Army_State$ForecastDate,Army_State$Infected*VCRate,Army_State$Infected*VCRate*.75,Army_State$Infected*VCRate*1.25)
      colnames(Army_State)<-c("ForecastDate","Expected Ventilator Patients","Lower Estimate","Upper Estimate")
      Army_State$ID<-rep("CAA",nrow(Army_State))
      OverlayData<-rbind(OverlayData,Army_State)
      
      CUM1_State<-dplyr::filter(CUM1,fips %in% IncludedCounties$FIPS)
      CUM2_State<-dplyr::filter(CUM2,fips %in% IncludedCounties$FIPS)
      CUM3_State<-dplyr::filter(CUM3,fips %in% IncludedCounties$FIPS)
      CUM4_State<-dplyr::filter(CUM4,fips %in% IncludedCounties$FIPS) 
      
      CUM1_State<-subset(CUM1_State, select=-c(County,State)) #,death_25,death_50,death_75))
      CUM2_State<-subset(CUM2_State, select=-c(County,State)) #,death_25,death_50,death_75))    
      CUM3_State<-subset(CUM3_State, select=-c(County,State)) #,death_25,death_50,death_75))
      CUM4_State<-subset(CUM4_State, select=-c(County,State)) #,death_25,death_50,death_75))    
      
      CUM1_State$Date <- as.Date(CUM1_State$Date, "%m/%d/%y")
      CUM2_State$Date <- as.Date(CUM2_State$Date, "%m/%d/%y")
      CUM3_State$Date <- as.Date(CUM3_State$Date, "%m/%d/%y")
      CUM4_State$Date <- as.Date(CUM4_State$Date, "%m/%d/%y") 
      
      CUM1_State<-dplyr::filter(CUM1_State,Date >= Sys.Date())
      CUM2_State<-dplyr::filter(CUM2_State,Date >= Sys.Date())      
      CUM3_State<-dplyr::filter(CUM3_State,Date >= Sys.Date())
      CUM4_State<-dplyr::filter(CUM4_State,Date >= Sys.Date())
      
      CUM1_State<-aggregate(CUM1_State[,sapply(CUM1_State,is.numeric)],CUM1_State["Date"],sum)
      CUM2_State<-aggregate(CUM2_State[,sapply(CUM2_State,is.numeric)],CUM2_State["Date"],sum)
      CUM3_State<-aggregate(CUM3_State[,sapply(CUM3_State,is.numeric)],CUM3_State["Date"],sum)
      CUM4_State<-aggregate(CUM4_State[,sapply(CUM4_State,is.numeric)],CUM4_State["Date"],sum)  
      
      CUM1_State<-CUM1_State[1:DaysProjected,]
      CUM2_State<-CUM2_State[1:DaysProjected,]
      CUM3_State<-CUM3_State[1:DaysProjected,]
      CUM4_State<-CUM4_State[1:DaysProjected,]
      
      CUM1_State <- data.frame(CUM1_State$Date,CUM1_State$vent_need_50,CUM1_State$vent_need_25,CUM1_State$vent_need_75)
      CUM2_State <- data.frame(CUM2_State$Date,CUM2_State$vent_need_50,CUM2_State$vent_need_25,CUM2_State$vent_need_75)
      CUM3_State <- data.frame(CUM3_State$Date,CUM3_State$vent_need_50,CUM3_State$vent_need_25,CUM3_State$vent_need_75)
      CUM4_State <- data.frame(CUM4_State$Date,CUM4_State$vent_need_50,CUM4_State$vent_need_25,CUM4_State$vent_need_75)      
      
      colnames(CUM1_State)<-c("ForecastDate","Expected Ventilator Patients","Lower Estimate","Upper Estimate")
      CUM1_State$ID<-rep("CUM1",nrow(CUM1_State))
      colnames(CUM2_State)<-c("ForecastDate","Expected Ventilator Patients","Lower Estimate","Upper Estimate")
      CUM2_State$ID<-rep("CUM2",nrow(CUM2_State))
      colnames(CUM3_State)<-c("ForecastDate","Expected Ventilator Patients","Lower Estimate","Upper Estimate")
      CUM3_State$ID<-rep("CUM3",nrow(CUM3_State))
      colnames(CUM4_State)<-c("ForecastDate","Expected Ventilator Patients","Lower Estimate","Upper Estimate")
      CUM4_State$ID<-rep("CUM4",nrow(CUM4_State))      
      
      colnames(UT_Data)<-c("ForecastDate", "Expected Ventilator Patients", "Lower Estimate","Upper Estimate")
      UT_Data$ID<-rep("UT",nrow(UT_Data))
      
      OverlayData<-rbind(OverlayData,UT_Data)      
      OverlayData<-rbind(OverlayData,CUM1_State)
      OverlayData<-rbind(OverlayData,CUM2_State)
      OverlayData<-rbind(OverlayData,CUM3_State)
      OverlayData<-rbind(OverlayData,CUM4_State)
      OverlayData<-rbind(OverlayData,DPT1)
      OverlayData<-rbind(OverlayData,DPT2)
      OverlayData<-rbind(OverlayData,DPT3)        
      
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
    
    # SD <- c(27,23,19,15,12,8,4)
    # sdrow<-length(SD)
    # 
    # for (j in 1:sdrow){
    #   socialdistancing<-SD[j]
    #   
    #   ####################################################################################
    #   #Mean Estimate
    #   #Established Variables at the start for every county or populations
    #   
    #   doubling<-as.integer(CaseDblRate(MyCounties))
    #   if (doubling == 0) {
    #     doubling <- as.integer(40)
    #   }
    #   
    #   #for the OCONUS locations, use the Rt from YYG files
    #   Ro<-Estimate_Rt(MyCounties)
    #   if (Ro == "Undefined for Region"){
    #     Ro<-as.integer(1)
    #   } else if (Ro < 1){
    #     Ro<-as.integer(1)
    #   }
    #   
    #   incubationtime<-5
    #   latenttime<-2
    #   recoverydays<-14
    #   hospitalizationrate<-3.4
    #   icurate<-.92
    #   ventilatorrate<-2.58
    #   hospitaltime<-3.5
    #   icutime<-4
    #   ventilatortime<-7
    #   #doubling<-8
    #   #Ro<-2.5
    #   
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
    #                              ventilatortime,daysforecasted,Ro, .5)
    #   
    #   MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    #   DailyData<-data.frame(MyDates, SEIARProj$sir$icu_add)
    #   TotalData<-data.frame(MyDates, SEIARProj$sir$icu_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    #   
    #   # CHIMEPeak<-round(max(DailyData$`Expected Hospitalizations`[1:DaysProjected]))
    #   # PeakDate<-which.max(DailyData$`Expected Hospitalizations`[1:DaysProjected])
    #   # PeakDate<-format(DailyData$ForecastDate[PeakDate], format="%b-%d")
    #   # PeakDates<-rbind(PeakDates,PeakDate)
    #   # PeakValues<-rbind(PeakValues,CHIMEPeak)
    #   ####################################################################################
    #   #Lower Estimate
    #   #Established Variables at the start for every county or populations
    #   doubling<-doubling*.75
    #   Ro<-Ro*.75
    #   hospitalizationrate<-hospitalizationrate*.75
    #   hospitaltime<-hospitalizationrate*.75
    #   
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
    #                              icutime,ventilatortime,daysforecasted,Ro, .5)
    #   
    #   DailyData<-data.frame(DailyData, SEIARProj$sir$icu_add)
    #   TotalData<-data.frame(TotalData, SEIARProj$sir$icu_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
    #   
    #   ####################################################################################
    #   #Upper Estimate
    #   #Established Variables at the start for every county or populations
    #   doubling<-doubling*1.25
    #   Ro<-Ro*1.25
    #   hospitalizationrate<-hospitalizationrate*1.25
    #   hospitaltime<-hospitalizationrate*1.25
    #   #Next we use the calculated values, along with estimated values from the Estimated Values.
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
    #                              icutime,ventilatortime,daysforecasted,Ro, .5)
    #   
    #   DailyData<-data.frame(DailyData, SEIARProj$sir$icu_add)
    #   TotalData<-data.frame(TotalData, SEIARProj$sir$icu_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected ICU Patients","Lower Estimate","Upper Estimate")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
    #   
    #   DailyData$`Expected ICU Patients` <- round(DailyData$`Expected ICU Patients`,0)
    #   DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
    #   DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
    #   DailyData<-DailyData[-1,]
    #   
    #   chimelabel<-paste("CHIME_",socialdistancing,"%_SD",sep = "")
    #   DailyData$ID<-rep(chimelabel,nrow(DailyData))
    #   OverlayData<-rbind(OverlayData,DailyData)
    # }
    
    
    #HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    #HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
    OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
    HistoricalData<- dplyr::filter(HistoricalData, ForecastDate <= Sys.Date())    
    OverlayData<- dplyr::filter(OverlayData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
    
    OverlayData<-rbind(HistoricalData, OverlayData)
    
    #########
    #########
    OverlayData<-subset(OverlayData, ID %in% ModelIDList)
    
    
    hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
    #Finds number of hospitals in radius
    #TotalBeds<-sum(hospCounty$num_staffed_beds)
    #get historic utilization
    hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
    #totalUsedBeds <- sum(hospCounty$bedsUsed)
    #baseUtlz <- totalUsedBeds/TotalBeds
    #bcap = TotalBeds * (1-baseUtlz)
    
    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Ventilator Patients`, color = ID, fill = ID, linetype = ID)) +
      geom_line(aes(linetype = ID, color = ID)) + 
      geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2) +
      
      ggtitle("Projected Daily Ventilator Patient Utilization")+
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
    projections <- projections %>% config(toImageButtonOptions = list(format = "png",width = 1100,height = 500))
    
  } else {
    
    OverlayData <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate","ID"))    
    
    if (CONUSSelect == "CONUS"){
      LANL_State <- dplyr::filter(LANLD_Data, State == toString(BaseState$State[1])) 
    } else {
      LANL_State <- dplyr::filter(LANLGD_Data, countries == toString(BaseState$Country[1])) 
    }    
    
    
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidDeathHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    
    if (nrow(CovidDeathHist) != 0){
      HistoricalData<-colSums(CovidDeathHist[,5:length(CovidDeathHist)])
    } else {
      HistoricalDataHosp<-colSums(HistoricalDataDaily)
    }
    
    HistoricalDates = as.Date(names(HistoricalData), "%m/%d/%y")
    #HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    
    if (nrow(IHME_State) !=0 ) {
      # Apply ratio's to IHME data
      IHME_Region <- IHME_State
      IHME_Region$deaths_mean = round(IHME_State$totdea_mean*PopRatio)
      IHME_Region$deaths_lower = round(IHME_State$totdea_lower*PopRatio)
      IHME_Region$deaths_upper = round(IHME_State$totdea_upper*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$deaths_mean, IHME_Region$deaths_lower, IHME_Region$deaths_upper)
      colnames(IHME_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
      IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
      OverlayData<-rbind(OverlayData,IHME_Data)
    }
    
    if (nrow(YYG_State) !=0 ) {    
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
      YYG_Data$ID<-rep("YYG",nrow(YYG_Data))
      OverlayData<-rbind(OverlayData,YYG_Data)        
    }
    
    if (nrow(LANL_State) !=0 ) {
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
      LANL_Data$ID<-rep("LANL",nrow(LANL_Data))
      OverlayData<-rbind(OverlayData,LANL_Data)
    }
    
    
    if (CONUSSelect == "CONUS"){     
      # Apply ratio's to UT data
      UT_Region <- UT_State
      UT_Region$daily_deaths_est = round(UT_State$daily_deaths_est*PopRatio)
      UT_Region$daily_deaths_90CI_lower = round(UT_State$daily_deaths_95CI_lower*PopRatio)
      UT_Region$daily_deaths_90CI_upper = round(UT_State$daily_deaths_95CI_upper*PopRatio)
      UT_Data<-data.frame(UT_Region$date,UT_Region$daily_deaths_est, UT_Region$daily_deaths_95CI_lower, UT_Region$daily_deaths_95CI_upper)
      
      #For DTRA Data, multiply number of cases by projected hospitalization rate
      DPT1<-data.frame(DPT1$ForecastDate,DPT1$'Expected Hospitalizations'*FCRate,DPT1$'Lower Estimate'*FCRate,DPT1$'Upper Estimate'*FCRate,DPT1$ID)
      DPT2<-data.frame(DPT2$ForecastDate,DPT2$'Expected Hospitalizations'*FCRate,DPT2$'Lower Estimate'*FCRate,DPT2$'Upper Estimate'*FCRate,DPT2$ID)
      DPT3<-data.frame(DPT3$ForecastDate,DPT3$'Expected Hospitalizations'*FCRate,DPT3$'Lower Estimate'*FCRate,DPT3$'Upper Estimate'*FCRate,DPT3$ID)        
      colnames(DPT1)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate","ID")
      colnames(DPT2)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate","ID")   
      colnames(DPT3)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate","ID")  
      
      Torch_State$Date <- as.Date(Torch_State$Date, "%m/%d/%Y")
      Torch_State<-dplyr::filter(Torch_State,Date >= Sys.Date())
      Torch_State<-aggregate(Torch_State[,sapply(Torch_State,is.numeric)],Torch_State["Date"],sum)
      Torch_State<-Torch_State[1:DaysProjected,]
      Torch_State<-data.frame(Torch_State$Date,Torch_State$HCasesEst*FHRate,Torch_State$HCasesEstLow*FHRate,Torch_State$HCasesEstUp*FHRate)
      colnames(Torch_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
      Torch_State$ID<-rep("Torch",nrow(Torch_State))
      OverlayData<-rbind(OverlayData,Torch_State)       
      
      Army_State<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Infected,State))    
      Army_State$Date <- as.Date(Army_State$ForecastDate, "%m/%d/%y")
      Army_State<-dplyr::filter(Army_State,ForecastDate >= Sys.Date())
      Army_State<-aggregate(Army_State[,sapply(Army_State,is.numeric)],Army_State["ForecastDate"],sum)
      Army_State<-Army_State[1:DaysProjected,]
      Army_State<-data.frame(Army_State$ForecastDate,Army_State$Fatalities,Army_State$Fatalities*.75,Army_State$Fatalities*1.25)
      colnames(Army_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
      Army_State$ID<-rep("CAA",nrow(Army_State)) 
      OverlayData<-rbind(OverlayData,Army_State)
      
      CUM1_State<-dplyr::filter(CUM1,fips %in% IncludedCounties$FIPS)
      CUM2_State<-dplyr::filter(CUM2,fips %in% IncludedCounties$FIPS)
      CUM3_State<-dplyr::filter(CUM3,fips %in% IncludedCounties$FIPS)
      CUM4_State<-dplyr::filter(CUM4,fips %in% IncludedCounties$FIPS) 
      
      CUM1_State<-subset(CUM1_State, select=-c(County,State)) #,death_25,death_50,death_75))
      CUM2_State<-subset(CUM2_State, select=-c(County,State)) #,death_25,death_50,death_75))    
      CUM3_State<-subset(CUM3_State, select=-c(County,State)) #,death_25,death_50,death_75))
      CUM4_State<-subset(CUM4_State, select=-c(County,State)) #,death_25,death_50,death_75))     
      
      CUM1_State$Date <- as.Date(CUM1_State$Date, "%m/%d/%y")
      CUM2_State$Date <- as.Date(CUM2_State$Date, "%m/%d/%y")
      CUM3_State$Date <- as.Date(CUM3_State$Date, "%m/%d/%y")
      CUM4_State$Date <- as.Date(CUM4_State$Date, "%m/%d/%y") 
      
      CUM1_State<-dplyr::filter(CUM1_State,Date >= Sys.Date())
      CUM2_State<-dplyr::filter(CUM2_State,Date >= Sys.Date())      
      CUM3_State<-dplyr::filter(CUM3_State,Date >= Sys.Date())
      CUM4_State<-dplyr::filter(CUM4_State,Date >= Sys.Date())
      
      CUM1_State<-aggregate(CUM1_State[,sapply(CUM1_State,is.numeric)],CUM1_State["Date"],sum)
      CUM2_State<-aggregate(CUM2_State[,sapply(CUM2_State,is.numeric)],CUM2_State["Date"],sum)
      CUM3_State<-aggregate(CUM3_State[,sapply(CUM3_State,is.numeric)],CUM3_State["Date"],sum)
      CUM4_State<-aggregate(CUM4_State[,sapply(CUM4_State,is.numeric)],CUM4_State["Date"],sum)  
      
      CUM1_State<-CUM1_State[1:DaysProjected,]
      CUM2_State<-CUM2_State[1:DaysProjected,]
      CUM3_State<-CUM3_State[1:DaysProjected,]
      CUM4_State<-CUM4_State[1:DaysProjected,]
      
      CUM1_State <- data.frame(CUM1_State$Date,CUM1_State$death_50,CUM1_State$death_25,CUM1_State$death_75)
      CUM2_State <- data.frame(CUM2_State$Date,CUM2_State$death_50,CUM2_State$death_25,CUM2_State$death_75)
      CUM3_State <- data.frame(CUM3_State$Date,CUM3_State$death_50,CUM3_State$death_25,CUM3_State$death_75)
      CUM4_State <- data.frame(CUM4_State$Date,CUM4_State$death_50,CUM4_State$death_25,CUM4_State$death_75)      
      
      colnames(CUM1_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
      CUM1_State$ID<-rep("CUM1",nrow(CUM1_State))
      colnames(CUM2_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
      CUM2_State$ID<-rep("CUM2",nrow(CUM2_State))
      colnames(CUM3_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
      CUM3_State$ID<-rep("CUM3",nrow(CUM3_State))
      colnames(CUM4_State)<-c("ForecastDate","Expected Fatalities","Lower Estimate","Upper Estimate")
      CUM4_State$ID<-rep("CUM4",nrow(CUM4_State))      
      
      colnames(UT_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
      UT_Data$ID<-rep("UT",nrow(UT_Data))
      
      OverlayData<-rbind(OverlayData,UT_Data)    
      OverlayData<-rbind(OverlayData,CUM1_State)
      OverlayData<-rbind(OverlayData,CUM2_State)
      OverlayData<-rbind(OverlayData,CUM3_State)
      OverlayData<-rbind(OverlayData,CUM4_State)
      OverlayData<-rbind(OverlayData,DPT1)
      OverlayData<-rbind(OverlayData,DPT2)  
      OverlayData<-rbind(OverlayData,DPT3)          
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
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    daysforecasted<- 120
    
    # SD <- c(27,23,19,15,12,8,4)
    # sdrow<-length(SD)
    # for (j in 1:sdrow){
    #   socialdistancing<-SD[j]
    #   ####################################################################################
    #   #Mean Estimate
    #   #Established Variables at the start for every county or populations
    # 
    #   doubling<-as.integer(CaseDblRate(MyCounties))
    #   if (doubling == 0) {
    #     doubling <- as.integer(40)
    #   }
    #   
    #   #for the OCONUS locations, use the Rt from YYG files
    #   Ro<-Estimate_Rt(MyCounties)
    #   if (Ro == "Undefined for Region"){
    #     Ro<-as.integer(1)
    #   } else if (Ro < 1){
    #     Ro<-as.integer(1)
    #   }
    #   
    #   incubationtime<-5
    #   latenttime<-2
    #   recoverydays<-14
    #   hospitalizationrate<-3.4
    #   icurate<-.92
    #   ventilatorrate<-2.58
    #   hospitaltime<-3.5
    #   icutime<-4
    #   ventilatortime<-7
    #   #doubling<-8
    #   #Ro<-2.5          
    #   
    #   #Established Variables at the start for every county or populations
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
    #                              ventilatortime,daysforecasted,Ro, .5)
    #   
    #   MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    #   DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    #   TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    #   
    #   ####################################################################################
    #   #Lower Estimate
    #   #Established Variables at the start for every county or populations
    #   Ro<-2.5
    #   incubationtime<-5
    #   latenttime<-2
    #   recoverydays<-14
    #   hospitalizationrate<-5
    #   icurate<-6
    #   ventilatorrate<-3
    #   hospitaltime<-3.5
    #   icutime<-4
    #   ventilatortime<-7
    #   #Established Variables at the start for every county or populations
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
    #                              icutime,ventilatortime,daysforecasted,Ro, .5)
    #   
    #   DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    #   TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
    #   
    #   ####################################################################################
    #   #Upper Estimate
    #   #Established Variables at the start for every county or populations
    #   Ro<-2.5
    #   incubationtime<-5
    #   latenttime<-2
    #   recoverydays<-14
    #   hospitalizationrate<-5.5
    #   icurate<-6
    #   ventilatorrate<-3
    #   hospitaltime<-3.5
    #   icutime<-4
    #   ventilatortime<-7
    #   #Next we use the calculated values, along with estimated values from the Estimated Values.
    #   #Established Variables at the start for every county or populations
    #   
    #   #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #   #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    #   SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
    #                              socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
    #                              icutime,ventilatortime,daysforecasted,Ro, .5)
    #   
    #   DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    #   TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    #   colnames(DailyData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
    #   colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
    #   
    #   DailyData$`Expected Fatalities` <- round(DailyData$`Expected Fatalities`*(.25/5.5),0)
    #   DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`*(.15/4),0)
    #   DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`*(1/8),0)
    #   DailyData<-DailyData[-1,]
    #   DailyData$`Expected Fatalities`<-cumsum(DailyData$`Expected Fatalities`)
    #   DailyData$`Lower Estimate`<-cumsum(DailyData$`Lower Estimate`)
    #   DailyData$`Upper Estimate`<-cumsum(DailyData$`Upper Estimate`)
    #   chimelabel<-paste("CHIME_",socialdistancing,"%_SD",sep = "")
    #   DailyData$ID<-rep(chimelabel,nrow(DailyData))
    #   OverlayData<-rbind(OverlayData,DailyData)
    # }
    
    colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30 & ForecastDate <= Sys.Date())
    
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