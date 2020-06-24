#' #' Create charts for projecting local health data
#' @details EXTRA EXTRA Need to find a better way later to replace this 
#'          probably have the overlay function return a list with two objects. 
#'          need the data.frame from overlay in the report
PlotOverlay2<-function(ChosenBase, IncludedCounties,IncludedHospitals,DaysProjected,CONUSSelect){

    # ###Uncomment to test plot function without running the app
    # #i<-80
    # #ChosenBase = AFBaseLocations$Base[i]
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
    # ####
    # ####
  
    #Establish initial inputs such as base, counties, and filter IHME model
    BaseState<-dplyr::filter(AFBaseLocations, Base == toString(ChosenBase))
    if (CONUSSelect == "CONUS"){
        hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
        TTBCounty <- sum(IncludedHospitals$BEDS)
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
    } else {
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$Country[1]))
    }
  
    #Get regional and state populations
    RegPop <- sum(IncludedCounties$Population)
    StPop <- sum(StPopList$Population)
    
    # Use Population ratio to scale IHME
    PopRatio <- RegPop/StPop
  
    if (CONUSSelect == "CONUS"){
        #Torch_State <- dplyr::filter(torchData, State == toString(BaseState$State[1]))  
        Torch_State<-dplyr::filter(torchData,FIP %in% IncludedCounties$FIPS)
        #startdate <- "2020-05-22"
        #startdate <-as.Date(startdate, "%Y-%m-%d")
        #datediff <- as.numeric(Sys.Date()-startdate)
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
      HistoricalDataHosp<-colSums(HistoricalDataDaily*.055)
      
      #Create dataframe to hold daily hospitalizations
      #HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
      HistoricalDates = as.Date(names(HistoricalDataHosp), "%m/%d/%y")
      HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
      colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")      
    }
    
    currHosp = HistoricalData[nrow(HistoricalData),2]

    if (CONUSSelect == "CONUS"){
        #Torch_State<-subset(Torch_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State))    
        Torch_State$Date <- as.Date(Torch_State$Date, "%m/%d/%y")
        Torch_State<-dplyr::filter(Torch_State,Date >= Sys.Date())
        Torch_State<-aggregate(Torch_State[,sapply(Torch_State,is.numeric)],Torch_State["Date"],sum)
        #Torch_State<-Torch_State[1:DaysProjected,]
        Torch_Hosp<-data.frame(Torch_State$Date,Torch_State$HCasesEst,Torch_State$HCasesEstLow,Torch_State$HCasesEstUp)
        colnames(Torch_Hosp)<-c("ForecastDate","Beds","Lower Estimate","Upper Estimate")
        Torch_ICU<-data.frame(Torch_State$Date,Torch_State$ICUCasesEst,Torch_State$ICUCasesEstLow,Torch_State$ICUCasesEstUp)
        colnames(Torch_ICU)<-c("ForecastDate","Beds","Lower Estimate","Upper Estimate")

        Torch_Hosp$ID<-rep("Estimated COVID-19 Cases in Hospital",nrow(Torch_Hosp))
        Torch_ICU$ID<-rep("Estimated COVID-19 Cases in ICU",nrow(Torch_ICU))
        OverlayData<-rbind(Torch_Hosp,Torch_ICU) 
                
        Torch_HospAvail<-data.frame(Torch_State$Date,Torch_State$EstHospBedsAvail)
        colnames(Torch_HospAvail)<-c("ForecastDate"," Estimated Available Hospital Beds")
        
        Torch_ICUAvail<-data.frame(Torch_State$Date,Torch_State$EstICUBedsAvail)
        colnames(Torch_ICUAvail)<-c("ForecastDate","Estimated Available ICU Beds")        
        
        # UTPeak<-round(max(UT_Data$`Expected Hospitalizations`[1:DaysProjected]))
        # UTDate<-which.max(UT_Data$`Expected Hospitalizations`[1:DaysProjected])
        # UTDate<-format(UT_Data$ForecastDate[UTDate], format="%b-%d")    
        # PeakDates<-rbind(PeakDates,UTDate)
        # PeakValues<-rbind(PeakValues,UTPeak)    
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
    
    
    # HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    # HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
    # OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
    # 
    # OverlayData<- dplyr::filter(OverlayData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
    # 
    # OverlayData<-rbind(HistoricalData, OverlayData)
    
    #########
    #########
    #OverlayData<-subset(OverlayData, ID %in% ModelIDList)
    
    
    hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
    #Finds number of hospitals in radius
    #TotalBeds<-sum(hospCounty$num_staffed_beds)
    #get historic utilization
    hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
    #totalUsedBeds <- sum(hospCounty$bedsUsed)
    #baseUtlz <- totalUsedBeds/TotalBeds
    #bcap = TotalBeds * (1-baseUtlz)
    Torch_HospAvail <- unique(Torch_HospAvail$` Estimated Available Hospital Beds`)
    Torch_HospAvail <- Torch_HospAvail[1]
    Torch_ICUAvail <- unique(Torch_ICUAvail$` Estimated Available ICU Beds`)
    Torch_ICUAvail <- Torch_ICUAvail[1]    
    
    
    projections <-  
      ggplot(OverlayData, aes(x=ForecastDate, y=`Beds`, color = ID, fill = ID, linetype = ID)) +
      geom_line(aes(linetype = ID, color = ID)) + 
      geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2) +
      
      # ggplot(Torch_Hosp, aes(x=ForecastDate,y='Estimated COVID-19 Cases in Hospital`, color = "red")) +
      # geom_line(aes(linetype = ID, color = ID)) + 
      # geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2) +
      # 
      # ggplot(Torch_ICU, aes(x=ForecastDate,y='Estimated COVID-19 Cases in ICU', color = "blue")) +
      # geom_line(aes(linetype = ID, color = ID)) + 
      # geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2) +      

      #scale_colour_manual(values=c("tan", "blue", "black","red"))+
      #scale_fill_manual(values = c("tan4", "cadetblue", "gray","red"))+
      #scale_linetype_manual(values=c("dashed", "solid", "dashed", "solid"))+
      #geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`),alpha = .2)+

      geom_hline(aes(yintercept = Torch_HospAvail,linetype = "Estimated Available Hospital Beds"),colour = "red")+  #,linetype = "dashed") +
      geom_hline(aes(yintercept = Torch_ICUAvail,linetype = "Estimated Available ICU Beds"),colour = "blue")+  #,linetype = "dashed") +      
      #geom_hline(aes(yintercept = bcap,linetype = "Estimated COVID Patient Bed Capacity"),colour = "red")+
      ggtitle("Estimated Active COVID-19 Cases")+
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
  
}