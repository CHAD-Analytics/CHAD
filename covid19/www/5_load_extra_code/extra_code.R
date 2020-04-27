#Create list of hospitals, bases, and counties.
BaseList <- sort(AFBaseLocations$Base, decreasing = FALSE)
HospitalList <- HospitalInfo$NAME
CountyList <- CountyInfo$County
MAJCOMList <- sort(unique(AFBaseLocations$`Major Command`), decreasing = FALSE)
MAJCOMList<-c("All",'Active Duty',MAJCOMList)


# Is this used anywhere?
currCount = 0

##########################################################################################################
##########################################################################################################
# Create Charts for plotting lines showing trends among the virus  ------------------------------------------------------------------------------------------------------------------




# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------

AFrow = nrow(AFBaseLocations)
ForecastDataTable <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                                                                        "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                        "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                        "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                                                                        "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date"))

ForecastDataTableCases <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Cases Per 100,000", "Cases Per 10,000", "New Cases",
                                                                             "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                             "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                             "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                                                                             "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date"))


for (i in 2:AFrow){
  #Create Number of current cases and cases per 100,000 in a local area
  radius<-50
  MyCounties<-GetCounties(AFBaseLocations$Base[i],radius)
  CovidDataCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
  NewCases<-sum(rev(CovidDataCounties)[,1]-rev(CovidDataCounties)[,2])
  NewHospitalizations<-round(NewCases*.2)
  TotalPop<-CalculateCounties(MyCounties)
  TotalCases<-CalculateCovid(MyCounties)
  CasesPer100000<-round(TotalCases/TotalPop*100000)
  CasesPer10000<-round(TotalCases/TotalPop*10000)
  HospitalizationsPer100000<-round(CasesPer100000*.2)
  HospitalizationsPer10000<-round(HospitalizationsPer100000/10)
  
  
  #Create a datatable with just the forecasted values for every installation
  #Creating the stats and dataframes determined by the base we choose to look at.
  #IHME_Model is the initial import data table from global.R
  #BaseState<-AFBaseLocations$State[i] #dplyr::filter(AFBaseLocations, Base == baseinput)
  #IncludedHospitals<-GetHospitals() 
  #GetHospitals
  HospitalInfo$DistanceMiles = himd[,as.character(AFBaseLocations$Base[i])]
  MyHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= radius))
  MyHospitals<-dplyr::filter(MyHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
  
  IHME_State <- dplyr::filter(IHME_Model, State == AFBaseLocations$State[i])
  TotalBedsCounty <- sum(MyHospitals$BEDS)
  
  
  hospCounty <- subset(HospUtlzCounty, fips %in% MyCounties$FIPS)
  #Finds number of hospitals in radius
  TotalBeds<-sum(hospCounty$num_staffed_beds)
  #get historic utilization
  hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
  totalUsedBeds <- sum(hospCounty$bedsUsed)
  baseUtlz <- totalUsedBeds/TotalBeds
  
  #Get regional and state populations
  #MyCounties <- GetCounties()
  #GetCounties
  CountyInfo$DistanceMiles = cimd[,as.character(AFBaseLocations$Base[i])]
  MyCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius)
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
  HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
  HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
  HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21) #, HistoricalData*.15, HistoricalData*.27)
  colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations") #, "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
  
  StPopList <- dplyr::filter(CountyInfo, State == AFBaseLocations$State[i])
  RegPop <- sum(MyCounties$Population)
  StPop <- sum(StPopList$Population)
  
  # Use Population ratio to scale IHME
  PopRatio <- RegPop/StPop
  
  # Get total hospital bed number across state
  IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == AFBaseLocations$State[i])
  TotalBedsState <- sum(IncludedHospitalsST$BEDS)
  
  # Calculate bed ratio
  BedProp <- TotalBedsCounty/TotalBedsState
  
  # Apply ratio's to IHME data
  IHME_Region <- IHME_State
  IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
  #IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
  #IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
  IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean) #, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
  colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations") #, "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
  IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date())
  
  IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
  IHME_Region <- dplyr::arrange(IHME_Region,ForecastDate)
  
  DeathCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
  CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% MyCounties$FIPS)
  CountyDataTable<-cbind(MyCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
  CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
  colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
  
  ####################################################################################
  #Mean Estimate
  
  #Next we use the calculated values, along with estimated values from the Estimated Values. 
  #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
  #CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS) 
  ActiveCases<-rev(CovidCounties)[1:7]
  ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1],MyCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
  colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
  SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`)) 
  colnames(SIRinputs)<-c("cases","pop","doubling")
  
  #Established Variables at the start for every county or populations 
  cases<-SIRinputs$cases
  pop<-SIRinputs$pop
  
  if(nrow(IHME_Region) == 0 || pop == 0){
    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0)
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000", "New Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")    
    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
  }else{ 
    incubationtime<-5
    latenttime<-2
    doubling<-8 
    recoverydays<-14
    socialdistancing<-15
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    Ro<-2.5
    
    daysforecasted<-60
    SEIARProj<-SEIAR_Model_Run(cases,pop,incubationtime,latenttime,doubling,recoverydays,socialdistancing,hospitalizationrate,
                               icurate,ventilatorrate,hospitaltime,icutime,ventilatortime,daysforecasted,Ro,.5)
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate","Expected Hospitalizations")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData<-DailyData[-1,]
    DailyData<- dplyr::filter(DailyData, ForecastDate > Sys.Date())
    ########################################################################################
    SevDayVal<-round(DailyData$`Expected Hospitalizations`[7])
    FourteenDayVal<-round(DailyData$`Expected Hospitalizations`[14])
    ThirtyDayVal<-round(DailyData$`Expected Hospitalizations`[21])
    SixtyDayVal<-round(DailyData$`Expected Hospitalizations`[30])
    PeakSevDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:7]))
    PeakFourteenDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:14]))
    PeakThirtyDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:21]))
    PeakSixtyDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:30]))
    PeakDateSevDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:7])
    PeakDateFourteenDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:14])
    PeakDateThirtyDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:21])
    PeakDateSixtyDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:30])
    PeakDateSevDayVal<-format(DailyData$ForecastDate[PeakDateSevDayVal], format="%b-%d")
    PeakDateFourteenDayVal<-format(DailyData$ForecastDate[PeakDateFourteenDayVal], format="%b-%d")
    PeakDateThirtyDayVal<-format(DailyData$ForecastDate[PeakDateThirtyDayVal], format="%b-%d")
    PeakDateSixtyDayVal<-format(DailyData$ForecastDate[PeakDateSixtyDayVal], format="%b-%d")
    
    
    #BEGIN IHME CALCS
    I1 = round(IHME_Region$`Expected Hospitalizations`[7])
    I2 = round(IHME_Region$`Expected Hospitalizations`[14])
    I3 = round(IHME_Region$`Expected Hospitalizations`[21])
    I4 = round(IHME_Region$`Expected Hospitalizations`[30])
    
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:7])
    Peak<-IHME_Region[PeakDate,2]
    PI1<-round(Peak)
    PID1<-IHME_Region[PeakDate,1]
    PID1<-format(PID1, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:14])
    Peak<-IHME_Region[PeakDate,2]
    PI2<-round(Peak)
    PID2<-IHME_Region[PeakDate,1]
    PID2<-format(PID2, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:21])
    Peak<-IHME_Region[PeakDate,2]
    PI3<-round(Peak)
    PID3<-IHME_Region[PeakDate,1]
    PID3<-format(PID3, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:30])
    Peak<-IHME_Region[PeakDate,2]
    PI4<-round(Peak)
    PID4<-IHME_Region[PeakDate,1]
    PID4<-format(PID4, format="%b-%d")
    
    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), HospitalizationsPer100000, HospitalizationsPer10000, NewHospitalizations,
                        I1,PI1,PID1,SevDayVal,PeakSevDayVal,PeakDateSevDayVal,
                        I2,PI2,PID2,FourteenDayVal,PeakFourteenDayVal,PeakDateFourteenDayVal,
                        I3,PI3,PID3,ThirtyDayVal,PeakThirtyDayVal,PeakDateThirtyDayVal,
                        I4,PI4,PID4,SixtyDayVal,PeakSixtyDayVal,PeakDateSixtyDayVal) 
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000","New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")
    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), CasesPer100000, CasesPer10000, NewCases,
                             I1/.2,PI1/.2,PID1,SevDayVal/.2,PeakSevDayVal/.2,PeakDateSevDayVal,
                             I2/.2,PI2/.2,PID2,FourteenDayVal/.2,PeakFourteenDayVal/.2,PeakDateFourteenDayVal,
                             I3/.2,PI3/.2,PID3,ThirtyDayVal/.2,PeakThirtyDayVal/.2,PeakDateThirtyDayVal,
                             I4/.2,PI4/.2,PID4,SixtyDayVal/.2,PeakSixtyDayVal/.2,PeakDateSixtyDayVal) 
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000","New Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")
    ForecastDataTableCases <- rbind(ForecastDataTableCases,NewDFCases)
  }
}

ForecastDataTable$Installation<-as.character(ForecastDataTable$Installation)
ForecastDataTable<-ForecastDataTable %>% arrange(ForecastDataTable$Installation)

ForecastDataTableCases$Installation<-as.character(ForecastDataTableCases$Installation)
ForecastDataTableCases<-ForecastDataTableCases %>% arrange(ForecastDataTableCases$Installation)


#Create Top 15 Bases Report###################################################################################
TruncatedReport<-ForecastDataTable[order(ForecastDataTable$`Hopitalization Per 100,000`, decreasing = TRUE),]
TruncatedReport<-TruncatedReport %>% filter(MAJCOM != "ANG")
TruncatedReport<-TruncatedReport %>% filter(MAJCOM != "AFRC")
TruncatedReport<-TruncatedReport[,c(1,7,20:25)]
colnames(TruncatedReport)<-c("Installation","New Hospitalizations", "30 Day IHME (Hosp)","30 Day IHME Peak (Hosp)", "30 Day IHME Date (Hosp)", "30 Day CHIME (Hosp)", "30 Day CHIME Peak (Hosp)", "30 Day CHIME Date (Hosp)")

TruncatedReport2<-ForecastDataTableCases[order(ForecastDataTableCases$`Cases Per 100,000`, decreasing = TRUE),]
TruncatedReport2<-TruncatedReport2 %>% filter(MAJCOM != "ANG")
TruncatedReport2<-TruncatedReport2 %>% filter(MAJCOM != "AFRC")
TruncatedReport2<-TruncatedReport2[c(1:15),c(1,2,3,4,5,6,7,20:25)]
colnames(TruncatedReport2)<-c("Installation","MAJCOM","State", "Availab Beds", "Cases Per 100,000", "Cases Per 10,000", "Cases Today", "30 Day IHME (Cases)","30 Day IHME Peak (Cases)", "30 Day IHME Date (Cases)", "30 Day CHIME (Cases)", "30 Day CHIME Peak (Cases)", "30 Day CHIME Date (Cases)")

Top15Report<-join(TruncatedReport2, TruncatedReport, by = "Installation")
Top15Report<-Top15Report[,c(1,2,3,5,6,7,14,4,8,9,10,11,12,13,15,16,17,18,19,20)]
rm(TruncatedReport)
rm(TruncatedReport2)
##############################################################################################################


######################## Summary Tab Heat Map
HeatMapForecast<-merge(AFBaseLocations, ForecastDataTable, by.x = "Base", by.y = "Installation")
HeatMapForecast<-data.frame(HeatMapForecast$Base, HeatMapForecast$Location, HeatMapForecast$State.x, HeatMapForecast$`Major Command`, HeatMapForecast$Lat, HeatMapForecast$Long,HeatMapForecast$`Available Beds`,HeatMapForecast$`Hopitalization Per 100,000`,HeatMapForecast$`Hopitalization Per 10,000`,HeatMapForecast$`New Hospitalizations`,HeatMapForecast$`New Hospitalizations` ,HeatMapForecast$`7D SEIAR Forecast`, HeatMapForecast$`7D IHME Forecast`,HeatMapForecast$`14D SEIAR Forecast`,  HeatMapForecast$`14D IHME Forecast`,  HeatMapForecast$`21D SEIAR Forecast`, HeatMapForecast$`21D IHME Forecast`, HeatMapForecast$`30D SEIAR Forecast`, HeatMapForecast$`30D IHME Forecast`)
colnames(HeatMapForecast)<-c("Base","City","State","MAJCOM","Lat","Long","Beds","Hospitalizations Per 100,000","Hospitalizations Per 10,000","Today.CHIME","Today.IHME", "Seven.IHME","Seven.CHIME","Fourteen.IHME","Fourteen.CHIME","Twenty-One.IHME","Twenty-One.CHIME", "Thirty.IHME","Thirty.CHIME")
HeatMapForecast<-reshape(HeatMapForecast, direction='long', 
                         varying=c('Today.CHIME','Today.IHME','Seven.IHME', 'Seven.CHIME', 'Fourteen.IHME', 'Fourteen.CHIME','Twenty-One.IHME','Twenty-One.CHIME','Thirty.IHME','Thirty.CHIME'), 
                         timevar='Days',
                         times=c('Today','Seven', 'Fourteen',"Twenty-One","Thirty"),
                         v.names=c('CHIME', 'IHME'),
                         idvar=c('Base','City','State','MAJCOM','Lat','Long','Beds',"Hospitalizations Per 100,000","Hospitalizations Per 10,000"))
HeatMapForecast<-transform(HeatMapForecast,IHMEID=ifelse((Beds)>=IHME,"Under Capacity","Over Capacity"))
HeatMapForecast<-transform(HeatMapForecast,CHIMEID=ifelse((Beds)>=CHIME,"Under Capacity","Over Capacity"))


HeatMapForecastCases<-merge(AFBaseLocations, ForecastDataTableCases, by.x = "Base", by.y = "Installation")
HeatMapForecastCases<-data.frame(HeatMapForecastCases$Base, HeatMapForecastCases$Location, HeatMapForecastCases$State.x, HeatMapForecastCases$`Major Command`, HeatMapForecastCases$Lat, HeatMapForecastCases$Long,HeatMapForecastCases$`Available Beds`,HeatMapForecastCases$`Cases Per 100,000`,HeatMapForecastCases$`Cases Per 10,000`,HeatMapForecastCases$`New Cases`,HeatMapForecastCases$`New Cases` ,HeatMapForecastCases$`7D SEIAR Forecast`, HeatMapForecastCases$`7D IHME Forecast`,HeatMapForecastCases$`14D SEIAR Forecast`,  HeatMapForecastCases$`14D IHME Forecast`,  HeatMapForecastCases$`21D SEIAR Forecast`, HeatMapForecastCases$`21D IHME Forecast`, HeatMapForecastCases$`30D SEIAR Forecast`, HeatMapForecastCases$`30D IHME Forecast`)
colnames(HeatMapForecastCases)<-c("Base","City","State","MAJCOM","Lat","Long","Beds","Cases Per 100,000","Cases_Per_10000","Today.CHIME","Today.IHME", "Seven.IHME","Seven.CHIME","Fourteen.IHME","Fourteen.CHIME","Twenty-One.IHME","Twenty-One.CHIME","Thirty.IHME","Thirty.CHIME")
HeatMapForecastCases<-reshape(HeatMapForecastCases, direction='long', 
                              varying=c('Today.CHIME','Today.IHME','Seven.IHME', 'Seven.CHIME', 'Fourteen.IHME', 'Fourteen.CHIME','Twenty-One.IHME','Twenty-One.CHIME','Thirty.IHME','Thirty.CHIME'), 
                              timevar='Days',
                              times=c('Today','Seven', 'Fourteen',"Twenty-One","Thirty"),
                              v.names=c('CHIME', 'IHME'),
                              idvar=c('Base','City','State','MAJCOM','Lat','Long','Beds',"Cases Per 100,000","Cases_Per_10000"))
HeatMapForecastCases<-transform(HeatMapForecastCases,IHMEID=ifelse((Cases_Per_10000*10000*.05)>=IHME,"Under 5% Population","Over 5% Population"))
HeatMapForecastCases<-transform(HeatMapForecastCases,CHIMEID=ifelse((Cases_Per_10000*10000*.05)>=CHIME,"Under 5% Population","Over 5% Population"))
