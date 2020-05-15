NAFList <- sort(unique(AFNAFS$NAF), decreasing = FALSE)
NAFList <- c(NAFList) 
NAFID <- sort(unique(AFNAFS$ID), decreasing = FALSE)
NAFID <- c(NAFID) 
AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)
WingList <- sort(unique(AFWings$Wing), decreasing = FALSE)
WingList <- c("All",WingList)

#Create list of hospitals, bases, and counties.
BaseList <- sort(AFBaseLocations$Base, decreasing = FALSE)
HospitalList <- HospitalInfo$NAME
CountyList <- CountyInfo$County

BranchList <- sort(unique(AFBaseLocations$Branch), decreasing = FALSE)
AFBases<-dplyr::filter(AFBaseLocations,Branch %in% "Air Force")
BaseListP<-dplyr::filter(AFBases,Operational %in% "Active")
MAJCOMList <- sort(unique(AFBases$'Major Command'), decreasing = FALSE)
MAJCOMList<-c("All",MAJCOMList)

OperationalList <- sort(unique(AFBases$Operational), decreasing = FALSE)
OperationalListP <- sort(unique(AFBases$Operational), decreasing = FALSE)
# Is this used anywhere?
currCount = 0

##########################################################################################################
##########################################################################################################
# Create Charts for plotting lines showing trends among the virus  ------------------------------------------------------------------------------------------------------------------


# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------

AFrow = nrow(AFBaseLocations)
ForecastDataTable <- setNames(data.frame(matrix(ncol = 37, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                                                                        "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                        "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                        "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                                                                        "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                                                                        "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date"))

ForecastDataTableCases <- setNames(data.frame(matrix(ncol = 38, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Cases Per 100,000", "Cases Per 10,000", "New Cases","Total Cases",
                                                                             "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                             "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                             "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                                                                             "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                                                                             "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date"))

for (i in 2:AFrow){
  #Create Number of current cases and cases per 100,000 in a local area
  radius<-50
  baseDF = dplyr::filter(AFBaseLocations, Base == AFBaseLocations$Base[i])
  CountyInfo$DistanceMiles = cimd[,AFBaseLocations$Base[i]]
  MyCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius | FIPS == baseDF$FIPS)
  
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
  IHME_Region$confirmed_infections = round(IHME_State$est_infections_mean*PopRatio)  
  IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean, IHME_Region$est_infections_mean)
  colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations","Expected Cases")
  IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date())
  
  IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
  IHME_Region <- dplyr::arrange(IHME_Region,ForecastDate)
  
  DeathCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
  CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% MyCounties$FIPS)
  if (nrow(CovidCounties)<nrow(MyCounties)){
    diff1<-setdiff(MyCounties$FIPS, CovidCounties$CountyFIPS) 
    r<-which(MyCounties$FIPS == diff1)
    CovidCounties[seq(r+1,nrow(CovidCounties)+1),] <- CovidCounties[seq(r,nrow(CovidCounties)),]
    CovidCounties[r,] <- 0
  }
  if (nrow(DeathCounties)<nrow(MyCounties)){
    diff2<-setdiff(MyCounties$FIPS, DeathCounties$CountyFIPS)
    r<-which(MyCounties$FIPS == diff1)
    DeathCounties[seq(r+1,nrow(DeathCounties)+1),] <- DeathCounties[seq(r,nrow(DeathCounties)),]
    DeathCounties[r,] <- 0
  }  
  if (nrow(CaseRate)<nrow(MyCounties)){
    diff3<-setdiff(MyCounties$FIPS, CaseRate$CountyFIPS)
    r<-which(MyCounties$FIPS == diff1)
    CaseRate[seq(r+1,nrow(CaseRate)+1),] <- CaseRate[seq(r,nrow(CaseRate)),]
    CaseRate[r,] <- 0
  }  
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
                        0,0,0,0,0,0,
                        0,0,0,0,0,0)
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                      "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date")
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0,                             
                             0,0,0,0,0,0)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000", "New Cases","Total Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                           "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date")    
    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
  }else{ 
    
    doubling<-as.integer(CaseDblRate(MyCounties))
    if (doubling == 0) {
      doubling <- as.integer(40)      
    }
    
    Ro<-Estimate_Rt(MyCounties)
    if (Ro == "Undefined for Region"){
      Ro<-as.integer(1)
    } else if (Ro < 1){
      Ro<-as.integer(1)
    }
    
    incubationtime<-5
    latenttime<-2
    #doubling<-8 
    recoverydays<-14
    socialdistancing<-15
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    #Ro<-2.5
    
    daysforecasted<-120
    SEIARProj<-SEIAR_Model_Run(cases,pop,incubationtime,latenttime,doubling,recoverydays,socialdistancing,hospitalizationrate,
                               icurate,ventilatorrate,hospitaltime,icutime,ventilatortime,daysforecasted,Ro,.5)
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    DailyDataCases<-data.frame(MyDates, SEIARProj$sir$Id)    
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate","Expected Hospitalizations")
    colnames(DailyDataCases)<-c("ForecastDate","Expected Cases")    
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData<-DailyData[-1,]
    DailyData<- dplyr::filter(DailyData, ForecastDate > Sys.Date())
    DailyDataCases<-DailyDataCases[-1,]
    DailyDataCases<- dplyr::filter(DailyDataCases, ForecastDate > Sys.Date())    
    ########################################################################################
    SevDayVal<-round(DailyData$`Expected Hospitalizations`[7])
    FourteenDayVal<-round(DailyData$`Expected Hospitalizations`[14])
    TwentyOneDayVal<-round(DailyData$`Expected Hospitalizations`[21])
    ThirtyDayVal<-round(DailyData$`Expected Hospitalizations`[30])
    SixtyDayVal<-round(DailyData$`Expected Hospitalizations`[60])    
    PeakSevDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:7]))
    PeakFourteenDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:14]))
    PeakTwentyOneDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:21]))
    PeakThirtyDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:30]))
    PeakSixtyDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:60]))    
    PeakDateSevDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:7])
    PeakDateFourteenDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:14])
    PeakDateTwentyOneDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:21])
    PeakDateThirtyDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:30])
    PeakDateSixtyDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:60])    
    PeakDateSevDayVal<-format(DailyData$ForecastDate[PeakDateSevDayVal], format="%b-%d")
    PeakDateFourteenDayVal<-format(DailyData$ForecastDate[PeakDateFourteenDayVal], format="%b-%d")
    PeakDateTwentyOneDayVal<-format(DailyData$ForecastDate[PeakDateTwentyOneDayVal], format="%b-%d")
    PeakDateThirtyDayVal<-format(DailyData$ForecastDate[PeakDateThirtyDayVal], format="%b-%d")
    PeakDateSixtyDayVal<-format(DailyData$ForecastDate[PeakDateSixtyDayVal], format="%b-%d")
    
    #BEGIN IHME CALCS
    I1 = round(IHME_Region$`Expected Hospitalizations`[7])
    I2 = round(IHME_Region$`Expected Hospitalizations`[14])
    I3 = round(IHME_Region$`Expected Hospitalizations`[21])
    I4 = round(IHME_Region$`Expected Hospitalizations`[30])
    I5 = round(IHME_Region$`Expected Hospitalizations`[60])
    
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
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:60])
    Peak<-IHME_Region[PeakDate,2]
    PI5<-round(Peak)
    PID5<-IHME_Region[PeakDate,1]
    PID5<-format(PID5, format="%b-%d")
    
    ########################################################################################
    SevDayValCases<-round(DailyDataCases$`Expected Cases`[7])
    FourteenDayValCases<-round(DailyDataCases$`Expected Cases`[14])
    TwentyOneDayValCases<-round(DailyDataCases$`Expected Cases`[21])
    ThirtyDayValCases<-round(DailyDataCases$`Expected Cases`[30])
    SixtyDayValCases<-round(DailyDataCases$`Expected Cases`[60])    
    PeakSevDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:7]))
    PeakFourteenDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:14]))
    PeakTwentyOneDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:21]))
    PeakThirtyDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:30]))
    PeakSixtyDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:60]))    
    PeakDateSevDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:7])
    PeakDateFourteenDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:14])
    PeakDateTwentyOneDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:21])
    PeakDateThirtyDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:30])
    PeakDateSixtyDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:60])    
    PeakDateSevDayValCases<-format(DailyDataCases$ForecastDate[PeakDateSevDayValCases], format="%b-%d")
    PeakDateFourteenDayValCases<-format(DailyDataCases$ForecastDate[PeakDateFourteenDayValCases], format="%b-%d")
    PeakDateTwentyOneDayValCases<-format(DailyDataCases$ForecastDate[PeakDateTwentyOneDayValCases], format="%b-%d")
    PeakDateThirtyDayValCases<-format(DailyDataCases$ForecastDate[PeakDateThirtyDayValCases], format="%b-%d")
    PeakDateSixtyDayValCases<-format(DailyDataCases$ForecastDate[PeakDateSixtyDayValCases], format="%b-%d")
    
    #BEGIN IHME CALCS for CASES
    I1Cases = round(IHME_Region$`Expected Cases`[7])
    I2Cases = round(IHME_Region$`Expected Cases`[14])
    I3Cases = round(IHME_Region$`Expected Cases`[21])
    I4Cases = round(IHME_Region$`Expected Cases`[30])
    I5Cases = round(IHME_Region$`Expected Cases`[60])    
    
    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:7])
    Peak<-IHME_Region[PeakDate,2]
    PI1Cases<-round(Peak)
    PID1Cases<-IHME_Region[PeakDate,1]
    PID1Cases<-format(PID1, format="%b-%d")
    
    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:14])
    Peak<-IHME_Region[PeakDate,2]
    PI2Cases<-round(Peak)
    PID2Cases<-IHME_Region[PeakDate,1]
    PID2Cases<-format(PID2, format="%b-%d")
    
    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:21])
    Peak<-IHME_Region[PeakDate,2]
    PI3Cases<-round(Peak)
    PID3Cases<-IHME_Region[PeakDate,1]
    PID3Cases<-format(PID3, format="%b-%d")
    
    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:30])
    Peak<-IHME_Region[PeakDate,2]
    PI4Cases<-round(Peak)
    PID4Cases<-IHME_Region[PeakDate,1]
    PID4Cases<-format(PID4, format="%b-%d")
    
    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:60])
    Peak<-IHME_Region[PeakDate,2]
    PI5Cases<-round(Peak)
    PID5Cases<-IHME_Region[PeakDate,1]
    PID5Cases<-format(PID5, format="%b-%d")    
    
    ########################################################################################
    
    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), HospitalizationsPer100000, HospitalizationsPer10000, NewHospitalizations,
                        I1,PI1,PID1,SevDayVal,PeakSevDayVal,PeakDateSevDayVal,
                        I2,PI2,PID2,FourteenDayVal,PeakFourteenDayVal,PeakDateFourteenDayVal,
                        I3,PI3,PID3,TwentyOneDayVal,PeakTwentyOneDayVal,PeakDateTwentyOneDayVal,
                        I4,PI4,PID4,ThirtyDayVal,PeakThirtyDayVal,PeakDateThirtyDayVal,
                        I5,PI5,PID5,SixtyDayVal,PeakSixtyDayVal,PeakDateSixtyDayVal) 
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000","New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                      "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date")

    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), CasesPer100000, CasesPer10000, NewCases,TotalCases,
                             I1Cases,PI1Cases,PID1Cases,SevDayValCases,PeakSevDayValCases,PeakDateSevDayValCases,
                             I2Cases,PI2Cases,PID2Cases,FourteenDayValCases,PeakFourteenDayValCases,PeakDateFourteenDayValCases,
                             I3Cases,PI3Cases,PID3Cases,TwentyOneDayValCases,PeakTwentyOneDayValCases,PeakDateTwentyOneDayValCases,                             
                             I4Cases,PI4Cases,PID4Cases,ThirtyDayValCases,PeakThirtyDayValCases,PeakDateThirtyDayValCases,
                             I5Cases,PI5Cases,PID5Cases,SixtyDayValCases,PeakSixtyDayValCases,PeakDateSixtyDayValCases) 
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000","New Cases","Total Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                           "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date") 
    
    ForecastDataTableCases <- rbind(ForecastDataTableCases,NewDFCases)
  }
}

ForecastDataTable$Installation<-as.character(ForecastDataTable$Installation)
ForecastDataTable<-ForecastDataTable %>% arrange(ForecastDataTable$Installation)

ForecastDataTableCases$Installation<-as.character(ForecastDataTableCases$Installation)
ForecastDataTableCases<-ForecastDataTableCases %>% arrange(ForecastDataTableCases$Installation)


#write.csv(ForecastDataTable,"C:/Users/taylo/Documents/CHAD/ForecastDataTable.csv", row.names = FALSE)
#write.csv(ForecastDataTableCases,"C:/Users/taylo/Documents/CHAD/ForecastDataTableCases.csv", row.names = FALSE)


#Create Top 15 Bases Report###################################################################################
TruncatedReport<-ForecastDataTable[order(ForecastDataTable$`Hopitalization Per 100,000`, decreasing = TRUE),]
TruncatedReport<-TruncatedReport %>% filter(MAJCOM != "ANG")
TruncatedReport<-TruncatedReport %>% filter(MAJCOM != "AFRC")
TruncatedReport<-TruncatedReport[,c(1,7,20:25)]
colnames(TruncatedReport)<-c("Installation","New Hospitalizations", "30 Day IHME (Hosp)","30 Day IHME Peak (Hosp)", "30 Day IHME Date (Hosp)", "30 Day CHIME (Hosp)", "30 Day CHIME Peak (Hosp)", "30 Day CHIME Date (Hosp)")

TruncatedReport2<-ForecastDataTableCases[order(ForecastDataTableCases$`Cases Per 100,000`, decreasing = TRUE),]
TruncatedReport2<-TruncatedReport2 %>% filter(MAJCOM != "ANG")
TruncatedReport2<-TruncatedReport2 %>% filter(MAJCOM != "AFRC")
TruncatedReport2<-TruncatedReport2[c(1:16),c(1,2,3,4,5,6,7,8,20:25)]
colnames(TruncatedReport2)<-c("Installation","MAJCOM","State", "Availab Beds", "Cases Per 100,000", "Cases Per 10,000", "Cases Today","Total Cases", "30 Day IHME (Cases)","30 Day IHME Peak (Cases)", "30 Day IHME Date (Cases)", "30 Day CHIME (Cases)", "30 Day CHIME Peak (Cases)", "30 Day CHIME Date (Cases)")

Top15Report<-join(TruncatedReport2, TruncatedReport, by = "Installation")
Top15Report<-Top15Report[,c(1,2,3,5,6,7,14,4,8,9,10,11,12,13,15,16,17,18,19,20)]
rm(TruncatedReport)
rm(TruncatedReport2)
##############################################################################################################



# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------
AFrow = nrow(AFBaseLocations)
ForecastDataTableOneMile <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                                                                        "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                        "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                        "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                                                                        "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date"))

ForecastDataTableCasesOneMile <- setNames(data.frame(matrix(ncol = 32, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Cases Per 100,000", "Cases Per 10,000", "New Cases","Total Cases",
                                                                             "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                             "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                             "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                                                                             "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date"))
##Repeat the above process to generate all of the same information for the single county the base is in (SG request)
for (i in 2:AFrow){
  #Create Number of current cases and cases per 100,000 in a local area
  radius<-10
  baseDF = dplyr::filter(AFBaseLocations, Base == AFBaseLocations$Base[i])
  CountyInfo$DistanceMiles = cimd[,AFBaseLocations$Base[i]]
  MyCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius | FIPS == baseDF$FIPS)
  
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
  IHME_Region$confirmed_infections = round(IHME_State$est_infections_mean*PopRatio)  
  IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean, IHME_Region$est_infections_mean)
  colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations","Expected Cases")
  IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date())
  
  IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
  IHME_Region <- dplyr::arrange(IHME_Region,ForecastDate)
  
  DeathCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
  CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% MyCounties$FIPS)
  if (nrow(CovidCounties)<nrow(MyCounties)){
      diff1<-setdiff(MyCounties$FIPS, CovidCounties$CountyFIPS) 
      r<-which(MyCounties$FIPS == diff1)
      CovidCounties[seq(r+1,nrow(CovidCounties)+1),] <- CovidCounties[seq(r,nrow(CovidCounties)),]
      CovidCounties[r,] <- 0
  }
  if (nrow(DeathCounties)<nrow(MyCounties)){
      diff2<-setdiff(MyCounties$FIPS, DeathCounties$CountyFIPS)
      r<-which(MyCounties$FIPS == diff1)
      DeathCounties[seq(r+1,nrow(DeathCounties)+1),] <- DeathCounties[seq(r,nrow(DeathCounties)),]
      DeathCounties[r,] <- 0
  }  
  if (nrow(CaseRate)<nrow(MyCounties)){
      diff3<-setdiff(MyCounties$FIPS, CaseRate$CountyFIPS)
      r<-which(MyCounties$FIPS == diff1)
      CaseRate[seq(r+1,nrow(CaseRate)+1),] <- CaseRate[seq(r,nrow(CaseRate)),]
      CaseRate[r,] <- 0
  }
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
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000", "New Cases","Total Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")    
    ForecastDataTableOneMile <- rbind(ForecastDataTableOneMile,NewDF)
  }else{ 
    doubling<-as.integer(CaseDblRate(MyCounties))
    if (doubling == 0) {
      doubling <- as.integer(40)      
    }

    Ro<-Estimate_Rt(MyCounties)
    if (Ro == "Undefined for Region"){
      Ro<-as.integer(1)
    } else if (Ro < 1){
      Ro<-as.integer(1)
    }
    
    incubationtime<-5
    latenttime<-2
    #doubling<-8 
    recoverydays<-14
    socialdistancing<-15
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    #Ro<-2.5
    
    
    
    daysforecasted<-60
    SEIARProj<-SEIAR_Model_Run(cases,pop,incubationtime,latenttime,doubling,recoverydays,socialdistancing,hospitalizationrate,
                               icurate,ventilatorrate,hospitaltime,icutime,ventilatortime,daysforecasted,Ro,.5)
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    DailyDataCases<-data.frame(MyDates, SEIARProj$sir$Id)    
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate","Expected Hospitalizations")
    colnames(DailyDataCases)<-c("ForecastDate","Expected Cases")    
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData<-DailyData[-1,]
    DailyData<- dplyr::filter(DailyData, ForecastDate > Sys.Date())
    DailyDataCases<-DailyDataCases[-1,]
    DailyDataCases<- dplyr::filter(DailyDataCases, ForecastDate > Sys.Date())    
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

    ########################################################################################
    #All of the case information from the CHIME model
    SevDayValCases<-round(DailyDataCases$`Expected Cases`[7])
    FourteenDayValCases<-round(DailyDataCases$`Expected Cases`[14])
    ThirtyDayValCases<-round(DailyDataCases$`Expected Cases`[21])
    SixtyDayValCases<-round(DailyDataCases$`Expected Cases`[30])
    PeakSevDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:7]))
    PeakFourteenDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:14]))
    PeakThirtyDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:21]))
    PeakSixtyDayValCases<-round(max(DailyDataCases$`Expected Cases`[1:30]))
    PeakDateSevDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:7])
    PeakDateFourteenDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:14])
    PeakDateThirtyDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:21])
    PeakDateSixtyDayValCases<-which.max(DailyDataCases$`Expected Cases`[1:30])
    PeakDateSevDayValCases<-format(DailyDataCases$ForecastDate[PeakDateSevDayValCases], format="%b-%d")
    PeakDateFourteenDayValCases<-format(DailyDataCases$ForecastDate[PeakDateFourteenDayValCases], format="%b-%d")
    PeakDateThirtyDayValCases<-format(DailyDataCases$ForecastDate[PeakDateThirtyDayValCases], format="%b-%d")
    PeakDateSixtyDayValCases<-format(DailyDataCases$ForecastDate[PeakDateSixtyDayValCases], format="%b-%d")
    
    #BEGIN IHME CALCS for CASES
    I1Cases = round(IHME_Region$`Expected Cases`[7])
    I2Cases = round(IHME_Region$`Expected Cases`[14])
    I3Cases = round(IHME_Region$`Expected Cases`[21])
    I4Cases = round(IHME_Region$`Expected Cases`[30])
    
    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:7])
    Peak<-IHME_Region[PeakDate,2]
    PI1Cases<-round(Peak)
    PID1Cases<-IHME_Region[PeakDate,1]
    PID1Cases<-format(PID1, format="%b-%d")

    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:14])
    Peak<-IHME_Region[PeakDate,2]
    PI2Cases<-round(Peak)
    PID2Cases<-IHME_Region[PeakDate,1]
    PID2Cases<-format(PID2, format="%b-%d")

    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:21])
    Peak<-IHME_Region[PeakDate,2]
    PI3Cases<-round(Peak)
    PID3Cases<-IHME_Region[PeakDate,1]
    PID3Cases<-format(PID3, format="%b-%d")

    PeakDate<-which.max(IHME_Region$`Expected Cases`[1:30])
    Peak<-IHME_Region[PeakDate,2]
    PI4Cases<-round(Peak)
    PID4Cases<-IHME_Region[PeakDate,1]
    PID4Cases<-format(PID4, format="%b-%d")
    ########################################################################################
    
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
    ForecastDataTableOneMile <- rbind(ForecastDataTableOneMile,NewDF)
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), CasesPer100000, CasesPer10000, NewCases,TotalCases,
                             I1Cases,PI1Cases,PID1Cases,SevDayValCases,PeakSevDayValCases,PeakDateSevDayValCases,
                             I2Cases,PI2Cases,PID2Cases,FourteenDayValCases,PeakFourteenDayValCases,PeakDateFourteenDayValCases,
                             I3Cases,PI3Cases,PID3Cases,ThirtyDayValCases,PeakThirtyDayValCases,PeakDateThirtyDayValCases,
                             I4Cases,PI4Cases,PID4Cases,SixtyDayValCases,PeakSixtyDayValCases,PeakDateSixtyDayValCases) 
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000","New Cases","Total Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")
    ForecastDataTableCasesOneMile <- rbind(ForecastDataTableCasesOneMile,NewDFCases)
  }
}

ForecastDataTableOneMile$Installation<-as.character(ForecastDataTableOneMile$Installation)
ForecastDataTableOneMile<-ForecastDataTableOneMile %>% arrange(ForecastDataTableOneMile$Installation)

ForecastDataTableCasesOneMile$Installation<-as.character(ForecastDataTableCasesOneMile$Installation)
ForecastDataTableCasesOneMile<-ForecastDataTableCasesOneMile %>% arrange(ForecastDataTableCasesOneMile$Installation)


#Create Top 15 Bases Report###################################################################################
TruncatedReport<-ForecastDataTableOneMile[order(ForecastDataTableOneMile$`Hopitalization Per 100,000`, decreasing = TRUE),]
TruncatedReport<-TruncatedReport %>% filter(MAJCOM != "ANG")
TruncatedReport<-TruncatedReport %>% filter(MAJCOM != "AFRC")
TruncatedReport<-TruncatedReport[,c(1,7,20:25)]
colnames(TruncatedReport)<-c("Installation","New Hospitalizations", "30 Day IHME (Hosp)","30 Day IHME Peak (Hosp)", "30 Day IHME Date (Hosp)", "30 Day CHIME (Hosp)", "30 Day CHIME Peak (Hosp)", "30 Day CHIME Date (Hosp)")

TruncatedReport2<-ForecastDataTableCasesOneMile[order(ForecastDataTableCasesOneMile$`Cases Per 100,000`, decreasing = TRUE),]
TruncatedReport2<-TruncatedReport2 %>% filter(MAJCOM != "ANG")
TruncatedReport2<-TruncatedReport2 %>% filter(MAJCOM != "AFRC")
TruncatedReport2<-TruncatedReport2[c(1:16),c(1,2,3,4,5,6,7,8,20:25)]
colnames(TruncatedReport2)<-c("Installation","MAJCOM","State", "Availab Beds", "Cases Per 100,000", "Cases Per 10,000", "Cases Today","Total Cases", "30 Day IHME (Cases)","30 Day IHME Peak (Cases)", "30 Day IHME Date (Cases)", "30 Day CHIME (Cases)", "30 Day CHIME Peak (Cases)", "30 Day CHIME Date (Cases)")

Top15ReportOneMile<-join(TruncatedReport2, TruncatedReport, by = "Installation")
Top15ReportOneMile<-Top15ReportOneMile[,c(1,2,3,5,6,7,14,4,8,9,10,11,12,13,15,16,17,18,19,20)]
rm(TruncatedReport)
rm(TruncatedReport2)
##############################################################################################################



######################## Summary Tab Heat Map
HeatMapForecast<-merge(AFBaseLocations, ForecastDataTable, by.x = c("Base","State","Major Command"), by.y = c("Installation","State","MAJCOM"))
HeatMapForecast<-data.frame(HeatMapForecast$Base,HeatMapForecast$State,HeatMapForecast$Branch,HeatMapForecast$Operational,HeatMapForecast$`Major Command`, 
                            HeatMapForecast$Lat, HeatMapForecast$Long,HeatMapForecast$`Available Beds`,HeatMapForecast$`Hopitalization Per 100,000`,HeatMapForecast$`Hopitalization Per 10,000`,
                            HeatMapForecast$`New Hospitalizations`,HeatMapForecast$`New Hospitalizations`,HeatMapForecast$`7D SEIAR Forecast`, HeatMapForecast$`7D IHME Forecast`,
                            HeatMapForecast$`14D SEIAR Forecast`,HeatMapForecast$`14D IHME Forecast`,  HeatMapForecast$`21D SEIAR Forecast`, HeatMapForecast$`21D IHME Forecast`, 
                            HeatMapForecast$`30D SEIAR Forecast`, HeatMapForecast$`30D IHME Forecast`,HeatMapForecast$ID)
colnames(HeatMapForecast)<-c("Base","State","Branch","Operational","MAJCOM","Lat","Long","Beds","Hospitalizations Per 100,000","Hospitalizations Per 10,000","Today.CHIME","Today.IHME", "Seven.IHME","Seven.CHIME",
                            "Fourteen.IHME","Fourteen.CHIME","Twenty-One.IHME","Twenty-One.CHIME", "Thirty.IHME","Thirty.CHIME","ID")
HeatMapForecast<-reshape(HeatMapForecast, direction='long', 
                         varying=c('Today.CHIME','Today.IHME','Seven.IHME','Seven.CHIME','Fourteen.IHME','Fourteen.CHIME','Twenty-One.IHME','Twenty-One.CHIME','Thirty.IHME','Thirty.CHIME'), 
                         timevar='Days',
                         times=c('Today','Seven', 'Fourteen',"Twenty-One","Thirty"),
                         v.names=c('CHIME', 'IHME'),
                         idvar=c("Base","State","Branch","Operational","MAJCOM","Lat","Long","Beds","Hospitalizations Per 100,000","Hospitalizations Per 10,000","ID"))
HeatMapForecast<-transform(HeatMapForecast,IHMEID=ifelse((Beds)>=IHME,"Under Capacity","Over Capacity"))
HeatMapForecast<-transform(HeatMapForecast,CHIMEID=ifelse((Beds)>=CHIME,"Under Capacity","Over Capacity"))


HeatMapForecastCases<-merge(AFBaseLocations, ForecastDataTableCases, by.x = c("Base","State","Major Command"), by.y = c("Installation","State","MAJCOM"))
HeatMapForecastCases<-data.frame(HeatMapForecastCases$Base, HeatMapForecastCases$State,HeatMapForecastCases$Branch,HeatMapForecastCases$Operational,HeatMapForecastCases$`Major Command`,
                                 HeatMapForecastCases$Lat, HeatMapForecastCases$Long,HeatMapForecastCases$`Available Beds`,HeatMapForecastCases$`Cases Per 100,000`,HeatMapForecastCases$`Cases Per 10,000`,
                                 HeatMapForecastCases$`New Cases`,HeatMapForecastCases$`New Cases`,HeatMapForecastCases$`7D SEIAR Forecast`, HeatMapForecastCases$`7D IHME Forecast`,
                                 HeatMapForecastCases$`14D SEIAR Forecast`,  HeatMapForecastCases$`14D IHME Forecast`,  HeatMapForecastCases$`21D SEIAR Forecast`, HeatMapForecastCases$`21D IHME Forecast`, 
                                 HeatMapForecastCases$`30D SEIAR Forecast`, HeatMapForecastCases$`30D IHME Forecast`,HeatMapForecastCases$ID)
colnames(HeatMapForecastCases)<-c("Base","State","Branch","Operational","MAJCOM","Lat","Long","Beds","Cases Per 100,000","Cases_Per_10000","Today.CHIME","Today.IHME", "Seven.IHME","Seven.CHIME",
                                  "Fourteen.IHME","Fourteen.CHIME","Twenty-One.IHME","Twenty-One.CHIME","Thirty.IHME","Thirty.CHIME","ID")
HeatMapForecastCases<-reshape(HeatMapForecastCases, direction='long', 
                              varying=c('Today.CHIME','Today.IHME','Seven.IHME', 'Seven.CHIME', 'Fourteen.IHME', 'Fourteen.CHIME','Twenty-One.IHME','Twenty-One.CHIME','Thirty.IHME','Thirty.CHIME'), 
                              timevar='Days',
                              times=c('Today','Seven', 'Fourteen',"Twenty-One","Thirty"),
                              v.names=c('CHIME', 'IHME'),
                              idvar=c('Base','State','Branch','Operational','MAJCOM','Lat','Long','Beds',"Cases Per 100,000","Cases_Per_10000","ID"))
HeatMapForecastCases<-transform(HeatMapForecastCases,IHMEID=ifelse((Cases_Per_10000*10000*.05)>=IHME,"Under 5% Population","Over 5% Population"))
HeatMapForecastCases<-transform(HeatMapForecastCases,CHIMEID=ifelse((Cases_Per_10000*10000*.05)>=CHIME,"Under 5% Population","Over 5% Population"))


########################################################################################################################################################################
################################################# HOT SPOT CHART DATA INITIALIZE #######################################################################################
########################################################################################################################################################################



#convert cases and death dataframes to long format. Also add in new_cases in last 1, 3, and 30 days
tempCases = CovidConfirmedCases %>% select(-State, -stateFIPS) %>%
  reshape2::melt(id.var = c('CountyFIPS','County Name'), variable.name = 'date', value.name = "cumulative_cases") %>%
  mutate(date = as.Date(str_replace(date, "X",""), format = "%m/%d/%y"), `County Name` = as.character(`County Name`)) %>%
  distinct(CountyFIPS,date,.keep_all = TRUE)
CasesGrowth <- tempCases %>% group_by(CountyFIPS) %>% arrange(CountyFIPS, date) %>%
  dplyr::mutate(new_cases_1 = cumulative_cases - lag(cumulative_cases, 1), 
                new_cases_3_days = cumulative_cases - lag(cumulative_cases,3),
                new_cases_30_days = cumulative_cases - lag(cumulative_cases, 30),
                case_growth = ifelse(is.nan(new_cases_3_days/(new_cases_30_days)), 0,
                                     (new_cases_3_days/(new_cases_30_days))),
                new_cases_7_days = cumulative_cases - lag(cumulative_cases,7),
                new_cases_14_days = cumulative_cases - lag(cumulative_cases,14),
                case_growth_week = ifelse(is.nan(new_cases_7_days/(new_cases_14_days - new_cases_7_days)), 0,
                                          ifelse(is.infinite(new_cases_7_days/(new_cases_14_days  - new_cases_7_days)),0,
                                                 new_cases_7_days/(new_cases_14_days  - new_cases_7_days)))) %>%
  replace(is.na(.),0)

tempDeaths = CovidDeaths %>% select(-State, -stateFIPS) %>%
  reshape2::melt(id.var = c('CountyFIPS','County Name'), variable.name = 'date', value.name = "cumulative_deaths") %>%
  mutate(date = as.Date(str_replace(date, "X",""), format = "%m/%d/%y"), `County Name` = as.character(`County Name`)) %>%
  distinct(CountyFIPS,date,.keep_all = TRUE) %>% mutate(cumulative_deaths = ifelse(is.na(cumulative_deaths), 0, cumulative_deaths))

# join Cases and deaths dataframes
Growth = CasesGrowth %>% left_join(tempDeaths, by = c("CountyFIPS" = "CountyFIPS", "date" = "date", "County Name" = "County Name"))
# deaths_1_days = lag(cumulative_deaths,1),
# deaths_3_days = cumulative_deaths - lag(cumulative_deaths,3), 
# deaths_10_days = cumulative_deaths - lag(cumulative_deaths,10)) 

#Join case data with county population data
Growth = Growth %>% left_join(CountyInfo %>% select(FIPS, Population), by = c("CountyFIPS" = "FIPS")) %>% 
  mutate(Population = ifelse(is.na(Population), 0, Population),
         cumulative_deaths = ifelse(is.na(cumulative_deaths), 0, cumulative_deaths))


#######
Growth = dplyr::filter(Growth, Population != 0)
Growth <- Growth %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

#########

# Function to fix 4-letter FIPS
fix.fips <- function(column){
  column <- str_pad(column, width=5, side="left", pad="0")
  return(column)
}
# per capita calcs
Growth = Growth %>% mutate(new_cases_1_pp = new_cases_1*100000/Population, new_cases_3_pp = new_cases_3_days*100000/Population,
                           new_cases_30_pp = new_cases_30_days*100000/Population, cases_pp = cumulative_cases*100000/Population,
                           new_cases_7_pp = new_cases_7_days*100000/Population, new_cases_14_pp = new_cases_14_days*100000/Population,
                           deaths_pp = cumulative_deaths*100000/Population) %>% ungroup() %>%
  mutate(CountyFIPS = fix.fips(CountyFIPS))

# Convert cimd dataframe to long format and filter to within 50 miles of base
rownames(cimd) = CountyInfo$FIPS
cimd_long <- cimd %>% rownames_to_column(var= "FIPS")
cimd_long <- cimd_long %>% gather(-c(FIPS), key = base, value = DistanceMiles) 
Bases50 <- cimd_long %>% filter(DistanceMiles <= 50) %>% mutate(FIPS = fix.fips(FIPS))

#test code
# Bases50 %>% filter(base == 'Pentagon') %>% left_join(Growth, by = c("FIPS" = "CountyFIPS")) %>%filter(date == current_date)

#join base data with county growth data. aggregate county data to base-radius level. also add back in the MAJCOM column 
bases_radius <- Bases50 %>% left_join(Growth, by = c("FIPS" = "CountyFIPS")) %>% dplyr::group_by(base, date) %>% 
  dplyr::summarise(cumulative_cases = sum(cumulative_cases),  new_cases_1 = sum(new_cases_1),
                   new_cases_3_days = sum(new_cases_3_days),new_cases_7_days = sum(new_cases_7_days),
                   new_cases_14_days = sum(new_cases_14_days),new_cases_30_days = sum(new_cases_30_days),
                   cumulative_deaths = sum(cumulative_deaths), Population = sum(Population)
  ) %>% 
  mutate(cases_pp = cumulative_cases/Population,
         new_cases_1_pp = new_cases_1/Population*100000, new_cases_3_pp = new_cases_3_days/Population*100000,
         new_cases_7_pp = new_cases_7_days/Population*100000, new_cases_14_pp = new_cases_14_days/Population*100000,
         new_cases_30_pp = new_cases_30_days/Population*100000, deaths_pp = cumulative_deaths/Population*100000,
         case_growth = ifelse(is.nan(new_cases_3_pp/(new_cases_3_pp + new_cases_30_pp)), 0,
                              ifelse(is.infinite(new_cases_3_pp/(new_cases_3_pp +new_cases_30_pp)),0,
                                     new_cases_3_pp/(new_cases_3_pp +new_cases_30_pp))),
         case_growth_week = ifelse(is.nan(new_cases_7_pp/(new_cases_14_pp - new_cases_7_pp)), 0,
                                   ifelse(is.infinite(new_cases_7_pp/(new_cases_14_pp  - new_cases_7_pp)),0,
                                          new_cases_7_pp/(new_cases_14_pp  - new_cases_7_pp)))
  )

bases_radius = bases_radius %>% left_join(AFBaseLocations %>% select(Base,Branch,Operational,'Major Command'), by = c("base" = "Base"))