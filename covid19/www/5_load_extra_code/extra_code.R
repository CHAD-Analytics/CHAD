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

MTFSummaryReport <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),c("Installation","MAJCOM","State","50MileTotalCases","50MileNewCases","50MileCasesPer1000","50MileDblRate"))

ForecastDataTable <- setNames(data.frame(matrix(ncol = 37, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                                                                        "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                                                                        "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                                                                        "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                                                                        "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date",
                                                                        "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D CAA Forecast","60D CAA Peak","60D CAA Peak Date"))

ForecastDataTableCases <- setNames(data.frame(matrix(ncol = 38, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Cases Per 100,000", "Cases Per 10,000", "New Cases","Total Cases",
                                                                             "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                                                                             "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                                                                             "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                                                                             "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date",
                                                                             "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D CAA Forecast","60D CAA Peak","60D CAA Peak Date"))

for (i in 2:AFrow){
  #Create Number of current cases and cases per 100,000 in a local area
  #i = 96
  radius = 50

  # Get Counties
  baseDF = dplyr::filter(AFBaseLocations, Base == AFBaseLocations$Base[i])
  baseDF = baseDF[1,]
  base = baseDF$Base
  CountyInfo$DistanceMiles = cimd[,as.character(base)]
  MyCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius | FIPS == baseDF$FIPS)
  MyCounties<-MyCounties %>% distinct(FIPS, .keep_all = TRUE)

  # Get Covid Case Data
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
  NewCases<-sum(rev(CovidCounties)[,1]-rev(CovidCounties)[,2])
  CummCases = colSums(CovidCounties[,5:ncol(CovidCounties)])
  currHosp = (rev(CummCases)[1] - rev(CummCases)[6])*0.055
  TotPop = sum(MyCounties$Population)

  #MyCounties<-dplyr::filter(MyCounties, FIPS %in% CovidCounties$CountyFIPS)

  NewHospitalizations<-round(NewCases*.055)
  TotalPop<-CalculateCounties(MyCounties)
  TotalCases<-CalculateCovid(MyCounties)
  CasesPer100000<-round(TotalCases/TotalPop*100000)
  CasesPer10000<-round(TotalCases/TotalPop*10000)
  CasesPer1000<-round(TotalCases/TotalPop*1000)
  HospitalizationsPer100000<-round(CasesPer100000*.055)
  HospitalizationsPer10000<-round(HospitalizationsPer100000/10)

  if (AFBaseLocations$Overseas[i] == "CONUS"){
      HospitalInfo$DistanceMiles = himd[,as.character(base)]
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
      StPopList <- dplyr::filter(CountyInfo, State == AFBaseLocations$State[i])

      # Get total hospital bed number across state
      IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == AFBaseLocations$State[i])
      TotalBedsState <- sum(IncludedHospitalsST$BEDS)
      # Calculate bed ratio
      BedProp <- TotalBedsCounty/TotalBedsState
  } else {
      if (AFBaseLocations$Country[i] == "South Korea"){
          IHME_State <- dplyr::filter(IHME_Model, location_name == "Republic of Korea")
      } else {
          IHME_State <- dplyr::filter(IHME_Model, location_name == toString(AFBaseLocations$"State Name"[i]))
          if (nrow(IHME_State)==0){
              IHME_State <- dplyr::filter(IHME_Model, location_name == toString(AFBaseLocations$County[i]))
          }
          if (nrow(IHME_State)==0) {
              IHME_State <- dplyr::filter(IHME_Model, location_name == toString(AFBaseLocations$Country[i]))
          }
      }
      StPopList <- dplyr::filter(CountyInfo, State == toString(AFBaseLocations$Country[i]))
      TotalBedsCounty = 0
  }


  RegPop <- sum(MyCounties$Population)
  StPop <- sum(StPopList$Population)

  # Use Population ratio to scale IHME
  PopRatio <- RegPop/StPop


  # Apply ratio's to IHME data
  IHME_Region <- IHME_State
  IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
  IHME_Region$confirmed_infections = round(IHME_State$est_infections_mean*PopRatio)
  IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean, IHME_Region$est_infections_mean)
  colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations","Expected Cases")
  IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date())

  IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
  IHME_Region <- dplyr::arrange(IHME_Region,ForecastDate)


  # if (nrow(CovidCounties)<nrow(MyCounties)){
  #   diff1<-setdiff(MyCounties$FIPS, CovidCounties$CountyFIPS)
  #   r<-which(MyCounties$FIPS == diff1)
  #   CovidCounties[seq(r+1,nrow(CovidCounties)+1),] <- CovidCounties[seq(r,nrow(CovidCounties)),]
  #   CovidCounties[r,] <- 0
  # }
  # if (nrow(DeathCounties)<nrow(MyCounties)){
  #   diff2<-setdiff(MyCounties$FIPS, DeathCounties$CountyFIPS)
  #   r<-which(MyCounties$FIPS == diff1)
  #   DeathCounties[seq(r+1,nrow(DeathCounties)+1),] <- DeathCounties[seq(r,nrow(DeathCounties)),]
  #   DeathCounties[r,] <- 0
  # }
  # if (nrow(CaseRate)<nrow(MyCounties)){
  #   diff3<-setdiff(MyCounties$FIPS, CaseRate$CountyFIPS)
  #   r<-which(MyCounties$FIPS == diff1)
  #   CaseRate[seq(r+1,nrow(CaseRate)+1),] <- CaseRate[seq(r,nrow(CaseRate)),]
  #   CaseRate[r,] <- 0
  # }

  ####################################################################################
  
  if (AFBaseLocations$Overseas[i] == "CONUS"){
    DaysProjected <- 120
    Army_State<-dplyr::filter(Army_Model,FIPS %in% MyCounties$FIPS)      
    Local<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State))    
    Local$Date <- as.Date(Local$ForecastDate, "%m/%d/%y")
    Local<-dplyr::filter(Local,ForecastDate >= Sys.Date())
  }

  if(nrow(IHME_Region) == 0 || TotPop == 0 || nrow(Local) == 0){

    MTFDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0)
    names(MTFDF)<-c("Installation","MAJCOM","State","50MileTotalCases","50MileNewCases","50MileCasesPer1000","50MileDblRate")

    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0)
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date",
                      "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D CAA Forecast","60D CAA Peak","60D CAA Peak Date")

    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000", "New Cases","Total Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date",
                           "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D CAA Forecast","60D CAA Peak","60D CAA Peak Date")
    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
  }else{

    if (AFBaseLocations$Overseas[i] == "CONUS"){
      DaysProjected <- 120
      Army_State<-dplyr::filter(Army_Model,FIPS %in% MyCounties$FIPS)      
      Local<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State))    
      Local$Date <- as.Date(Local$ForecastDate, "%m/%d/%y")
      Local<-dplyr::filter(Local,ForecastDate >= Sys.Date())
      Local<-aggregate(Local[,sapply(Local,is.numeric)],Local["ForecastDate"],sum)
      Local<-Local[1:DaysProjected,]
      Local<-data.frame(Local$ForecastDate,Local$Infected*.055,Local$Infected)
      colnames(Local)<-c("ForecastDate", "Expected Hospitalizations","Expected Infections")
      Local$ForecastDate<-as.Date(Local$ForecastDate)
      Local <- dplyr::arrange(Local,ForecastDate)      
    } else if (AFBaseLocations$Overseas[i] == "OCONUS") {  
      Local = dplyr::filter(CHIME_All, Base == base)
      Local = dplyr::filter(Local, ForecastDate > Sys.Date())
    }  
      
    doubling<-as.integer(CaseDblRate(MyCounties))

    ########################################################################################
    SevDayVal<-round(Local$`Expected Hospitalizations`[7])
    FourteenDayVal<-round(Local$`Expected Hospitalizations`[14])
    TwentyOneDayVal<-round(Local$`Expected Hospitalizations`[21])
    ThirtyDayVal<-round(Local$`Expected Hospitalizations`[30])
    SixtyDayVal<-round(Local$`Expected Hospitalizations`[60])
    PeakSevDayVal<-round(max(Local$`Expected Hospitalizations`[1:7]))
    PeakFourteenDayVal<-round(max(Local$`Expected Hospitalizations`[1:14]))
    PeakTwentyOneDayVal<-round(max(Local$`Expected Hospitalizations`[1:21]))
    PeakThirtyDayVal<-round(max(Local$`Expected Hospitalizations`[1:30]))
    PeakSixtyDayVal<-round(max(Local$`Expected Hospitalizations`[1:60]))
    PeakDateSevDayVal<-which.max(Local$`Expected Hospitalizations`[1:7])
    PeakDateFourteenDayVal<-which.max(Local$`Expected Hospitalizations`[1:14])
    PeakDateTwentyOneDayVal<-which.max(Local$`Expected Hospitalizations`[1:21])
    PeakDateThirtyDayVal<-which.max(Local$`Expected Hospitalizations`[1:30])
    PeakDateSixtyDayVal<-which.max(Local$`Expected Hospitalizations`[1:60])
    PeakDateSevDayVal<-format(Local$ForecastDate[PeakDateSevDayVal], format="%b-%d")
    PeakDateFourteenDayVal<-format(Local$ForecastDate[PeakDateFourteenDayVal], format="%b-%d")
    PeakDateTwentyOneDayVal<-format(Local$ForecastDate[PeakDateTwentyOneDayVal], format="%b-%d")
    PeakDateThirtyDayVal<-format(Local$ForecastDate[PeakDateThirtyDayVal], format="%b-%d")
    PeakDateSixtyDayVal<-format(Local$ForecastDate[PeakDateSixtyDayVal], format="%b-%d")
    
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
    SevDayValCases<-round(Local$`Expected Infections`[7])
    FourteenDayValCases<-round(Local$`Expected Infections`[14])
    TwentyOneDayValCases<-round(Local$`Expected Infections`[21])
    ThirtyDayValCases<-round(Local$`Expected Infections`[30])
    SixtyDayValCases<-round(Local$`Expected Infections`[60])
    PeakSevDayValCases<-round(max(Local$`Expected Infections`[1:7]))
    PeakFourteenDayValCases<-round(max(Local$`Expected Infections`[1:14]))
    PeakTwentyOneDayValCases<-round(max(Local$`Expected Infections`[1:21]))
    PeakThirtyDayValCases<-round(max(Local$`Expected Infections`[1:30]))
    PeakSixtyDayValCases<-round(max(Local$`Expected Infections`[1:60]))
    PeakDateSevDayValCases<-which.max(Local$`Expected Infections`[1:7])
    PeakDateFourteenDayValCases<-which.max(Local$`Expected Infections`[1:14])
    PeakDateTwentyOneDayValCases<-which.max(Local$`Expected Infections`[1:21])
    PeakDateThirtyDayValCases<-which.max(Local$`Expected Infections`[1:30])
    PeakDateSixtyDayValCases<-which.max(Local$`Expected Infections`[1:60])
    PeakDateSevDayValCases<-format(Local$ForecastDate[PeakDateSevDayValCases], format="%b-%d")
    PeakDateFourteenDayValCases<-format(Local$ForecastDate[PeakDateFourteenDayValCases], format="%b-%d")
    PeakDateTwentyOneDayValCases<-format(Local$ForecastDate[PeakDateTwentyOneDayValCases], format="%b-%d")
    PeakDateThirtyDayValCases<-format(Local$ForecastDate[PeakDateThirtyDayValCases], format="%b-%d")
    PeakDateSixtyDayValCases<-format(Local$ForecastDate[PeakDateSixtyDayValCases], format="%b-%d")

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

    MTFDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],TotalCases,NewCases,CasesPer1000,doubling)
    names(MTFDF)<-c("Installation","MAJCOM","State","50MileTotalCases","50MileNewCases","50MileCasesPer1000","50MileDblRate")
    MTFSummaryReport <- rbind(MTFSummaryReport,MTFDF)

    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), HospitalizationsPer100000, HospitalizationsPer10000, NewHospitalizations,
                        I1,PI1,PID1,SevDayVal,PeakSevDayVal,PeakDateSevDayVal,
                        I2,PI2,PID2,FourteenDayVal,PeakFourteenDayVal,PeakDateFourteenDayVal,
                        I3,PI3,PID3,TwentyOneDayVal,PeakTwentyOneDayVal,PeakDateTwentyOneDayVal,
                        I4,PI4,PID4,ThirtyDayVal,PeakThirtyDayVal,PeakDateThirtyDayVal,
                        I5,PI5,PID5,SixtyDayVal,PeakSixtyDayVal,PeakDateSixtyDayVal)
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000","New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date",
                      "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D CAA Forecast","60D CAA Peak","60D CAA Peak Date")

    ForecastDataTable <- rbind(ForecastDataTable,NewDF)

    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), CasesPer100000, CasesPer10000, NewCases,TotalCases,
                             I1Cases,PI1Cases,PID1Cases,SevDayValCases,PeakSevDayValCases,PeakDateSevDayValCases,
                             I2Cases,PI2Cases,PID2Cases,FourteenDayValCases,PeakFourteenDayValCases,PeakDateFourteenDayValCases,
                             I3Cases,PI3Cases,PID3Cases,TwentyOneDayValCases,PeakTwentyOneDayValCases,PeakDateTwentyOneDayValCases,
                             I4Cases,PI4Cases,PID4Cases,ThirtyDayValCases,PeakThirtyDayValCases,PeakDateThirtyDayValCases,
                             I5Cases,PI5Cases,PID5Cases,SixtyDayValCases,PeakSixtyDayValCases,PeakDateSixtyDayValCases)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000","New Cases","Total Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date",
                           "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D CAA Forecast","60D CAA Peak","60D CAA Peak Date")

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
MTFSummaryReport2 <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),c("Installation","MAJCOM","State","TotalCases","DblRate"))

ForecastDataTableOneMile <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                                                                               "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                                                                               "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                                                                               "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                                                                               "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date"))

ForecastDataTableCasesOneMile <- setNames(data.frame(matrix(ncol = 32, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Cases Per 100,000", "Cases Per 10,000", "New Cases","Total Cases",
                                                                                    "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                                                                                    "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                                                                                    "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                                                                                    "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date"))
##Repeat the above process to generate all of the same information for the single county the base is in (SG request)
for (i in 2:AFrow){
  radius = 10

  # Get Counties
  baseDF = dplyr::filter(AFBaseLocations, Base == AFBaseLocations$Base[i])
  baseDF = baseDF[1,]
  base = baseDF$Base
  #CountyInfo$DistanceMiles = cimd[,as.character(base)]
  MyCounties<-dplyr::filter(CountyInfo, County == baseDF$County)

  # Get Covid Case Data
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
  NewCases<-sum(rev(CovidCounties)[,1]-rev(CovidCounties)[,2])
  CummCases = colSums(CovidCounties[,5:ncol(CovidCounties)])
  currHosp = (rev(CummCases)[1] - rev(CummCases)[6])*0.055
  TotPop = sum(MyCounties$Population)

  MyCounties<-dplyr::filter(MyCounties, FIPS %in% CovidCounties$CountyFIPS)

  NewHospitalizations<-round(NewCases*.055)
  TotalPop<-CalculateCounties(MyCounties)
  TotalCases<-CalculateCovid(MyCounties)
  CasesPer100000<-round(TotalCases/TotalPop*100000)
  CasesPer10000<-round(TotalCases/TotalPop*10000)
  CasesPer1000<-round(TotalCases/TotalPop*1000)
  HospitalizationsPer100000<-round(CasesPer100000*.055)
  HospitalizationsPer10000<-round(HospitalizationsPer100000/10)

  if (AFBaseLocations$Overseas[i] == "CONUS"){
    HospitalInfo$DistanceMiles = himd[,as.character(base)]
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
    StPopList <- dplyr::filter(CountyInfo, State == AFBaseLocations$State[i])

    # Get total hospital bed number across state
    IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == AFBaseLocations$State[i])
    TotalBedsState <- sum(IncludedHospitalsST$BEDS)
    # Calculate bed ratio
    BedProp <- TotalBedsCounty/TotalBedsState
  } else {
    if (AFBaseLocations$Country[i] == "South Korea"){
      IHME_State <- dplyr::filter(IHME_Model, location_name == "Republic of Korea")
    } else {
      IHME_State <- dplyr::filter(IHME_Model, location_name == toString(AFBaseLocations$"State Name"[i]))
      if (nrow(IHME_State)==0){
        IHME_State <- dplyr::filter(IHME_Model, location_name == toString(AFBaseLocations$County[i]))
      }
      if (nrow(IHME_State)==0) {
        IHME_State <- dplyr::filter(IHME_Model, location_name == toString(AFBaseLocations$Country[i]))
      }
    }
    StPopList <- dplyr::filter(CountyInfo, State == toString(AFBaseLocations$Country[i]))
  }

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

  # if (nrow(CovidCounties)<nrow(MyCounties)){
  #   diff1<-setdiff(MyCounties$FIPS, CovidCounties$CountyFIPS)
  #   r<-which(MyCounties$FIPS == diff1)
  #   CovidCounties[seq(r+1,nrow(CovidCounties)+1),] <- CovidCounties[seq(r,nrow(CovidCounties)),]
  #   CovidCounties[r,] <- 0
  # }
  # if (nrow(DeathCounties)<nrow(MyCounties)){
  #   diff2<-setdiff(MyCounties$FIPS, DeathCounties$CountyFIPS)
  #   r<-which(MyCounties$FIPS == diff1)
  #   DeathCounties[seq(r+1,nrow(DeathCounties)+1),] <- DeathCounties[seq(r,nrow(DeathCounties)),]
  #   DeathCounties[r,] <- 0
  # }
  # if (nrow(CaseRate)<nrow(MyCounties)){
  #   diff3<-setdiff(MyCounties$FIPS, CaseRate$CountyFIPS)
  #   r<-which(MyCounties$FIPS == diff1)
  #   CaseRate[seq(r+1,nrow(CaseRate)+1),] <- CaseRate[seq(r,nrow(CaseRate)),]
  #   CaseRate[r,] <- 0
  # }

  ####################################################################################

  if (AFBaseLocations$Overseas[i] == "CONUS"){
    DaysProjected <- 120
    Army_State<-dplyr::filter(Army_Model,FIPS %in% MyCounties$FIPS)      
    Local<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State))    
    Local$Date <- as.Date(Local$ForecastDate, "%m/%d/%y")
    Local<-dplyr::filter(Local,ForecastDate >= Sys.Date())
  }
  
  if(nrow(IHME_Region) == 0 || TotPop == 0 || nrow(Local) == 0){
  
    MTFDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0)
    names(MTFDF)<-c("Installation","MAJCOM","State","TotalCases","DblRate")

    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0)
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date")

    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000", "New Cases","Total Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date")
    ForecastDataTableOneMile <- rbind(ForecastDataTableOneMile,NewDF)
  }else{

    if (AFBaseLocations$Overseas[i] == "CONUS"){
      DaysProjected <- 120
      Army_State<-dplyr::filter(Army_Model,FIPS %in% MyCounties$FIPS)      
      Local<-subset(Army_State, select=-c(Location,County,Susceptible,Exposed,Removed,Fatalities,State))    
      Local$Date <- as.Date(Local$ForecastDate, "%m/%d/%y")
      Local<-dplyr::filter(Local,ForecastDate >= Sys.Date())
      Local<-aggregate(Local[,sapply(Local,is.numeric)],Local["ForecastDate"],sum)
      Local<-Local[1:DaysProjected,]
      Local<-data.frame(Local$ForecastDate,Local$Infected*.055,Local$Infected)
      colnames(Local)<-c("ForecastDate", "Expected Hospitalizations","Expected Infections")
      Local$ForecastDate<-as.Date(Local$ForecastDate)
      Local <- dplyr::arrange(Local,ForecastDate)      
    } else if (AFBaseLocations$Overseas[i] == "OCONUS") {  
      Local = dplyr::filter(CHIME_All, Base == base)
      Local = dplyr::filter(Local, ForecastDate > Sys.Date())
    }  
    
    doubling<-as.integer(CaseDblRate(MyCounties))
    
    ########################################################################################
    SevDayVal<-round(Local$`Expected Hospitalizations`[7])
    FourteenDayVal<-round(Local$`Expected Hospitalizations`[14])
    ThirtyDayVal<-round(Local$`Expected Hospitalizations`[21])
    SixtyDayVal<-round(Local$`Expected Hospitalizations`[30])
    PeakSevDayVal<-round(max(Local$`Expected Hospitalizations`[1:7]))
    PeakFourteenDayVal<-round(max(Local$`Expected Hospitalizations`[1:14]))
    PeakThirtyDayVal<-round(max(Local$`Expected Hospitalizations`[1:21]))
    PeakSixtyDayVal<-round(max(Local$`Expected Hospitalizations`[1:30]))
    PeakDateSevDayVal<-which.max(Local$`Expected Hospitalizations`[1:7])
    PeakDateFourteenDayVal<-which.max(Local$`Expected Hospitalizations`[1:14])
    PeakDateThirtyDayVal<-which.max(Local$`Expected Hospitalizations`[1:21])
    PeakDateSixtyDayVal<-which.max(Local$`Expected Hospitalizations`[1:30])
    PeakDateSevDayVal<-format(Local$ForecastDate[PeakDateSevDayVal], format="%b-%d")
    PeakDateFourteenDayVal<-format(Local$ForecastDate[PeakDateFourteenDayVal], format="%b-%d")
    PeakDateThirtyDayVal<-format(Local$ForecastDate[PeakDateThirtyDayVal], format="%b-%d")
    PeakDateSixtyDayVal<-format(Local$ForecastDate[PeakDateSixtyDayVal], format="%b-%d")

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
    SevDayValCases<-round(Local$`Expected Infections`[7])
    FourteenDayValCases<-round(Local$`Expected Infections`[14])
    ThirtyDayValCases<-round(Local$`Expected Infections`[21])
    SixtyDayValCases<-round(Local$`Expected Infections`[30])
    PeakSevDayValCases<-round(max(Local$`Expected Infections`[1:7]))
    PeakFourteenDayValCases<-round(max(Local$`Expected Infections`[1:14]))
    PeakThirtyDayValCases<-round(max(Local$`Expected Infections`[1:21]))
    PeakSixtyDayValCases<-round(max(Local$`Expected Infections`[1:30]))
    PeakDateSevDayValCases<-which.max(Local$`Expected Infections`[1:7])
    PeakDateFourteenDayValCases<-which.max(Local$`Expected Infections`[1:14])
    PeakDateThirtyDayValCases<-which.max(Local$`Expected Infections`[1:21])
    PeakDateSixtyDayValCases<-which.max(Local$`Expected Infections`[1:30])
    PeakDateSevDayValCases<-format(Local$ForecastDate[PeakDateSevDayValCases], format="%b-%d")
    PeakDateFourteenDayValCases<-format(Local$ForecastDate[PeakDateFourteenDayValCases], format="%b-%d")
    PeakDateThirtyDayValCases<-format(Local$ForecastDate[PeakDateThirtyDayValCases], format="%b-%d")
    PeakDateSixtyDayValCases<-format(Local$ForecastDate[PeakDateSixtyDayValCases], format="%b-%d")

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

    MTFDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],TotalCases,doubling)
    names(MTFDF)<-c("Installation","MAJCOM","State","TotalCases","DblRate")
    MTFSummaryReport2 <- rbind(MTFSummaryReport2,MTFDF)

    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), HospitalizationsPer100000, HospitalizationsPer10000, NewHospitalizations,
                        I1,PI1,PID1,SevDayVal,PeakSevDayVal,PeakDateSevDayVal,
                        I2,PI2,PID2,FourteenDayVal,PeakFourteenDayVal,PeakDateFourteenDayVal,
                        I3,PI3,PID3,ThirtyDayVal,PeakThirtyDayVal,PeakDateThirtyDayVal,
                        I4,PI4,PID4,SixtyDayVal,PeakSixtyDayVal,PeakDateSixtyDayVal)
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000","New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date")
    ForecastDataTableOneMile <- rbind(ForecastDataTableOneMile,NewDF)

    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), CasesPer100000, CasesPer10000, NewCases,TotalCases,
                             I1Cases,PI1Cases,PID1Cases,SevDayValCases,PeakSevDayValCases,PeakDateSevDayValCases,
                             I2Cases,PI2Cases,PID2Cases,FourteenDayValCases,PeakFourteenDayValCases,PeakDateFourteenDayValCases,
                             I3Cases,PI3Cases,PID3Cases,ThirtyDayValCases,PeakThirtyDayValCases,PeakDateThirtyDayValCases,
                             I4Cases,PI4Cases,PID4Cases,SixtyDayValCases,PeakSixtyDayValCases,PeakDateSixtyDayValCases)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000","New Cases","Total Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D CAA Forecast","7D CAA Peak","7D CAA Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D CAA Forecast","14D CAA Peak","14D CAA Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D CAA Forecast","21D CAA Peak","21D CAA Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D CAA Forecast","30D CAA Peak","30D CAA Peak Date")
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


######################## Summary MTF Report
# MTFSummaryReport
# Local to Each Base
MTFSummaryReport <- merge(MTFSummaryReport2,
                          MTFSummaryReport,
                          by=c("Installation","MAJCOM","State"))

##############################################################################################################


######################## Summary Tab Heat Map
HeatMapForecast<-merge(AFBaseLocations, ForecastDataTable, by.x = c("Base","State","Major Command"), by.y = c("Installation","State","MAJCOM"))
HeatMapForecast<-data.frame(HeatMapForecast$Base,HeatMapForecast$State,HeatMapForecast$Branch,HeatMapForecast$Operational,HeatMapForecast$`Major Command`,
                            HeatMapForecast$Lat, HeatMapForecast$Long,HeatMapForecast$`Available Beds`,HeatMapForecast$`Hopitalization Per 100,000`,HeatMapForecast$`Hopitalization Per 10,000`,
                            HeatMapForecast$`New Hospitalizations`,HeatMapForecast$`New Hospitalizations`,HeatMapForecast$`7D CAA Forecast`, HeatMapForecast$`7D IHME Forecast`,
                            HeatMapForecast$`14D CAA Forecast`,HeatMapForecast$`14D IHME Forecast`,  HeatMapForecast$`21D CAA Forecast`, HeatMapForecast$`21D IHME Forecast`,
                            HeatMapForecast$`30D CAA Forecast`, HeatMapForecast$`30D IHME Forecast`,HeatMapForecast$ID)
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
                                 HeatMapForecastCases$`New Cases`,HeatMapForecastCases$`New Cases`,HeatMapForecastCases$`7D CAA Forecast`, HeatMapForecastCases$`7D IHME Forecast`,
                                 HeatMapForecastCases$`14D CAA Forecast`,  HeatMapForecastCases$`14D IHME Forecast`,  HeatMapForecastCases$`21D CAA Forecast`, HeatMapForecastCases$`21D IHME Forecast`,
                                 HeatMapForecastCases$`30D CAA Forecast`, HeatMapForecastCases$`30D IHME Forecast`,HeatMapForecastCases$ID)
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



###############################################################################################
############################### Establish Choropleth Plot for Local ###########################
###############################################################################################


choroplethObj = st_as_sf(county_df)
choroplethObj = st_transform(choroplethObj, crs = 4326)














###############################################################################################
############################### Set Map Options for Summary Tab ###############################
###############################################################################################



USlist = list(region="US",
              displayMode = "regions",
              resolution = "provinces",
              colors="['#52E74B','blue','#85050a']", #green to dark red
              width=1000,
              height = 600,
              legendPosition="bottom"
) 

NAlist = list(region="021",
              displayMode = "regions",
              colors="['#52E74B','blue','#85050a']", #green to dark red
              width=1000,
              height = 600,
              legendPosition="bottom"
)    
SAlist = list(region="005",
              displayMode = "regions",
              colors="['#52E74B','blue','#85050a']", #green to dark red
              width=1000,
              height = 600,
              legendPosition="bottom"
)    
EUROlist = list(region="150",
                displayMode = "regions",
                colors="['#52E74B','blue','#85050a']", #green to dark red
                width=1000,
                height = 600,
                legendPosition="bottom"
)
AFRICAlist = list(region="002",
                  displayMode = "regions",
                  colors="['#52E74B','blue','#85050a']", #green to dark red
                  width=1000,
                  height = 600,
                  legendPosition="bottom"
)    
ASIAlist = list(region="142",
                displayMode = "regions",
                colors="['#52E74B','blue','#85050a']", #green to dark red
                width=1000,
                height = 600,
                legendPosition="bottom"
)
OCEANIAlist = list(region="009",
                   displayMode = "province",
                   colors="['#52E74B','blue','#85050a']", #green to dark red
                   width=1000,
                   height = 600,
                   legendPosition="bottom"
)    

WORLDlist = list(region="world",
                 displayMode = "province",
                 colors="['#52E74B','blue','#85050a']", #green to dark red
                 width=1000,
                 height = 600,
                 legendPosition="bottom"
)





# Load Torch Data-----------------------------
#load(file = "Torch_Model.rda")
#Torch_Model = read_csv("C:/Users/taylo/Documents/CHADNew2/covid19/www/4_load_external_data/data_files/Torch_Model.csv")
Torch_Model = read_csv("www/4_load_external_data/data_files/Torch_Model.csv")









###############################################################################################
############################### Base Hot Spot Summary #########################################
###############################################################################################


BaseSummaryList <- setNames(data.frame(matrix(ncol = 16, nrow = 0)),
                            c("ID", "Base", "Branch", "Operational", "Major Command","Region",
                              "Population","Cases Per Capita","Total Weekly Case Change","Weekly Case Change",
                              "Total Cases", "New Weekly Cases", "New Weekly Cases (per capita)", "Deaths Per Capita", "Total Deaths", "New Weekly Deaths"))

for (i in 1:nrow(AFBaseLocations)){
  
  # Find counties in radius
  baseDF = dplyr::filter(AFBaseLocations, Base == AFBaseLocations$Base[i])
  CountyInfo$DistanceMiles = cimd[,as.character(AFBaseLocations$Base[i])]
  IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= 50 | FIPS == baseDF$FIPS)
  IncludedCounties<-IncludedCounties %>% distinct(FIPS, .keep_all = TRUE)
  PopReg = sum(IncludedCounties$Population)
  
  # Find metrics in region
  CovidCountiesCases = subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  CovidCountiesDeaths = subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
  CumCovid = colSums(CovidCountiesCases[,5:length(CovidCountiesCases)])
  CumDeaths = colSums(CovidCountiesDeaths[,5:length(CovidCountiesDeaths)])
  CummCases = rev(CumCovid)[1]
  CummDeaths = rev(CumDeaths)[1]
  weeklyCases = rev(CumCovid)[1] - rev(CumCovid)[8]
  weeklyDeaths = rev(CumDeaths)[1] - rev(CumDeaths)[8]
  CasesPerCap = round((CummCases/PopReg)*100000,0)
  DeathsPerCap = round((CummDeaths/PopReg)*100000,0)
  
  # Week over Week growth rates
  WeeklyChange = rev(CumCovid)[c(1:15)]
  WeeklyChange = (((WeeklyChange[1]-WeeklyChange[8])-(WeeklyChange[8]-WeeklyChange[15]))/(WeeklyChange[8]-WeeklyChange[15]))
  WeeklyChange[is.na(WeeklyChange)] = 0
  WeeklyChange[is.infinite(as.matrix(WeeklyChange))] = 0
  
  # Weekly Total Case Change
  WeeklyTotChange = rev(CumCovid)[c(1:15)]
  WeeklyTotChange = ((WeeklyTotChange[1]-WeeklyTotChange[8])/WeeklyTotChange[8])
  WeeklyTotChange[is.na(WeeklyTotChange)] = 0
  WeeklyTotChange[is.infinite(as.matrix(WeeklyTotChange))] = 0
  
  # Weekly case numbers per capita
  weeklyCaseCapita = round(((rev(CumCovid)[1] - rev(CumCovid)[8])/PopReg)*100000,0)
  
  # Append to list
  ListTemp = cbind(baseDF[1:5],baseDF$`State Name`,PopReg,CasesPerCap,WeeklyTotChange,WeeklyChange,CummCases,weeklyCases,weeklyCaseCapita,DeathsPerCap,CummDeaths,weeklyDeaths)
  colnames(ListTemp) = colnames(BaseSummaryList)
  BaseSummaryList = rbind(BaseSummaryList, ListTemp, make.row.names = FALSE, stringsAsFactors = FALSE)
  
}

# # Create scores using min/max scaler
# 
# # Capita
# minCap = min(BaseSummaryList$`Cases Per Capita`)
# maxCap = max(BaseSummaryList$`Cases Per Capita`)
# 
# # Weekly total change
# minTot = 0
# maxTot = max(BaseSummaryList$`Total Weekly Case Change`)
# 
# # Weekly change
# minWeek = min(BaseSummaryList$`Weekly Case Change`)
# maxWeek = max(BaseSummaryList$`Weekly Case Change`)

# # Score each metric
# BaseSummaryList$ScoreC = (BaseSummaryList$`Cases Per Capita` - minCap)/(maxCap - minCap)
# BaseSummaryList$ScoreT = (BaseSummaryList$`Total Weekly Case Change` - minTot)/(maxTot - minTot)
# BaseSummaryList$ScoreW = (BaseSummaryList$`Weekly Case Change` - minWeek)/(maxWeek - minWeek)
# 
# # Compute weight score
# BaseSummaryList$Scen1aScore = (1/3)*BaseSummaryList$ScoreC + (1/3)*BaseSummaryList$ScoreT + (1/3)*BaseSummaryList$ScoreW
# BaseSummaryList$Scen1bScore = (1/3)*(1-BaseSummaryList$ScoreC) + (1/3)*BaseSummaryList$ScoreT + (1/3)*BaseSummaryList$ScoreW
# BaseSummaryList$Scen2aScore = (1/3)*BaseSummaryList$ScoreC + (1/3)*(1-BaseSummaryList$ScoreT) + (1/3)*BaseSummaryList$ScoreW
# BaseSummaryList$Scen2bScore = (1/3)*(1-BaseSummaryList$ScoreC) + (1/3)*(1-BaseSummaryList$ScoreT) + (1/3)*BaseSummaryList$ScoreW
# BaseSummaryList$Scen3aScore = (1/3)*BaseSummaryList$ScoreC + (1/3)*BaseSummaryList$ScoreT + (1/3)*(1-BaseSummaryList$ScoreW)
# BaseSummaryList$Scen3bScore = (1/3)*(1-BaseSummaryList$ScoreC) + (1/3)*BaseSummaryList$ScoreT + (1/3)*(1-BaseSummaryList$ScoreW)