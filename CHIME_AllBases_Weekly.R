# Generate CHIME Outputs for all bases at 50 and 5 mile radius

CHIME_All_1mile <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),c("Base", "ForecastDate", "Expected Hospitalizations", "Expected Infections"))
radius = 5

for (i in 1:nrow(AFBaseLocations)){
  
  # Get Counties
  baseDF = dplyr::filter(AFBaseLocations, Base == AFBaseLocations$Base[i])
  baseDF = baseDF[1,]
  base = baseDF$Base
  CountyInfo$DistanceMiles = cimd[,as.character(base)]
  IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius | FIPS == baseDF$FIPS)
  
  # Get Covid Case Data
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  TotCases = colSums(CovidCounties[,5:ncol(CovidCounties)])
  currHosp = (rev(TotCases)[1] - rev(TotCases)[6])*0.055
  TotPop = sum(IncludedCounties$Population)
  
  # Get doubling rate
  currTot = rev(TotCases)[1]
  
  if (currTot != 0){
    j = 0
    
    while (currTot/2 < TotCases[length(TotCases)-j])
    {
      j = j + 1
    }
    dblDays = j
    
  }else{
    dblDays = 0
  }

  if (dblDays == 0){dblDays = 40}
  
  
  ####################################################################################
  #Mean Estimate
  #Established Variables at the start for every county or populations

  

  Ro<-Estimate_Rt(IncludedCounties)
  if (Ro == "Undefined for Region"){
    Ro<-as.integer(1)
  } else if (Ro < 1){
    Ro<-as.integer(1)
  }

  incubationtime<-5
  latenttime<-2
  recoverydays<-14
  hospitalizationrate<-5.5
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-5
  icutime<-4
  ventilatortime<-7
  socialdistancing<-15
  daysforecasted<-120

  
  
  #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
  #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
  SEIARProj<-SEIAR_Model_Run(currHosp, TotPop, incubationtime, latenttime,dblDays,recoverydays,
                             socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                             ventilatortime,daysforecasted,Ro, .5)

  MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
  CHIME_data<-data.frame(base, MyDates, SEIARProj$sir$hos_add, SEIARProj$sir$Id)
  colnames(CHIME_data)<-c("Base", "ForecastDate", "Expected Hospitalizations", "Expected Infections")

  CHIME_data$`Expected Hospitalizations` <- round(CHIME_data$`Expected Hospitalizations`,0)
  CHIME_data$`Expected Infections` <- round(CHIME_data$`Expected Infections`,0)
  CHIME_data<-CHIME_data[-1,]
  
  CHIME_All_1mile = rbind(CHIME_All,CHIME_data)
  
}

save(CHIME_All_1mile,file = "CHIME_All_1mile.rda")
