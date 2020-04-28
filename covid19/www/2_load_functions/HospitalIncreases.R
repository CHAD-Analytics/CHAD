HospitalIncreases<-function(IncludedCounties){
    
  #Find hospitals in selected region
  hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
  
  #Calculate total beds and weighted average utilization
  TotalBeds<-sum(hospCounty$num_staffed_beds)
  hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
  totalUsedBeds <- sum(hospCounty$bedsUsed)
  baseUtlz <- totalUsedBeds/TotalBeds
  
  #Get COVID cases and county demographic hospitalization rates
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
  
  #Estimate current hospital utilization
  TotalHospital<-sum(CovidCounties[,length(CovidCounties)]*CovidCountiesHospRate$HospRate)
  NotHospital<-sum(CovidCounties[,(length(CovidCounties)-5)]*CovidCountiesHospRate$HospRate)
  StillHospital<-ceiling((TotalHospital-NotHospital))
  Utilz<- round(((StillHospital)/TotalBeds+baseUtlz)*100,0)
    
  paste(Utilz," %", sep = "") 
}
