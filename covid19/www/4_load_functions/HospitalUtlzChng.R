#' @export
HospitalUtlzChng <- function(IncludedCounties){

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
    TotalHospital<-sum(rev(CovidCounties)[,1]*CovidCountiesHospRate$HospRate)
    NotHospital<-sum(rev(CovidCounties)[,7]**CovidCountiesHospRate$HospRate)
    StillHospital<-ceiling((TotalHospital-NotHospital))
    Utilz<- round((baseUtlz - (StillHospital)/TotalBedsbaseUtlz)*100,0)

    # # Yesterday's utilization
    # TotalHospitaly<-sum(rev(CovidCounties)[,2]*CovidCountiesHospRate$HospRate)
    # NotHospitaly<-sum(rev(CovidCounties)[,8]*CovidCountiesHospRate$HospRate)
    # StillHospitaly<-ceiling((TotalHospitaly-NotHospitaly))
    # Utilzy<-(signif(((StillHospitaly)/TotalBeds+baseUtlz)*100,3))
    #
    # # find change
    # chng <- round((Utilz-Utilzy)/2, 1)

    # if (chng < 0) {
    #   sign <- ""
    # } else {
    #   sign <- "+"
    # }
    #
    paste(Utilz,"%")
}
