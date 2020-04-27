CovidConfirmedCases = vroom::vroom("www/3_load_external_data/data_files/time_series_covid19_confirmed_US.csv")

CovidConfirmedCases = CovidConfirmedCases[colSums(!is.na(CovidConfirmedCases)) > 0]

CovidConfirmedCases = CovidConfirmedCases[,c(5, 12:ncol(CovidConfirmedCases))]
colnames(CovidConfirmedCases)[1] = "CountyFIPS"

CovidConfirmedCases = merge(StateInfo,CovidConfirmedCases,by = "CountyFIPS")

CovidConfirmedCases[is.na(CovidConfirmedCases)] <- 0

colnames(CovidConfirmedCases)[1] <- "CountyFIPS"

#Calculate county case doubling rate for most recent day
CovidConfirmedCases <- dplyr::filter(CovidConfirmedCases, CountyFIPS != 0)
CovidConfirmedCases <- head(CovidConfirmedCases,-1)

#Get rid of days with incorrect cumulative reporting of zeros after reported cases have been seen
for(i in 6:(ncol(CovidConfirmedCases))){
  
    CovidConfirmedCases[,i] = ifelse(CovidConfirmedCases[,i] < CovidConfirmedCases[,(i-1)],
                                     CovidConfirmedCases[,(i-1)],
                                     CovidConfirmedCases[,i])
  
}


