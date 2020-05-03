#' Create National Data table on summary page
#' 

StatePop = CountyInfo
StatePop$State = as.factor(StatePop$State)
StatePop = StatePop[,c("State","Population")]
StatePop = aggregate(.~State,
                     StatePop,
                     sum)

NationalDataTable = CovidConfirmedCases
NationalDataTable$State = as.factor(NationalDataTable$State)
NationalDataTable = NationalDataTable[,-c(1,2,4)]
NationalDataTable = aggregate(.~State, 
                              NationalDataTable, 
                              sum)

combNames = c(as.character(StatePop$State),as.character(NationalDataTable$State))
dupNames = combNames[duplicated(combNames)]

NationalDataTable = NationalDataTable %>% filter(State %in% dupNames)
StatePop = StatePop %>% filter(State %in% dupNames)

RateofCovidChange = rev(NationalDataTable)[c(1:7)]
RateofCovidChange = ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)

NationalDeathTable = CovidDeaths
NationalDeathTable$State = as.factor(NationalDeathTable$State)
NationalDeathTable = NationalDeathTable[,-c(1,2,4)]

NationalDeathTable = aggregate(.~State, 
                               NationalDeathTable, 
                               sum)

NationalDeathTable = NationalDeathTable %>% filter(State %in% dupNames)

RateofDeathChange  = rev(NationalDeathTable)[c(1:7)]
RateofDeathChange  = ceiling(rowSums(RateofDeathChange[1:6]-RateofDeathChange[2:7])/6)


NationalDataTable = data.frame(NationalDataTable$State,
                               NationalDataTable[,length(NationalDataTable)],
                               RateofCovidChange, 
                               NationalDeathTable[,length(NationalDeathTable)], 
                               RateofDeathChange)

colnames(NationalDataTable) = c("State",
                                "Total Cases",
                                "Average New Cases Per Day", 
                                "Total Deaths",
                                "Average New Deaths Per Day")


NationalDataTable$`Cases Per 100,000 People` = as.numeric(StatePop$Population)

NationalDataTable$`Cases Per 100,000 People` = round(NationalDataTable$`Total Cases`/(NationalDataTable$`Cases Per 100,000 People`/100000))
