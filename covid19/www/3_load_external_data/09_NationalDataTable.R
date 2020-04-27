#' Create National Data table on summary page
#' 
NationalDataTable = CovidConfirmedCases
NationalDataTable$State = as.factor(NationalDataTable$State)
NationalDataTable = NationalDataTable[,-c(1,2,4)]
NationalDataTable = aggregate(.~State, NationalDataTable, sum)
RateofCovidChange = rev(NationalDataTable)[c(1:7)]
RateofCovidChange = ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)

NationalDeathTable = CovidDeaths
NationalDeathTable$State = as.factor(NationalDeathTable$State)
NationalDeathTable = NationalDeathTable[,-c(1,2,4)]

NationalDeathTable = aggregate(.~State, 
                               NationalDeathTable, 
                               sum)

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

NationalDataTable$`Cases Per 100,000 People` = c(731545,4903185,3017825,
                                                 7278717,39512223,5758736,
                                                 3565287,705749,973764,
                                                 21477737,10617423,1415872,
                                                 3155070,1787065,12671821,
                                                 6732219,2913314,4467673,
                                                 4648794,6949503,6045680,
                                                 1344212,9986857,5639632,
                                                 6137428,2976149,1068778,
                                                 10488084,762062,1934408,
                                                 1359711,8882190,2096829,
                                                 3080156,19453561,11689100,
                                                 3956971,4217737,12801989,
                                                 1059361,5148714,884659,
                                                 6833174,28995881,3205958,
                                                 8535519,623989,7614893,
                                                 5822434,1792147,578759)

NationalDataTable$`Cases Per 100,000 People` = round(NationalDataTable$`Total Cases`/(NationalDataTable$`Cases Per 100,000 People`/100000))
