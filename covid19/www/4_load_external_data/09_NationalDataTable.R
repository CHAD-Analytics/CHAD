#' Create National Data table on summary page
#' 

StatePop = CountyInfo
StatePop$State = as.factor(StatePop$State)
StatePop = StatePop[,c("State","Population")]
StatePop = aggregate(.~State,
                     StatePop,
                     sum)
StatePop = StatePop[order(StatePop$State),]

NationalDataTable = ContinentMap
#NationalDataTable$State = as.factor(NationalDataTable$State)
NationalDataTable = NationalDataTable[,-c(1,2,4)]
NationalDataTable = NationalDataTable[,c(2,1,3:ncol(NationalDataTable))]
NationalDataTable = aggregate(.~State+Continent, 
                              NationalDataTable, 
                              sum)

# combNames = c(as.character(StatePop$State),as.character(NationalDataTable$State))
# dupNames = combNames[duplicated(combNames)]

#NationalDataTable = NationalDataTable %>% filter(State %in% dupNames)
#StatePop = StatePop %>% filter(State %in% dupNames)

RateofCovidChange = rev(NationalDataTable)[c(1:8)]
RateofCovidChange = ceiling(rowSums(RateofCovidChange[1:7]-RateofCovidChange[2:8])/7)

##########

NationalDeathTable = ContinentMapd
#NationalDeathTable$State = as.factor(NationalDeathTable$State)
NationalDeathTable = NationalDeathTable[,-c(1,2,4)]
NationalDeathTable = NationalDeathTable[,c(2,1,3:ncol(NationalDeathTable))]
NationalDeathTable = aggregate(.~State+Continent, 
                               NationalDeathTable, 
                               sum)

#NationalDeathTable = NationalDeathTable %>% filter(State %in% dupNames)

RateofDeathChange  = rev(NationalDeathTable)[c(1:8)]
RateofDeathChange  = ceiling(rowSums(RateofDeathChange[1:7]-RateofDeathChange[2:8])/7)


###########

NationalDataTable = data.frame(NationalDataTable$Continent,
                               NationalDataTable$State,
                               NationalDataTable[,length(NationalDataTable)],
                               RateofCovidChange, 
                               NationalDeathTable[,length(NationalDeathTable)], 
                               RateofDeathChange)

colnames(NationalDataTable) = c("Continent",
                                "State",
                                "Total Cases",
                                "Average New Cases Per Day", 
                                "Total Deaths",
                                "Average New Deaths Per Day")

NationalDataTable = NationalDataTable[order(NationalDataTable$State),]

NationalDataTable<-merge(NationalDataTable, StatePop, by.x = "State", by.y = "State")
#NationalDataTable$`Cases Per 100,000 People` = as.numeric(StatePop$Population)

NationalDataTable$`Cases Per 100,000 People` = round((NationalDataTable$`Total Cases`/NationalDataTable$Population)*100000)
NationalDataTable<-subset(NationalDataTable, select=-c(Population))   


