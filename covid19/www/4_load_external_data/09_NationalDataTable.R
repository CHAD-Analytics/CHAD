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
NationalDataTable = NationalDataTable[,c(3,5,6,7:ncol(NationalDataTable))]
NationalDataTable = aggregate(.~State+Continent+Country, 
                              NationalDataTable, 
                              sum)

# combNames = c(as.character(StatePop$State),as.character(NationalDataTable$State))
# dupNames = combNames[duplicated(combNames)]

#NationalDataTable = NationalDataTable %>% filter(State %in% dupNames)
#StatePop = StatePop %>% filter(State %in% dupNames)

# Average Daily case change (over 1 week)
RateofCovidChange = rev(NationalDataTable)[c(1:8)]
RateofCovidChange = ceiling(rowSums(RateofCovidChange[1:7]-RateofCovidChange[2:8])/7)

# Weekly Case Change
WeeklyChange = rev(NationalDataTable)[c(1:15)]
WeeklyChange = (((WeeklyChange[1]-WeeklyChange[8])-(WeeklyChange[8]-WeeklyChange[15]))/(WeeklyChange[8]-WeeklyChange[15]))
WeeklyChange[is.na(WeeklyChange)] = 0
WeeklyChange[is.infinite(as.matrix(WeeklyChange))] = 0

# Weekly Total Case Change
WeeklyTotChange = rev(NationalDataTable)[c(1:15)]
WeeklyTotChange = ((WeeklyTotChange[1]-WeeklyTotChange[8])/WeeklyTotChange[8])
WeeklyTotChange[is.na(WeeklyTotChange)] = 0
WeeklyTotChange[is.infinite(as.matrix(WeeklyTotChange))] = 0

##########

NationalDeathTable = ContinentMapd
#NationalDeathTable$State = as.factor(NationalDeathTable$State)
NationalDeathTable = NationalDeathTable[,c(3,5,6,7:ncol(NationalDeathTable))]
NationalDeathTable = aggregate(.~State+Continent+Country, 
                               NationalDeathTable, 
                               sum)

#NationalDeathTable = NationalDeathTable %>% filter(State %in% dupNames)

RateofDeathChange  = rev(NationalDeathTable)[c(1:8)]
RateofDeathChange  = ceiling(rowSums(RateofDeathChange[1:7]-RateofDeathChange[2:8])/7)


###########

NationalDataTable = data.frame(NationalDataTable$Continent,
                               NationalDataTable$State,
                               NationalDataTable$Country,
                               NationalDataTable[,length(NationalDataTable)],
                               RateofCovidChange, 
                               NationalDeathTable[,length(NationalDeathTable)], 
                               RateofDeathChange,
                               WeeklyTotChange,
                               WeeklyChange,
                               stringsAsFactors = FALSE)

colnames(NationalDataTable) = c("Continent",
                                "State",
                                "Country",
                                "Total Cases",
                                "Average New Cases Per Day", 
                                "Total Deaths",
                                "Average New Deaths Per Day",
                                "Weekly Total Case Change",
                                "Weekly Case Change")

NationalDataTable = NationalDataTable[order(NationalDataTable$State),]

NationalDataTable<-merge(NationalDataTable, StatePop, by.x = "State", by.y = "State")
#NationalDataTable$`Cases Per 100,000 People` = as.numeric(StatePop$Population)

NationalDataTable$`Cases Per 100,000 People` = round((NationalDataTable$`Total Cases`/NationalDataTable$Population)*100000)
#NationalDataTable<-subset(NationalDataTable, select=-c(Population))   


# Add a row for cumulative US Data
UStemp = dplyr::filter(ContinentMap, Country == "United States")
UStemp = plyr::ddply(UStemp, "Country", numcolwise(sum))
UStempd = dplyr::filter(ContinentMapd, Country == "United States")
UStempd = plyr::ddply(UStempd, "Country", numcolwise(sum))

avgCase = ceiling(rowSums(rev(UStemp)[1:7]-rev(UStemp)[2:8])/7)
avgDeath = ceiling(rowSums(rev(UStempd)[1:7]-rev(UStempd)[2:8])/7)

WeeklyChangeUS = rev(UStemp)[c(1:15)]
WeeklyChangeUS = (((WeeklyChangeUS[1]-WeeklyChangeUS[8])-(WeeklyChangeUS[8]-WeeklyChangeUS[15]))/(WeeklyChangeUS[8]-WeeklyChangeUS[15]))
WeeklyChangeUS[is.na(WeeklyChangeUS)] = 0
WeeklyChangeUS[is.infinite(as.matrix(WeeklyChangeUS))] = 0

WeeklyTotChangeUS = rev(UStemp)[c(1:15)]
WeeklyTotChangeUS = ((WeeklyTotChangeUS[1]-WeeklyTotChangeUS[8])/WeeklyTotChangeUS[8])
WeeklyTotChangeUS[is.na(WeeklyTotChangeUS)] = 0
WeeklyTotChangeUS[is.infinite(as.matrix(WeeklyTotChangeUS))] = 0

popUS = CountyInfo %>% dplyr::filter(Country == "United States")
popUS = sum(popUS$Population)
perCapitaUS = round((rev(UStemp)[1]/popUS)*100000)

USData = dplyr::filter(NationalDataTable, Country == "United States")
USData <- plyr::ddply(USData, "Country", numcolwise(sum))
USData = data.frame("United States", 
                    "North America", 
                    "United States", 
                    USData$`Total Cases`, 
                    avgCase, 
                    USData$`Total Deaths`, 
                    avgDeath,
                    WeeklyTotChangeUS,
                    WeeklyChangeUS,
                    popUS,
                    perCapitaUS)

names(USData) = names(NationalDataTable)

NationalDataTable = rbind(NationalDataTable, USData)
