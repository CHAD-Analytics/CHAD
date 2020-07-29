#CovidConfirmedCases = vroom::vroom("www/4_load_external_data/data_files/time_series_covid19_confirmed_US.csv")
CovidConfirmedCases = as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))

CovidConfirmedCases = CovidConfirmedCases[colSums(!is.na(CovidConfirmedCases)) > 0]

CovidConfirmedCases = CovidConfirmedCases[,c(5, 12:ncol(CovidConfirmedCases))]
colnames(CovidConfirmedCases)[1] = "CountyFIPS"

CovidConfirmedCases = merge(StateInfo,CovidConfirmedCases,by = "CountyFIPS")

CovidConfirmedCases[is.na(CovidConfirmedCases)] <- 0

# #Calculate county case doubling rate for most recent day
# CovidConfirmedCases <- dplyr::filter(CovidConfirmedCases, CountyFIPS != 0)
# CovidConfirmedCases <- head(CovidConfirmedCases,-1)


#Get rid of days with incorrect cumulative reporting of zeros after reported cases have been seen
for(i in 6:(ncol(CovidConfirmedCases))){
  
  CovidConfirmedCases[,i] = ifelse(CovidConfirmedCases[,i] < CovidConfirmedCases[,(i-1)],
                                   CovidConfirmedCases[,(i-1)],
                                   CovidConfirmedCases[,i])
}

colnames(CovidConfirmedCases) = c(colnames(CovidConfirmedCases[1:4]), 
                                  format.Date(lubridate::mdy(colnames(CovidConfirmedCases[5:ncol(CovidConfirmedCases)])),
                                              "%m/%d/%y"))

##########################################################################################

# GlobalData = read.csv("www/4_load_external_data/data_files/data.csv")
# #GlobalData = vroom::vroom("www/4_load_external_data/data_files/data.csv")
# GlobalData = replace(GlobalData, GlobalData == "", NA)



#Split cases and deaths into separate data frames. Then convert from long to wide, then order it by country.
#We need to remove first entry of any duplicate, this way we do not double count any countries that are split by region as well.
GlobalCases<- GlobalData[,-c(8,10)]
GlobalCases = GlobalCases[!duplicated(GlobalCases[c(1,2,3)]),]
GlobalCases <- spread(GlobalCases, Date, Confirmed)
GlobalCases<-GlobalCases %>% arrange(GlobalCases$CountryName)
#GlobalCases<-GlobalCases %>% group_by(CountryName) %>% filter(duplicated(CountryName) | n()==1)
GlobalCases<- filter(GlobalCases, !(CountryName %in% "United States of America"))

GlobalCases = inner_join(GlobalCases,CountyInfo, by = "Key")

GlobalInfo = data.frame(GlobalCases$FIPS,GlobalCases$County,GlobalCases$State,GlobalCases$Key)
GlobalCases = cbind(GlobalInfo,GlobalCases[,33:(ncol(GlobalCases)-ncol(CountyInfo)+1)])


#GlobalCases<-GlobalCases[,c(1,6,4,3,33:ncol(GlobalCases))] #Need the correct number of days now, otherwise rbind wont line up
GlobalCases[,5][is.na(GlobalCases[,5])] <- 0
GlobalCases<- data.frame(t(apply(GlobalCases[,], 1, zoo::na.locf)))
colnames(GlobalCases) = c(colnames(CovidConfirmedCases[1:4]),
                          format.Date(sub('.',
                                          '',
                                          gsub("\\.",
                                               "/",
                                               names(GlobalCases[5:ncol(GlobalCases)]))),
                                      "%m/%d/%y"))

combNames = c(colnames(GlobalCases),colnames(CovidConfirmedCases))
dupNames = combNames[duplicated(combNames)]

GlobalCases = GlobalCases %>% select(all_of(dupNames))
CovidConfirmedCases = CovidConfirmedCases %>% select(all_of(dupNames))
CovidConfirmedCases<-rbind(CovidConfirmedCases,GlobalCases)

cols<-names(CovidConfirmedCases[5:length(CovidConfirmedCases)])
CovidConfirmedCases[,cols]<-lapply(CovidConfirmedCases[cols], as.numeric)

CovidConfirmedCases$CountyFIPS = as.numeric(CovidConfirmedCases$CountyFIPS)


tempTbl = left_join(CovidConfirmedCases, CountyInfo, by = c("CountyFIPS" = "FIPS")) 

labelsDF = c(colnames(CovidConfirmedCases)[1:4], "Continent", "Country", colnames(CovidConfirmedCases)[5:ncol(CovidConfirmedCases)])
ContinentMap = data.frame(tempTbl[1:4], tempTbl$Continent, tempTbl$Country, tempTbl[5:ncol(CovidConfirmedCases)])
names(ContinentMap) = labelsDF
