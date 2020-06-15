#CovidDeaths = vroom::vroom("www/4_load_external_data/data_files/time_series_covid19_deaths_US.csv")
CovidDeaths = as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))

CovidDeaths = CovidDeaths[,c(5, 13:ncol(CovidDeaths))]
colnames(CovidDeaths)[1] = "CountyFIPS"

CovidDeaths = merge(StateInfo,CovidDeaths,by = "CountyFIPS")

CovidDeaths[is.na(CovidDeaths)] <- 0


for(i in 6:(ncol(CovidDeaths))){
  
  CovidDeaths[,i] = ifelse(CovidDeaths[,i] < CovidDeaths[,(i-1)],
                           CovidDeaths[,(i-1)],
                           CovidDeaths[,i])
}

colnames(CovidDeaths) = c(colnames(CovidDeaths[1:4]), 
                          format.Date(lubridate::mdy(colnames(CovidDeaths[5:ncol(CovidDeaths)])),
                                      "%m/%d/%y"))




##########################################################################################

# GlobalData = read.csv("www/4_load_external_data/data_files/data.csv")
# #GlobalData = vroom::vroom("www/4_load_external_data/data_files/data.csv")
# GlobalData = replace(GlobalData, GlobalData == "", NA)


# #Split cases and deaths into separate data frames. Then convert from long to wide, then order it by country.
# #We need to remove first entry of any duplicate, this way we do not double count any countries that are split by region as well.
GlobalDeaths<-GlobalData[,-7]
GlobalDeaths = GlobalDeaths[!duplicated(GlobalDeaths[c(1,2,3)]),]
GlobalDeaths <- spread(GlobalDeaths, Date, Deaths)
GlobalDeaths<-GlobalDeaths%>% arrange(GlobalDeaths$CountryName)
#GlobalDeaths<-GlobalDeaths %>% group_by(CountryName) %>% filter(duplicated(CountryName) | n()==1)
GlobalDeaths<- filter(GlobalDeaths, !(CountryName %in% "United States of America"))

GlobalDeaths = inner_join(GlobalDeaths,CountyInfo, by = "Key")

GlobalInfo = data.frame(GlobalDeaths$FIPS,GlobalDeaths$County,GlobalDeaths$State,GlobalDeaths$Key)
GlobalDeaths = cbind(GlobalInfo,GlobalDeaths[,33:(ncol(GlobalDeaths)-ncol(CountyInfo)+1)])

GlobalDeaths[,5][is.na(GlobalDeaths[,5])] <- 0
GlobalDeaths<- data.frame(t(apply(GlobalDeaths[,], 1, zoo::na.locf)))

colnames(GlobalDeaths) = c(colnames(CovidDeaths[1:4]),
                           format.Date(sub('.',
                                           '',
                                           gsub("\\.",
                                                "/",
                                                names(GlobalDeaths[5:ncol(GlobalDeaths)]))),
                                       "%m/%d/%y"))

combNames = c(colnames(GlobalDeaths),colnames(CovidDeaths))
dupNames = combNames[duplicated(combNames)]

GlobalDeaths = GlobalDeaths %>% select(dupNames)
CovidDeaths = CovidDeaths %>% select(dupNames)
CovidDeaths<-rbind(CovidDeaths,GlobalDeaths)

cols<-names(CovidDeaths[5:length(CovidDeaths)])
CovidDeaths[,cols]<-lapply(CovidDeaths[cols], as.numeric)

CovidDeaths$CountyFIPS = as.numeric(CovidDeaths$CountyFIPS)


tempTbld = left_join(CovidDeaths, CountyInfo, by = c("CountyFIPS" = "FIPS")) 

labelsDFd = c(colnames(CovidDeaths)[1:4], "Continent", "Country", colnames(CovidDeaths)[5:ncol(CovidDeaths)])
ContinentMapd = data.frame(tempTbld[1:4], tempTbld$Continent, tempTbld$Country, tempTbld[5:ncol(CovidDeaths)])
names(ContinentMapd) = labelsDFd
