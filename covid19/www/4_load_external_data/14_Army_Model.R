#Army_Model <- vroom::vroom("C:/Users/taylo/Documents/CHADNew/covid19/www/4_load_external_data/data_files/para_R_abridged_2020-05-20_UnitedStates.csv")
Army_Model = vroom::vroom("www/4_load_external_data/data_files/para_R_abridged_2020-05-25_UnitedStates.csv")
colset<-c(2,4,5,6,8,9,10,11,12)
Army_Model<-Army_Model[, names(Army_Model)[colset]]  
colnames(Army_Model)<-c("ForecastDate","FIPS","County","Location","Susceptible","Exposed","Infected","Removed","Fatalities")
Army_Model <- merge(Army_Model,
                    StateList,  # defined in 1_StateInfo.R
                    by.x = names(Army_Model[4]),
                    by.y = names(StateList)[1])

names(Army_Model)[names(Army_Model)=="state.abb"] <- "State"

Army_Model$number <- str_count(Army_Model$FIPS, "/")
Army_Model$number <- Army_Model$number+1
Army_Model$Infected <- Army_Model$Infected/Army_Model$number
Army_Model$Fatalities <- Army_Model$Fatalities/Army_Model$number
Army_Model<-separate_rows(Army_Model,FIPS,Infected,convert = TRUE)
Army_Model$County<-as.character(Army_Model$County)
Army_Model$Location<-as.character(Army_Model$Location)
Army_Model$State<-as.character(Army_Model$State)

Army_Model <- merge(Army_Model,
                    CountyInfo,
                    by.x = names(Army_Model[3]),
                    by.y = names(CountyInfo)[3])

colset<-c(1,2,3,5,6,7,8,9,13,14)
Army_Model<-Army_Model[, names(Army_Model)[colset]]  
colnames(Army_Model)<-c("FIPS","Location","ForecastDate","Susceptible","Exposed","Infected","Removed","Fatalities","State","County")
Army_Model$FIPS <- as.numeric(Army_Model$FIPS)



