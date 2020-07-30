### Have to rerun with new Army files

# Army_Model = read_csv("C:/Users/taylo/Documents/CHADNew6/covid19/www/4_load_external_data/data_files/para_R_abridged.csv")
# #Army_Model = read_csv("www/4_load_external_data/data_files/para_R_abridged.csv")
# colset<-c(2,3,4,5,6,7,8,9,10,11,17,18,19,20,21,22,23,24,25,26,27,28)
# Army_Model<-Army_Model[, names(Army_Model)[colset]]
# colnames(Army_Model)<-c("ForecastDate","FIPS","County","StateFull","Susceptible","Exposed","Infected","Removed","Fatalities","TFatalities"
#                       ,"LSusceptible","LExposed","LInfected","LRemoved","LFatalities","LTFatalities"
#                       ,"USusceptible","UExposed","UInfected","URemoved","UFatalities","UTFatalities")
# save(Army_Model, file = "Army_Model.rda")

Army_Model <- merge(Army_Model,
                     CountyInfo,
                     by.x = names(Army_Model[3]),
                     by.y = names(CountyInfo)[3])

# Army_Model<-subset(Army_Model, select=-c(22,24,25,26,27,28,29,30,31,32))

Army_Model<-subset(Army_Model, select= -c(15,16,17,18,19,20,21,22,23,24,25))
colnames(Army_Model)<-c("FIPS","StateFull","ForecastDate","County","Infected","Fatalities","TFatalities"
                        ,"LInfected","LFatalities","LTFatalities","UInfected","UFatalities","UTFatalities","State")
Army_Model$FIPS <- as.numeric(Army_Model$FIPS)
Army_Model$ForecastDate <- as.Date(Army_Model$ForecastDate, "%m/%d/%Y")
#save(Army_Model, file = "Army_Model.rda")

