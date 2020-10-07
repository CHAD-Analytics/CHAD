### Have to rerun with new Army files

# Army_Model = read_csv("C:/Users/taylo/Documents/CHADNew6/covid19/www/4_load_external_data/data_files/para_R_abridged.csv")
# #Army_Model = read_csv("www/4_load_external_data/data_files/para_R_abridged.csv")
# colset<-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
# Army_Model<-Army_Model[, names(Army_Model)[colset]]
# colnames(Army_Model)<-c("ForecastDate","FIPS","County","StateFull"                                      #(1-4)
#                         ,"Susceptible","Exposed","Infected","Removed","Fatalities","TFatalities"        #(5-10)
#                         ,"LSusceptible","LExposed","LInfected","LRemoved","LFatalities","LTFatalities"  #(11-16)
#                         ,"USusceptible","UExposed","UInfected","URemoved","UFatalities","UTFatalities") #(17-22)
# save(Army_Model, file = "Army_Model.rda")
# ncol(Army_Model)

ncol(Army_Model)
colset<-c(1,2,3,4,7,9,10,13,15,16,19,21,22)

ncol(Army_Model)
Army_Model<-Army_Model[, names(Army_Model)[colset]]
colnames(Army_Model)<-c("ForecastDate","FIPS","County","StateFull"                                       
                        ,"Infected","Fatalities","TFatalities"      
                        ,"LInfected","LFatalities","LTFatalities"   
                        ,"UInfected","UFatalities","UTFatalities") 

Army_Model <- merge(Army_Model,
                     CountyInfo,
                     by.x = names(Army_Model[2]),
                     by.y = names(CountyInfo)[3])

Army_Model<-subset(Army_Model, select= -c(14,16,17,18,19,20,21,22,23,24))
colnames(Army_Model)<-c("FIPS","ForecastDate","County","StateFull","Infected","Fatalities","TFatalities"
                        ,"LInfected","LFatalities","LTFatalities","UInfected","UFatalities","UTFatalities","State")
Army_Model$FIPS <- as.numeric(Army_Model$FIPS)
Army_Model$ForecastDate <- as.Date(Army_Model$ForecastDate, "%m/%d/%Y")
#save(Army_Model, file = "Army_Model.rda")

