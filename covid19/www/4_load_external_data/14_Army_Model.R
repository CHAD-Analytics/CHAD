# Army_Model <- vroom::vroom("C:/Users/taylo/Documents/chadgitlabNEW/covid19/www/4_load_external_data/data_files/para_R_abridged.csv")
# #Army_Model = vroom::vroom("www/4_load_external_data/data_files/para_R_abridged_UnitedStates.csv")
# 
# ### Have to rerun with new Army files
# 
# 
# Army_Model = read_csv("C:/Users/taylo/Documents/CHADNew/covid19/www/4_load_external_data/data_files/para_R_abridged_UnitedStates.csv")
# colset<-c(2,4,6,8,9,10,11,12,13,25,26,27,28,29,30,32,33,34,35,36,37)
# Army_Model<-Army_Model[, names(Army_Model)[colset]]
# colnames(Army_Model)<-c("ForecastDate","FIPS","Location","Susceptible","Exposed","Infected","Removed","Fatalities","TFatalities"
#                                                      ,"LSusceptible","LExposed","LInfected","LRemoved","LFatalities","LTFatalities"
#                                                      ,"USusceptible","UExposed","UInfected","URemoved","UFatalities","UTFatalities")
# 
# Army_Model <- merge(Army_Model,
#                      CountyInfo,
#                      by.x = names(Army_Model[2]),
#                      by.y = names(CountyInfo)[3])
# 
# Army_Model<-subset(Army_Model, select=-c(22,24,25,26,27,28,29,30,31,32))
# Army_Model$FIPS <- as.numeric(Army_Model$FIPS)
# save(Army_Model, file = "Army_Model.rda")