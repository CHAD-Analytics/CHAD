#DP1 <- vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_By_County.csv")
DP1 = vroom::vroom("www/4_load_external_data/data_files/Current_Response_Daily_Confirmed_Cases_By_County.csv")
DP1 <- DP1 %>% select(-c(State)) %>%
  reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Expected Hospitalizations")
   
#DP2 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP2 = vroom::vroom("www/4_load_external_data/data_files/Current_Response_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP2 <- DP2 %>% select(-c(State)) %>%
  reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Lower Estimate")

#DP3 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP3 = vroom::vroom("www/4_load_external_data/data_files/Current_Response_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP3 <- DP3 %>% select(-c(State)) %>%
  reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Upper Estimate")

DP1<- left_join(DP1,DP2)
remove(DP2)
DP1<- left_join(DP1,DP3)
remove(DP3)

DP1 <- as.data.table(DP1)

#DP4 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_By_County.csv")
DP4 = vroom::vroom("www/4_load_external_data/data_files/Improved_Response_Daily_Confirmed_Cases_By_County.csv")
DP4 <- DP4 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Expected Hospitalizations")

#DP5 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP5 = vroom::vroom("www/4_load_external_data/data_files/Improved_Response_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP5 <- DP5 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Lower Estimate")

#DP6 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP6 = vroom::vroom("www/4_load_external_data/data_files/Improved_Response_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP6 <- DP6 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Upper Estimate")

DP2<- left_join(DP4,DP5)
remove(DP4,DP5)
DP2<- left_join(DP2,DP6)
remove(DP6)

DP2 <- as.data.table(DP2)

#DP7 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_By_County.csv")
DP7 = vroom::vroom("www/4_load_external_data/data_files/Worst_Case_Daily_Confirmed_Cases_By_County.csv")
DP7 <- DP7 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Expected Hospitalizations")

#DP8 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP8 = vroom::vroom("www/4_load_external_data/data_files/Worst_Case_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP8 <- DP8 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Lower Estimate")

#DP9 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP9 = vroom::vroom("www/4_load_external_data/data_files/Worst_Case_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP9 <- DP9 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Upper Estimate")

DP3<- left_join(DP7,DP8)
remove(DP7,DP8)
DP3<- left_join(DP3,DP9)
remove(DP9)

DP3 <- as.data.table(DP3)