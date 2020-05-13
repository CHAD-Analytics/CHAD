#DP1 <- vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_By_County.csv")
DP1 = vroom::vroom("www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_By_County.csv")
DP1 <- DP1 %>% select(-c(State)) %>%
  reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Expected Hospitalizations")
   
#DP2 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP2 = vroom::vroom("www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP2 <- DP2 %>% select(-c(State)) %>%
  reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Lower Estimate")

#DP3 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP3 = vroom::vroom("www/4_load_external_data/data_files/Relaxed_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP3 <- DP3 %>% select(-c(State)) %>%
  reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Upper Estimate")

DP1<- left_join(DP1,DP2)
remove(DP2)
DP1<- left_join(DP1,DP3)
remove(DP3)

DP1 <- as.data.table(DP1)

#DP4 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_By_County.csv")
DP4 = vroom::vroom("www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_By_County.csv")
DP4 <- DP4 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Expected Hospitalizations")

#DP5 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP5 = vroom::vroom("www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_Lower_Bound_By_County.csv")
DP5 <- DP5 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Lower Estimate")

#DP6 = vroom::vroom("C:/Users/taylo/Documents/CHAD/covid19/www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP6 = vroom::vroom("www/4_load_external_data/data_files/Testing_Daily_Confirmed_Cases_Upper_Bound_By_County.csv")
DP6 <- DP6 %>% select(-c(State)) %>%
   reshape2::melt(id.var = c('FIPS'), variable.name = 'ForecastDate', value.name = "Upper Estimate")

DP2<- left_join(DP4,DP5)
remove(DP4,DP5)
DP2<- left_join(DP2,DP6)
remove(DP6)

DP2 <- as.data.table(DP2)
