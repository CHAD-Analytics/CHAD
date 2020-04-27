CovidDeaths = vroom::vroom("www/3_load_external_data/data_files/time_series_covid19_deaths_US.csv")

CovidDeaths = CovidDeaths[,c(5, 13:ncol(CovidDeaths))]
colnames(CovidDeaths)[1] = "CountyFIPS"

CovidDeaths = merge(StateInfo,CovidDeaths,by = "CountyFIPS")

colnames(CovidDeaths)[1] = "CountyFIPS"

CovidDeaths[is.na(CovidDeaths)] <- 0

