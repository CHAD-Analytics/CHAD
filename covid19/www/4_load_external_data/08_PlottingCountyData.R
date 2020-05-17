PlottingCountyData = as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))


PlottingCountyData = PlottingCountyData[colSums(!is.na(PlottingCountyData)) > 0]

PlottingCountyData = data.frame(PlottingCountyData[,5],
                                rev(PlottingCountyData)[,1])

colnames(PlottingCountyData) = c("GEOID","Cases")

# Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
county_df = tigris::counties(state = NULL, 
                             cb = TRUE, 
                             resolution = "5m")
