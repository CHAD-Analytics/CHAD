PlottingCountyData = vroom::vroom("www/3_load_external_data/data_files/time_series_covid19_confirmed_US.csv")

PlottingCountyData = PlottingCountyData[colSums(!is.na(PlottingCountyData)) > 0]

PlottingCountyData = data.frame(PlottingCountyData[,5],
                                rev(PlottingCountyData)[,1])

colnames(PlottingCountyData) = c("GEOID","Cases")

# Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
county_df = tigris::counties(state = NULL, 
                             cb = TRUE, 
                             resolution = "5m")
