WorldHeatMaps <- function (MapView, MapScale, Metric){
  
  # Establish which map to be used
  if (MapView == "Europe"){
    MapChoice = EUROlist
    MapFilter = "Europe"
  }else if (MapView == "Asia"){
    MapChoice = ASIAlist
    MapFilter = "Asia"
  }else if (MapView == "North America"){
    MapChoice = NAlist
    MapFilter = "North America"
  }else if (MapView == "Africa"){
    MapChoice = AFRICAlist
    MapFilter = "Africa"
  }else if (MapView == "Oceania"){
    MapChoice = OCEANIAlist
    MapFilter = "Oceania"   
  }else if (MapView == "United States"){
    MapChoice = USlist
    MapFilter = "United States" 
  }else {
    MapChoice = WORLDlist
    MapFilter = "World" 
  }
  
  
  # Filter cases based on map choice
  if (MapFilter == "World"){
    DF<-NationalDataTable
    select <- which(DF$Country == "United States" & DF$State != "United States")
    DF = DF[-c(select),]
  } else if (MapFilter == "United States"){
    DF<-dplyr::filter(NationalDataTable, Country == MapFilter)
    DF<-dplyr::filter(NationalDataTable, Country == "United States" & State != "United States")
  } else{
    DF<-dplyr::filter(NationalDataTable, Continent == MapFilter)
  }
  
  
  
  # Establish Log cases for map choice
  if (MapScale == "Log"){
    DF$`Value` = round(log(DF$`Total Cases`, base=10),digits = 1)
  } else{
    DF$`Value` = DF$`Total Cases`
  }
  
  # Create text for hover tooltip
  if (MapFilter == "World" & Metric != "Total Cases"){
    DF$hoverVar = paste(DF$State)
  } else{
    DF$hoverVar = paste0(DF$State,
                         " -- Total Cases: ",
                         as.character(format(DF$`Total Cases`,big.mark=",")),
                         " || Cases Per 100k: ",
                         DF$`Cases Per 100,000 People`,
                         " || Population: ",
                         as.character(format(DF$Population,big.mark=","))
    )
  }
  
  names(DF)[8] = paste("Weekly Total Case Change Percent")
  names(DF)[9] = paste("Weekly Case Change Percent")
  
  DF$`Weekly Total Case Change Percent` = DF$`Weekly Total Case Change Percent`*100
  DF$`Weekly Case Change Percent` = DF$`Weekly Case Change Percent`*100
  
  # Make plot
  if (Metric == "Weekly Total Change"){
    DF$`Weekly Total Case Change` = DF$`Weekly Total Case Change`*100
    g = gvisGeoChart(DF, locationvar = "State", hovervar = c("hoverVar"), colorvar = "Weekly Total Case Change Percent",
                     options = MapChoice)
  } else if (Metric == "Weekly Change"){
    DF$`Weekly Case Change` = DF$`Weekly Case Change`*100
    g = gvisGeoChart(DF, locationvar = "State", hovervar = c("hoverVar"), colorvar = "Weekly Case Change Percent",
                     options = MapChoice)
  } else{
    g = gvisGeoChart(DF, locationvar = "State", hovervar = c("hoverVar"), colorvar = "Value",
                     options = MapChoice)
  }
  
  
}