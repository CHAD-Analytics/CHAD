#' Create plot of Covid Cases by County
#' Create choropleth functions -------------------------------------------------------------------------------------------------------------------------------------------------------
PlotLocalChoro<-function(IncludedCounties, ChosenBase, TypofPlot){
  
  if (TypofPlot == "County") {
    choropleth <- st_as_sf(county_df)
    choropleth <- st_transform(choropleth, crs = 4326)
    choropleth<-choropleth %>% 
      mutate(GEOID = as.numeric(GEOID))
    choropleth<-subset(choropleth, GEOID %in% IncludedCounties$FIPS)
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    Base_point<-st_point(c(BaseStats$Long, BaseStats$Lat)) #COrdinates for base
    Base_point<-st_sfc(Base_point, crs=4326)
    Base_point<-st_sf(BaseStats, geometry = Base_point)
    
    ## Join the cases to spatial file by FIPS (GEOID) & add 360 to long so that we can project acroos date line
    choropleth<-merge(choropleth, PlottingCountyData, by= "GEOID")
    choropleth<-st_shift_longitude(choropleth)
    Base_point<-st_shift_longitude(Base_point)
    PlotCovidLocal<-ggplot()+
      geom_sf(data = choropleth,aes(fill=Cases, color=NAME)) +
      geom_sf(data = Base_point, color = "red", size = 3,show.legend ="Null")+
      # geom_text(data = Base_point,
      #           aes(x = Long+360, y = Lat,
      #               label = Base), hjust = .5) +
      ggtitle("COVID-19 Cases by County (County View)")+ 
      coord_sf() +
      theme_minimal() +
      theme(axis.line = element_blank(), axis.text = element_blank(),
            axis.ticks = element_blank(), axis.title = element_blank())+
      scale_fill_viridis(choropleth$Cases)
    
    PlotCovidLocal <- ggplotly(PlotCovidLocal)%>% 
      style(hoveron = "fills",line.color = toRGB("gray60"))%>%
      hide_legend()
    PlotCovidLocal <- PlotCovidLocal %>% config(displayModeBar = FALSE)
    PlotCovidLocal
    
  } else  {
    choropleth <- st_as_sf(county_df)
    choropleth <- st_transform(choropleth, crs = 4326)
    choropleth<-choropleth %>% 
      mutate(STATEFP = fips_codes$state[match(as.numeric(STATEFP), as.numeric(fips_codes$state_code))])
    choropleth<-choropleth %>% 
      mutate(GEOID = as.numeric(GEOID))
    choropleth<-subset(choropleth, STATEFP %in% IncludedCounties$State)
    choropleth<-merge(choropleth, PlottingCountyData, by= "GEOID")
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    Base_point<-st_point(c(BaseStats$Long, BaseStats$Lat)) #COrdinates for base
    Base_point<-st_sfc(Base_point, crs=4326)
    Base_point<-st_sf(BaseStats, geometry = Base_point)
    
    ## Join the cases to spatial file by FIPS (GEOID) & add 360 to long so that we can project acroos date line
    choropleth<-st_shift_longitude(choropleth)
    Base_point<-st_shift_longitude(Base_point)
    PlotCovidLocal<-ggplot()+
      geom_sf(data = choropleth,aes(fill=Cases, color=NAME)) +
      geom_sf(data = Base_point, color = "red", size = 3,show.legend ="Null")+
      # geom_text(data = Base_point,
      #           aes(x = Long+360, y = Lat,
      #               label = Base), hjust = .5) +
      ggtitle("COVID-19 Cases by County (County View)")+ 
      coord_sf() +
      theme_minimal() +
      theme(axis.line = element_blank(), axis.text = element_blank(),
            axis.ticks = element_blank(), axis.title = element_blank())+
      scale_fill_viridis(choropleth$Cases)
    
    PlotCovidLocal <- ggplotly(PlotCovidLocal)%>% 
            style(hoveron = "fills",line.color = toRGB("gray60"))%>%
      hide_legend()
    PlotCovidLocal <- PlotCovidLocal %>% config(displayModeBar = FALSE)
    PlotCovidLocal
  }
}
