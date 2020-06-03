#' Create plot of Covid Cases by County
#' Create choropleth functions -------------------------------------------------------------------------------------------------------------------------------------------------------
PlotLocalChoro<-function(IncludedCounties, ChosenBase, TypofPlot){
  # this function has been upodated to bettr dislpay the counties and
  # their information.  I hope to later add selecting a grey county as an input change
  allcounties<-GetCounties(ChosenBase,100,NULL,NULL) # Get all counties within 100 miles radius
  
  if (TypofPlot == "County") {
    
    # choropleth <- st_as_sf(county_df)
    # choropleth <- st_transform(choropleth, crs = 4326)
    # choropleth<-choropleth %>% 
    #   mutate(GEOID = as.numeric(GEOID))
    # choropleth<-subset(choropleth, GEOID %in% IncludedCounties$FIPS)
    # BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    # Base_point<-st_point(c(BaseStats$Long, BaseStats$Lat)) #COrdinates for base
    # Base_point<-st_sfc(Base_point, crs=4326)
    # Base_point<-st_sf(BaseStats, geometry = Base_point)
    # 
    # ## Join the cases to spatial file by FIPS (GEOID) & add 360 to long so that we can project acroos date line
    # choropleth<-merge(choropleth, PlottingCountyData, by= "GEOID")
    # choropleth<-st_shift_longitude(choropleth)
    # Base_point<-st_shift_longitude(Base_point)
    # PlotCovidLocal<-ggplot()+
    #   geom_sf(data = choropleth,aes(fill=Cases, color=NAME)) +
    #   geom_sf(data = Base_point, color = "red", size = 3,show.legend ="Null")+
    #   # geom_text(data = Base_point,
    #   #           aes(x = Long+360, y = Lat,
    #   #               label = Base), hjust = .5) +
    #   ggtitle("COVID-19 Cases by County (County View)")+ 
    #   coord_sf() +
    #   theme_minimal() +
    #   theme(axis.line = element_blank(), axis.text = element_blank(),
    #         axis.ticks = element_blank(), axis.title = element_blank())+
    #   scale_fill_viridis(choropleth$Cases)
    # 
    # PlotCovidLocal <- ggplotly(PlotCovidLocal)%>% 
    #   style(hoveron = "fills",line.color = toRGB("gray60"))%>%
    #   hide_legend()
    # PlotCovidLocal <- PlotCovidLocal %>% config(displayModeBar = FALSE)
    #PlotCovidLocal
    
    # Build grey counties in the plot
    
    # choroplethall <- st_as_sf(county_df)
    # choroplethall <- st_transform(choroplethall, crs = 4326)
    choroplethall = choroplethObj
    choroplethall<-choroplethall %>% 
      mutate(STATEFP = fips_codes$state[match(as.numeric(STATEFP), as.numeric(fips_codes$state_code))])
    choroplethall<-choroplethall %>% 
      mutate(GEOID = as.numeric(GEOID))
    choroplethall<-subset(choroplethall, GEOID %in% allcounties$FIPS)
    choroplethall<-subset(choroplethall, !(GEOID %in% IncludedCounties$FIPS) )
    colnames(choroplethall)[6]<-"County"
    choroplethall<-merge(choroplethall, PlottingCountyData, by= "GEOID")
    choroplethall$County<-paste0(choroplethall$County, ", ", choroplethall$STATEFP, " (",choroplethall$LSAD,")")
    choroplethall<-st_shift_longitude(choroplethall)
    choroplethall<-choroplethall[order(choroplethall$County),]
    # row.names(choroplethall) <- choroplethall$County
    allcountiesnum<-nrow(choroplethall)
    
    
    # Build contties to color and display based on radius selected by user
    # choropleth <- st_as_sf(county_df)
    # choropleth <- st_transform(choropleth, crs = 4326)
    choropleth = choroplethObj
    choropleth<-choropleth %>% 
      mutate(STATEFP = fips_codes$state[match(as.numeric(STATEFP), as.numeric(fips_codes$state_code))])
    choropleth<-choropleth %>% 
      mutate(GEOID = as.numeric(GEOID))
    choropleth<-subset(choropleth, GEOID %in% IncludedCounties$FIPS)
    colnames(choropleth)[6]<-"County"
    choropleth<-merge(choropleth, PlottingCountyData, by= "GEOID")
    choropleth$County<-paste0(choropleth$County, ", ", choropleth$STATEFP, " (",choropleth$LSAD,")")
    choropleth<-st_shift_longitude(choropleth)
    choropleth<-choropleth[order(choropleth$County),]
    includedcountiesnum<-nrow(choropleth)
    
    # Build Base location point on graph
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    BaseStats = BaseStats[1,]
    Base_point<-st_point(c(BaseStats$Long, BaseStats$Lat)) #COrdinates for base
    Base_point<-st_sfc(Base_point, crs=4326)
    Base_point<-st_sf(BaseStats, geometry = Base_point)
    Base_point<-st_shift_longitude(Base_point)
    # if (is.null(CountyClicked$curveNumber)){}else{
    #   if(CountyClicked$curveNumber<=nrow(choroplethall)){ # remove from all and add to choropleth
    #     choropleth<-rbind(choropleth, choroplethall[as.integer(CountyClicked$curveNumber),])
    #     choroplethall <- choroplethall[-c(as.integer(CountyClicked$curveNumber)), ]
    #   }else{#remove from choropleth and add to all
    #     choroplethall<-rbind(choroplethall, choropleth[as.integer(CountyClicked$curveNumber)-nrow(choroplethall),])
    #     choropleth <- choropleth[-c(as.integer(CountyClicked$curveNumber)-nrow(choroplethall)), ]
    #     
    #     
    #   }
    # }
    if(allcountiesnum >0 ){
      PlotCovidLocal<-ggplot()+
        geom_sf(data = choroplethall,aes(color=County))+ # Grey counties
        geom_sf(data = choropleth,aes( fill=Cases, color=County)) + # counties with case data
        geom_sf(data = Base_point, color = "red", size = 1,show.legend ="Null")+ # base selected
        ggtitle("COVID-19 Cases by County (County View)")+ 
        coord_sf() +
        theme_minimal() +
        theme(axis.line = element_blank(), axis.text = element_blank(),  # get rid of axes
              axis.ticks = element_blank(), axis.title = element_blank())+
        scale_fill_viridis(choropleth$Cases) # scale county colors using continous scale
      event_register(PlotCovidLocal, 'plotly_click')
      
      PlotCovidLocal <- ggplotly(PlotCovidLocal,source="TEST") %>%
        style(hoveron = "fills") %>% #turn tooltip on when mosue over a region
        style(line.color = "black", line.width=1) %>%  # make all county outlines black
        style(line.color = "white", traces=1) %>%  # make graticules white so they disappear
        hide_legend() %>%
        style(line.color = "darkgrey", traces = c(2:(2+allcountiesnum-1))) #Make grey counties online grey
    }else{
      PlotCovidLocal<-ggplot()+
        geom_sf(data = choropleth,aes( fill=Cases, color=County)) + # counties with case data
        geom_sf(data = Base_point, color = "red", size = 1,show.legend ="Null")+ # base selected
        ggtitle("COVID-19 Cases by County (County View)")+ 
        coord_sf() +
        theme_minimal() +
        theme(axis.line = element_blank(), axis.text = element_blank(),  # get rid of axes
              axis.ticks = element_blank(), axis.title = element_blank())+
        scale_fill_viridis(choropleth$Cases) # scale county colors using continous scale
      event_register(PlotCovidLocal, 'plotly_click')
      
      PlotCovidLocal <- ggplotly(PlotCovidLocal) %>%
        style(hoveron = "fills") %>% # turn tooltip on when mosue over a region
        style(line.color = "black", line.width=1) %>% # make all county outlines black
        style(line.color = "white", traces=1)%>% # make graticules white so they disappear
        hide_legend()
    }
    PlotCovidLocal <- PlotCovidLocal %>% config(displayModeBar = FALSE)
    PlotCovidLocal
    
  } else  {
    # choropleth <- st_as_sf(county_df)
    # choropleth <- st_transform(choropleth, crs = 4326)
    # choropleth<-choropleth %>% 
    #   mutate(STATEFP = fips_codes$state[match(as.numeric(STATEFP), as.numeric(fips_codes$state_code))])
    # choropleth<-choropleth %>% 
    #   mutate(GEOID = as.numeric(GEOID))
    # choropleth<-subset(choropleth, STATEFP %in% IncludedCounties$State)
    # choropleth<-merge(choropleth, PlottingCountyData, by= "GEOID")
    # BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    # Base_point<-st_point(c(BaseStats$Long, BaseStats$Lat)) #COrdinates for base
    # Base_point<-st_sfc(Base_point, crs=4326)
    # Base_point<-st_sf(BaseStats, geometry = Base_point)
    # 
    # ## Join the cases to spatial file by FIPS (GEOID) & add 360 to long so that we can project acroos date line
    # choropleth<-st_shift_longitude(choropleth)
    # Base_point<-st_shift_longitude(Base_point)
    # PlotCovidLocal<-ggplot()+
    #   geom_sf(data = choropleth,aes(fill=Cases, color=NAME)) +
    #   geom_sf(data = Base_point, color = "red", size = 3,show.legend ="Null")+
    #   # geom_text(data = Base_point,
    #   #           aes(x = Long+360, y = Lat,
    #   #               label = Base), hjust = .5) +
    #   ggtitle("COVID-19 Cases by County (County View)")+ 
    #   coord_sf() +
    #   theme_minimal() +
    #   theme(axis.line = element_blank(), axis.text = element_blank(),
    #         axis.ticks = element_blank(), axis.title = element_blank())+
    #   scale_fill_viridis(choropleth$Cases)
    # 
    # PlotCovidLocal <- ggplotly(PlotCovidLocal)%>% 
    #         style(hoveron = "fills",line.color = toRGB("gray60"))%>%
    #   hide_legend()
    # PlotCovidLocal <- PlotCovidLocal %>% config(displayModeBar = FALSE)
    # PlotCovidLocal
    
    # Build grey counties in the plot for any state within 100 miloes of location selected
    # choroplethall <- st_as_sf(county_df)
    # choroplethall <- st_transform(choroplethall, crs = 4326)
    choroplethall = choroplethObj
    choroplethall<-choroplethall %>% 
      mutate(STATEFP = fips_codes$state[match(as.numeric(STATEFP), as.numeric(fips_codes$state_code))])
    choroplethall<-choroplethall %>% 
      mutate(GEOID = as.numeric(GEOID))
    choroplethall<-subset(choroplethall, STATEFP %in% allcounties$State)
    choroplethall<-subset(choroplethall, !(GEOID %in% IncludedCounties$FIPS) )
    colnames(choroplethall)[6]<-"County"
    choroplethall<-merge(choroplethall, PlottingCountyData, by= "GEOID")
    choroplethall$County<-paste(choroplethall$County, ", ", choroplethall$STATEFP)
    choroplethall<-st_shift_longitude(choroplethall)
    allcountiesnum<-nrow(choroplethall)    
    
    # Build contties to color and display based on radius selected by user
    # choropleth <- st_as_sf(county_df)
    # choropleth <- st_transform(choropleth, crs = 4326)
    choropleth = choroplethObj
    choropleth<-choropleth %>% 
      mutate(STATEFP = fips_codes$state[match(as.numeric(STATEFP), as.numeric(fips_codes$state_code))])
    choropleth<-choropleth %>% 
      mutate(GEOID = as.numeric(GEOID))
    choropleth<-subset(choropleth, GEOID %in% IncludedCounties$FIPS)
    colnames(choropleth)[6]<-"County"
    choropleth<-merge(choropleth, PlottingCountyData, by= "GEOID")
    choropleth$County<-paste(choropleth$County, ", ", choropleth$STATEFP)
    choropleth<-st_shift_longitude(choropleth)
    includedcountiesnum<-nrow(choropleth)
    
    # Build Base location point on graph
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    BaseStats = BaseStats[1,]
    Base_point<-st_point(c(BaseStats$Long, BaseStats$Lat)) #COrdinates for base
    Base_point<-st_sfc(Base_point, crs=4326)
    Base_point<-st_sf(BaseStats, geometry = Base_point)
    Base_point<-st_shift_longitude(Base_point)
    
    PlotCovidLocal<-ggplot()+
      geom_sf(data = choroplethall,aes(color=County))+ # Grey counties
      geom_sf(data = choropleth,aes( fill=Cases, color=County)) + # counties with case data
      scale_fill_viridis(choropleth$Cases)+
      geom_sf(data = Base_point, color = "red", size = 1,show.legend ="Null")+ # base selected
      ggtitle("COVID-19 Cases by County (County View)")+ 
      coord_sf() +
      theme_minimal() +
      theme(axis.line = element_blank(), axis.text = element_blank(),  # get rid of axes
            axis.ticks = element_blank(), axis.title = element_blank())+
      scale_fill_viridis(choropleth$Cases) # scale county colors using continous scale
    
    PlotCovidLocal <- ggplotly(PlotCovidLocal) %>%
      style(hoveron = "fills") %>% # turn tooltip on when mosue over a region
      style(line.color = "black")%>% # make all county outlines black
      style(line.color = "white", traces=1)%>% # make graticules white so they disappear
      hide_legend() %>%
      style(line.color = "darkgrey", traces = c(2:(2+allcountiesnum-1))) #Make grey counties online grey
    PlotCovidLocal <- PlotCovidLocal %>% config(displayModeBar = FALSE)
    PlotCovidLocal
    
  }
  #if (is.null(county_clicked$curveNumber)){}else{print(PlotCovidLocal[["x"]][["data"]][[as.numeric(county_clicked$curveNumber)]][["name"]])}
  # return(PlotCovidLocal[["x"]][["data"]][[as.numeric(county_clicked$curveNumber)]][["name"]])
}

