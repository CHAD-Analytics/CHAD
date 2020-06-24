getNewCounty<- function(IncludedCounties, ChosenBase, rowNum) {
  
  allcounties<-GetCounties(ChosenBase,100,NULL,NULL) # Get all counties within 100 miles radius
  choroplethall <- st_as_sf(county_df)
  choroplethall <- st_transform(choroplethall, crs = 4326)
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
  choropleth <- st_as_sf(county_df)
  choropleth <- st_transform(choropleth, crs = 4326)
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
  if(rowNum<=allcountiesnum){
    myFIPS<-choroplethall[[rowNum,1]]
    myData<-CountyInfo[CountyInfo$FIPS==myFIPS,]
    myDataType<-1 #add a county to be colored
  }else{
    myFIPS<-choropleth[[rowNum-allcountiesnum,1]]
    myData<-CountyInfo[CountyInfo$FIPS==myFIPS,]
    myDataType<-0 #deleted a colored county
  }
  newlist<-list(myData,myDataType)
  
  return(newlist)
}