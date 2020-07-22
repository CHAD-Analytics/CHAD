GetHPCONMap<-function(MAJCOMChoice){
  
  plot_data = HPCON_Map %>% group_by(Base)
  plot_data = filter(plot_data, Date == max(Date)) 
  
  if (MAJCOMChoice != "All") {
    plot_data<- plot_data %>% filter(MAJCOM == MAJCOMChoice)
    
  }
  
  plot_data$Date = as.character(plot_data$Date)
  
  labs = lapply(seq(nrow(plot_data)), 
                function(i) {
                  paste0(plot_data[i, "Base"], '</br>',
                         plot_data[i, "HPCON"], '</br></br>',
                         "Updated: ",plot_data[i, "Date"])
                })
  
  
  # mapStates = map("state", fill = TRUE, plot = FALSE)
  # leaflet(data = mapStates) %>% addTiles() %>%
  # addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
  #print(x)
  
  leaflet(data = plot_data) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat,
                     label = lapply(labs, htmltools::HTML),
                     labelOptions = labelOptions(noHide = F, textsize = "12px"),
                     fillColor = ~colorPalh(HPCON),
                     stroke = FALSE,
                     fillOpacity = 0.8,
    ) %>%
    addLegend("bottomright", pal = colorPalh, values = colorLabh,
              title = "HPCON Levels",
              opacity = 1,
    )
  
}

# # if (OpsSelect!="All"){
# #     plot_data<-dplyr::filter(plot_data,Operational %in% OpsSelect)
# # }
# # #Filter Majcom by branch
# # if (MAJNAFSelect=="MAJCOM"){
#     
#         #
#         #} else if(MAJCOMChoice=="Active Duty"){
#         #  plot_data<-plot_data %>%
#         #    filter((!MAJCOM %in% c("ANG","AFRC")) & (Days == ForecastChoice))
#         #}
#      } #else {
#     #     plot_data<- plot_data %>%
#     #       filter(MAJCOM == MAJCOMChoice & Days == ForecastChoice)
#     # }
# # }else{  # if NAF is
# #     if (NAFChoice == "All"){
# #         AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)
# #     } else {
# #         AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFChoice)
# #     }
# #     if (WingChoice=="All") {
# #         forecastbaselist<-dplyr::filter(AFWings,Wing %in% WingList)
# #         forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE)
# #         plot_data<-dplyr::filter(plot_data,Base %in% forecastbaselist)
# #         plot_data<- plot_data %>% filter(Days == ForecastChoice)
# #     } else {
# #         forecastbaselist<-dplyr::filter(AFWings,Wing %in% WingChoice)
# #         forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE)
# #         plot_data<-dplyr::filter(plot_data,Base %in% forecastbaselist)
# #         plot_data<- plot_data %>% filter(Days == ForecastChoice)
# #     }
# # }
# 
# 
# 
# 
# # GeoMarker <- gvisGeoChart(plot_data, 
# #                           locationvar = "latlong", 
# #                           hovervar = "base",  
# #                           colorvar="hpcon_factor", 
# #                           options=list(region="US",  
# #                                        displayMode = "Markers", 
# #                                        colorAxis="{colors:['green', 'blue', 'yellow', 'orange', 'red']}", 
# #                                        width=1600,
# #                                        height = 600,
# #                                        resolution = 'provinces',
# #                                        legendPosition='bottom',
# #                                        sizeAxis = "{minSize : 10, maxSize : 10}" )
# #                           )
# # GeoMarker
# # 

