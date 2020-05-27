GetHeatMap<-function(BranchSelect,OpsSelect,MAJNAFSelect,MAJCOMChoice,NAFChoice,WingChoice,ModelChoice,ForecastChoice,Stat){
  if (Stat == "Cases") {
    HeatMap<-HeatMapForecastCases
    Banner<-"Projected Daily New Cases"
  } else {
    HeatMap<-HeatMapForecast
    Banner<-"Projected Daily New Hospitalizations"
  }
  
  HeatMap<-dplyr::filter(HeatMap,Branch %in% BranchSelect) #"Air Force") 

  if (BranchSelect!="Air Force"){
        HeatMap<- HeatMap %>% filter(Days == ForecastChoice)
        if (OpsSelect!="All"){
          HeatMap<-dplyr::filter(HeatMap,Operational %in% OpsSelect)
        }
  } else {
        if (OpsSelect!="All"){
          HeatMap<-dplyr::filter(HeatMap,Operational %in% OpsSelect)
        }    
        #Filter Majcom by branch 
        if (MAJNAFSelect=="MAJCOM"){
          if (MAJCOMChoice=="All") {
            HeatMap<- HeatMap %>% filter(Days == ForecastChoice)
          #
          #} else if(MAJCOMChoice=="Active Duty"){
          #  HeatMap<-HeatMap %>%
          #    filter((!MAJCOM %in% c("ANG","AFRC")) & (Days == ForecastChoice))
          #}
          } else {
            HeatMap<- HeatMap %>%
              filter(MAJCOM == MAJCOMChoice & Days == ForecastChoice)
          }
        }else{  # if NAF is 
          if (NAFChoice == "All"){
            AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)
          } else {
            AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFChoice)
          }
          if (WingChoice=="All") {
            forecastbaselist<-dplyr::filter(AFWings,Wing %in% WingList)            
            forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
            HeatMap<-dplyr::filter(HeatMap,Base %in% forecastbaselist) 
            HeatMap<- HeatMap %>% filter(Days == ForecastChoice)
          } else {
            forecastbaselist<-dplyr::filter(AFWings,Wing %in% WingChoice)            
            forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
            HeatMap<-dplyr::filter(HeatMap,Base %in% forecastbaselist)       
            HeatMap<- HeatMap %>% filter(Days == ForecastChoice)
          }
        }
  }

  
#Consider Group Filtering
  # if (WingChoice=="All") {
  #   forecastbaselist<-dplyr::filter(AFWings,Wing %in% WingList)
  #   if (GroupChoice!="All") {                
  #     forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupChoice)                 
  #   } else {
  #     forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupList)                     
  #   }
  #   forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
  #   basesRadius<-dplyr::filter(basesRadius,base %in% forecastbaselist) 
  #   basesRadius <- basesRadius %>% 
  #     mutate(include = ifelse((new_cases_7_pp > 500) & (date == current_date), TRUE, FALSE))
  # } else {
  #   forecastbaselist<-dplyr::filter(AFWings,Wing %in% WingChoice) 
  #   if (GroupChoice!="All") {                
  #     forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupChoice)                 
  #   } else {
  #     forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupList)                     
  #   }                
  #   forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
  #   basesRadius<-dplyr::filter(basesRadius,base %in% forecastbaselist)       
  #   basesRadius <- basesRadius %>% 
  #     mutate(include = ifelse((new_cases_7_pp > 500) & (date == current_date), TRUE, FALSE))
  # }            

  
  if (ModelChoice=="IHME") {
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )
    
    HeatMap$IHME = ifelse(is.infinite(HeatMap$IHME),0,HeatMap$IHME)
    
    legend.sizes = seq(min(HeatMap$IHME),max(HeatMap$IHME), ceiling(max(HeatMap$IHME)/8))
    ax = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
    mk = list(sizeref=0.15, sizemode="area", symbol="circle-open", color="black")
     
    fig <- plot_geo(HeatMap, locationmode = 'USA-states', sizes = c(20, 400))
    fig <- fig %>% add_markers(x = ~Long, y = ~Lat, size = ~IHME, color = ~IHMEID, colors= c("Over 5% Population"="red", 
      "Under 5% Population"="#228B22",
      "Over Capacity"="red", "Under Capacity"="#228B22"), hoverinfo = "text",
      text = ~paste(HeatMap$Base, "<br />", HeatMap$IHME), marker=list(sizeref=0.55, opacity=0.5, sizemode="area")
    )
    
    fig <- fig %>% layout(title = Banner , geo = g, showlegend=TRUE)
    fig <- fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.5,
                                        y = 0.95))
    

    p.legend = plot_ly() %>%
      add_markers(x=1, y=legend.sizes, size = legend.sizes, showlegend = F, marker=mk,hoverinfo="text",
                  text= ~paste("Frequency = ", legend.sizes)) %>%
      layout(xaxis = ax, yaxis = list(showgrid = FALSE, zeroline=FALSE, showline=FALSE))

    fig<-subplot(p.legend, fig, widths = c(0.1, 0.9))
    
    fig
    
  } else {
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )

    legend.sizes = seq(min(HeatMap$CHIME), max(HeatMap$CHIME), ceiling(max(HeatMap$CHIME)/8))  
    ax = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
    mk = list(sizeref=0.15, sizemode="area", symbol="circle-open", color="black") 
    
    fig <- plot_geo(HeatMap, locationmode = 'USA-states', sizes = c(20, 400))
    fig <- fig %>% add_markers(x = ~Long, y = ~Lat, size =~CHIME,  
      color = ~CHIMEID, colors= c("Over 5% Population"="red", 
      "Under 5% Population"="#228B22",
     "Over Capacity"="red", "Under Capacity"="#228B22"), hoverinfo = "text",
      text = ~paste(HeatMap$Base, "<br />", HeatMap$CHIME), marker=list(sizeref=0.55, opacity=0.5, sizemode="area")
    )
    
    fig <- fig %>% layout(title = Banner , geo = g,showlegend=TRUE)
    fig <- fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                         xanchor = "center",  # use center of legend as anchor
                                         x = 0.5,
                                         y = 0.95))
    
    

    p.legend = plot_ly() %>%
      add_markers(x=1, y=legend.sizes, size =legend.sizes, showlegend=F, marker=mk,hoverinfo="text",
                  text= ~paste("Frequency = ", legend.sizes)) %>% 
      layout(xaxis = ax,
             yaxis=list(showgrid=FALSE, zeroline=FALSE, showline=FALSE))

    fig<-subplot(p.legend, fig, widths = c(0.1, 0.9))

    fig
    
  }
  


}
