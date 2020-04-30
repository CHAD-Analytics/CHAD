GetHeatMap<-function(MAJCOMChoice,ModelChoice,ForecastChoice,Stat){
  if (Stat == "Cases") {
    HeatMap<-HeatMapForecastCases
    Banner<-"Projected Daily New Cases"
  } else {
    HeatMap<-HeatMapForecast
    Banner<-"Projected Daily New Hospitalizations"
  }
  if (MAJCOMChoice=="All") {
    HeatMap<- HeatMap %>%
      filter(Days == ForecastChoice)
  } else if(MAJCOMChoice=="Active Duty"){
    HeatMap<-HeatMap %>%
      filter((!MAJCOM %in% c("ANG","AFRC")) & (Days == ForecastChoice))
  }
  else {
    HeatMap<- HeatMap %>%
      filter(MAJCOM == MAJCOMChoice & Days == ForecastChoice)
  }
  
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
    
    legend.sizes = seq(min(HeatMap$IHME),max(HeatMap$IHME), round(max(HeatMap$IHME)/8, -1))
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

    legend.sizes = seq(min(HeatMap$CHIME), max(HeatMap$CHIME), round(max(HeatMap$CHIME)/8,-2))  
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
