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
    
    fig <- plot_geo(HeatMap, locationmode = 'USA-states', sizes = c(20, 400))
    fig <- fig %>% add_markers(
      x = ~Long, y = ~Lat, size = ~IHME, color = ~IHMEID, colors = c("red","#228B22"), hoverinfo = "text",
      text = ~paste(HeatMap$Base, "<br />", HeatMap$IHME)
    )
    fig <- fig %>% layout(title = Banner , geo = g, showlegend=TRUE)
    fig <- fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.5,
                                        y = 0.95))
    
    # legend.sizes = seq(20,max(HeatMap$IHME), round(max(HeatMap$IHME)/8, -1))
    # ax = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
    # mk = list(sizeref=0.1, sizemode="area")
    # p.legend = plot_ly() %>%
    #   add_markers(x = 1, y = legend.sizes, size = legend.sizes, showlegend = F, marker = mk,  color = "#228B22") %>%
    #   layout(xaxis = ax, yaxis = list(showgrid = FALSE))
    # 
    # subplot(p.legend, fig, widths = c(0.1, 0.9))
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
    
    fig <- plot_geo(HeatMap, locationmode = 'USA-states', sizes = c(20, 400))
    fig <- fig %>% add_markers(
      x = ~Long, y = ~Lat, size = ~CHIME, color = ~CHIMEID, colors = c("red","#228B22"), hoverinfo = "text",
      text = ~paste(HeatMap$Base, "<br />", HeatMap$CHIME)
    )
    fig <- fig %>% layout(title = Banner , geo = g,showlegend=TRUE)
    fig <- fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.5,
                                        y = 0.97))
    
    # legend.sizes = seq(0,max(HeatMap$CHIME), round(max(HeatMap$CHIME)/8, -1))
    # ax = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
    # mk = list(sizeref=0.1, sizemode="area")
    # p.legend = plot_ly() %>%
    #   add_markers(x = 1, y = legend.sizes, size = legend.sizes, showlegend = F, marker = mk, color = "#228B22" ) %>%
    #   layout(xaxis = ax, yaxis = list(showgrid = FALSE))
    # 
    # subplot(p.legend, fig, widths = c(0.1, 0.9))
    
    fig
    
  }
  
}
