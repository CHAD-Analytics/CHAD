NationalOverlayPlot<-function(SocialDistance, DaysForecasted){
    
    
  #Get IHME Data upper lower and mean combined by date
  Dataframe1<-IHME_Model %>% 
    group_by(date) %>% 
    summarise(allbed_mean = sum(allbed_mean))
  Dataframe2<-IHME_Model %>% 
    group_by(date) %>% 
    summarise(allbed_lower = sum(allbed_lower))
  Dataframe3<-IHME_Model %>% 
    group_by(date) %>% 
    summarise(allbed_upper = sum(allbed_upper))
  IHMENationalData<-cbind(Dataframe1, Dataframe2$allbed_lower, Dataframe3$allbed_upper)
  
  #Get past data in daily hospital use
  #This will use a 5 day hospital stay as the average
  HistoricalDataDaily <- CovidConfirmedCases[,(5+5):length(CovidConfirmedCases)] -
    CovidConfirmedCases[,5:(length(CovidConfirmedCases)-5)]
  HistoricalDataHosp<-colSums(HistoricalDataDaily*CountyHospRate$HospRate)
  
  #Create dataframe to hold daily hospitalizations
  HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
  HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
  colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
  
  currHosp = HistoricalData[nrow(HistoricalData),2]
  
  ####################################################################################
  #Mean Estimate
  NationalPop <-  sum(CountyInfo$Population)
  NationalCases<-sum(rev(CovidConfirmedCases)[1]-rev(CovidConfirmedCases)[8])
  
  
  #Next we use the calculated values, along with estimated values from the Estimated Values. 
  #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
  cases<-currHosp
  pop<-NationalPop
  doubling<-8
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14
  socialdistancing<-SocialDistance
  hospitalizationrate<-14
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7
  daysforecasted<-120
  
  #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
  #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
  SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                             socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                             ventilatortime,daysforecasted,Ro, .5)
  
  MyDates<-seq(Sys.Date()-(length(CovidConfirmedCases)-80), length=daysforecasted, by="1 day")
  DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
  TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
  colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
  colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
  
  
  ####################################################################################
  #Lower Estimate
  
  #Next we use the calculated values, along with estimated values from the Estimated Values. 
  #The only input we want from the user is the social distancing rate. For this example, we just use 0.5

  doubling<-10
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14

  hospitalizationrate<-10
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7

  
  
  
  #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
  #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
  SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                             socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                             icutime,ventilatortime,daysforecasted,Ro, .5)
  
  DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
  TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
  colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
  colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
  
  ####################################################################################
  #Upper Estimate
  #Next we use the calculated values, along with estimated values from the Estimated Values. 
  
  #The only input we want from the user is the social distancing rate. For this example, we just use 0.5

  doubling<-7
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14

  hospitalizationrate<-20
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7

  
  #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
  #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
  SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                             socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                             icutime,ventilatortime,daysforecasted,Ro, .5)
  
  DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
  TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
  colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Estimate","Upper Estimate")
  colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases","Maximum Total Cases")
  
  DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
  DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
  DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
  DailyData<-DailyData[-1,]
  
  colnames(IHMENationalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
  DailyData$ID<-rep("CHIME",nrow(DailyData))
  IHMENationalData$ID<-rep("IHME",nrow(IHMENationalData))
  HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
  HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
  
  OverlayData<-rbind(DailyData,IHMENationalData)
  OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
  OverlayData<- dplyr::filter(OverlayData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysForecasted))
  OverlayData<-rbind(HistoricalData, OverlayData)    
  
  
  projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
    geom_line() + 
    scale_colour_manual(values=c("tan", "blue", "black"))+
    scale_fill_manual(values = c("tan4", "cadetblue", "gray"))+
    scale_linetype_manual(values = c("dashed", "dashed", "solid"))+
    geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                alpha = .2) +
    ggtitle("Projected Daily Hospital Bed Utilization")+
    ylab("Daily Beds Required")+
    theme_bw() + 
    theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
          axis.title = element_text(face = "bold", size = 11, family = "sans"),
          axis.text.x = element_text(angle = 60, hjust = 1), 
          axis.line = element_line(color = "black"),
          legend.position = "top",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    scale_x_date(date_breaks = "2 week")+
    labs(color = "ID")+
    scale_y_continuous(labels = comma)
  
  
  
  ggplotly(projections)
}
