IHMENationalProjections<-function(DaysProjected){

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
        colnames(IHMENationalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        
        #Get past data in daily hospital use
        #This will use a 5 day hospital stay as the average
        HistoricalDataDaily <- CovidConfirmedCases[,(5+5):length(CovidConfirmedCases)] -
          CovidConfirmedCases[,5:(length(CovidConfirmedCases)-5)]
        HistoricalDataHosp<-colSums(HistoricalDataDaily*CountyHospRate$HospRate)
        
        #Create dataframe to hold daily hospitalizations
        HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        
        IHMENationalData$ID<-rep("IHME", nrow(IHMENationalData))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        OverlayData<- dplyr::filter(IHMENationalData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
        OverlayData<-rbind(HistoricalData, OverlayData)
        
        projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
            geom_line() +
            scale_colour_manual(values=c("blue","black"))+
            scale_fill_manual(values = c("cadetblue", "gray"))+
            scale_linetype_manual(values = c("dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
            xlab('Date') +
            ylab('Daily Beds Required') +
            ggtitle("IHME Projected Daily Hospital Bed Utilization") +
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
            labs(color='')+
            scale_y_continuous(labels = comma)
        
        projections <- ggplotly(projections)
        projections <- projections %>% config(displayModeBar = FALSE)
        projections
}
