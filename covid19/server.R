##################
##### Server #####
##################

# Layout
##############################################################################################################################################
# The server is used to generate outputs based on the functions in the global. These outputs are then referenced in the UI and diplayed in the app
# First:  Creating reactive functions that change based on radius and Base. The reactive functions are the most important functions in the app.
#         Reactive functions change every time a new base is chosen or a radius is chosen. This updated the app automatically.
# Second: This creates the output variables that can be referenced in the user interface. Each plot, statistic or map needs to have an output.
#         There are 5 sub categories included: Common statistics, line plots, choropleth charts, projections, and data tables.
# Third:  This creates the help settings in the app so that users can see documentation of inputs, sources, and calculations.
##############################################################################################################################################       


# Define server logic, within this all ouputs and reactive variables are generated. 
server <- function(input, output,session) {
    
    # Step One
    ###################################################################################################################################################
    
    
    
    
    # Step Two
    ###################################################################################################################################################
    
    
    # Output common statistics -------------------------------------------------------------------------------------------------------------------------------------------
    
    #Finds which counties in given radius. Also Give county statistics
    output$TotalPopulation <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        valueBox(subtitle = "Total Regional Population",
                 comma(CalculateCounties(MyCounties)),
                 #icon = icon("list-ol"),
                 color = "light-blue"
        )
        
    })
    
    # Finds Covid Cases and statistics on covid per county
    output$CovidCases <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        valueBox(subtitle = "Total Confirmed Cases",
                 comma(CalculateCovid(MyCounties)),
                 #icon = icon("list-ol"),
                 color = "light-blue"
        )
        
    })
    
    # Finds Covid Cases per 1,000
    output$CasesPer1000 <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        valueBox(subtitle = "Total Confirmed Cases per 1,000",
                 comma(CalculateCovid1000(MyCounties)),
                 #icon = icon("list-ol"),
                 color = "teal"
        )
        
    })
    
    #Outputs change in covid cases per day
    output$CaseChangeLocal <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        
        valueBox(paste("+",toString(changeC)),
                 subtitle = "New Confirmed Cases", 
                 color = "light-blue")
    })
    
    
    # Finds Covid deaths and statistics on covid per county
    output$LocalCovidDeaths <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        valueBox(subtitle = "Total Fatalities",
                 comma(CalculateDeaths(MyCounties)),
                 #icon = icon("skull"),
                 color = "light-blue"
        )
    })
    
    #Outputs change in deaths per day   
    output$DeathChangeLocal <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        CovidCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        
        valueBox(paste("+",toString(changeC)),
                 subtitle = "New Fatalities", 
                 color = "light-blue")
    })
    
    #Finds hospital information within a given 100 mile radius. Calculates number of total hospital beds. Can compare to number of cases
    output$HospitalUtilization <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        valueBox(subtitle = "Estimated Local Hospital Bed Utilization",
                 HospitalIncreases(MyCounties),
                 #icon = icon("hospital"),
                 color = "navy")
    })
   
    
    output$CaseDbRate <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        valueBox(paste(CaseDblRate(MyCounties),"days"),
                 subtitle = "Case Doubling Rate",
                 color = "teal")
    })
    
    
    output$Rt_Estimate <- renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        valueBox(paste(Estimate_Rt(MyCounties)),
                 subtitle = "Estimated Virus Reproduction Rate",
                 color = "navy")
    })
    
    
###################################################################################################
    
    output$CHIMEPeakDate<-renderValueBox({
        MyCounties<-GetCounties(input$Base,input$Radius)
        if (is.null(input$SocialDistanceValue) ){social_dist<-1}
        
        CS      <- "CS"       %in% input$SocialDistanceValue
        CB    <- "CB"     %in% input$SocialDistanceValue
        SD <- "SD"  %in% input$SocialDistanceValue
        
        if (CS & CB & SD){
            social_dist <- 27
        } else if (CS & CB){
            social_dist <- 12
        } else if (CS & SD){
            social_dist <-19
        } else if (SD & CB){
            social_dist <-23
        } else if (CS) {
            social_dist <- 4
        }  else if (CB) {
            social_dist <- 8
        }  else if (SD) {
            social_dist <- 15
        }
        Peak<-CalculateCHIMEPeak(MyCounties, input$Base, input$Radius, social_dist, input$proj_days, input$StatisticType)
        Peak<-format(Peak)
        if (input$StatisticType == "Hospitalizations") {
            valueBox(subtitle = "CHIME Predicted Peak Hospitalizations",
                     paste(Peak),
                     #icon = icon("hospital"),
                     color = "blue") 
        } else {
                valueBox(subtitle = "CHIME Predicted Total Fatalities",
                         paste(Peak),
                         #icon = icon("skull"),
                         color = "blue")}
        
    })
    
    # output$CHIMEMinMax<-renderValueBox({
    #     MyCounties<-GetCounties()
    #     Peak<-CalculateCHIMEMinMax(MyCounties, input$Base, input$Radius, input$social_dist, input$proj_days)
    #     Peak<-format(Peak)
    #     valueBox(subtitle = "CHIME Predicted Peak Hospitalizations",
    #              paste(Peak),
    #              icon = icon("hospital"),
    #              color = "blue")
    # })
    
    output$IHMEPeakDate<-renderValueBox({
        MyHospitals<-GetHospitals(input$Base,input$Radius)
        Peak<-CalculateIHMEPeak(input$Base, MyHospitals, input$Radius, input$StatisticType)
        Peak<-format(Peak)
        if (input$StatisticType == "Hospitalizations") {
            valueBox(subtitle = "IHME Predicted Peak Hospitalizations",
                     paste(Peak),
                     #icon = icon("hospital"),
                     color = "navy")
        } else {
            valueBox(subtitle = "IHME Predicted Total Fatalities",
                     paste(Peak),
                     #icon = icon("hospital"),
                     color = "navy")
        }
        
    })
    
    
    # output$IHMEMinMax<-renderValueBox({
    #     MyHospitals<-GetHospitals()
    #     Peak<-CalculateIHMEMinMax(input$Base, MyHospitals, input$Radius)
    #     valueBox(subtitle = "IHME Predicted Min/Max Hospitalizations",
    #              paste(Peak),
    #              icon = icon("hospital"),
    #              color = "navy")
    # })
    
    # Output line plots for the dashboard ----------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create local health plot for Daily Cases 
    output$LocalHealthPlot1<-renderPlotly({
        
        MyCounties<-GetCounties(input$Base,input$Radius)
        DailyChart <- CovidCasesPerDayChart(MyCounties)
        DailyChart <- dplyr::filter(DailyChart, ForecastDate >= DailyChart$ForecastDate[1] + 35)
        
        plotDaily <- ggplot(DailyChart) + 
            geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 0.5) +
            scale_colour_manual(values=c("Blue", "Red")) +
            xlab('Date') +
            ylab('Number of People') +
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
            scale_x_date(date_breaks = "1 week") +
            labs(color='')
        
        plotDaily <- ggplotly(plotDaily)
        plotDaily <- plotDaily %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                          xanchor = "center",  # use center of legend as anchor
                                          x = 0.5,
                                          y = 1.2)) %>% config(displayModeBar = FALSE)
        plotDaily
    })
    
    #Create second plot of local health population 
    output$LocalHealthPlot2<-renderPlotly({
        
        MyCounties<-GetCounties(input$Base,input$Radius)
        CumulChart <- CovidCasesCumChart(MyCounties)
        CumulChart <- dplyr::filter(CumulChart, ForecastDate >= CumulChart$ForecastDate[1] + 35)
        
        #Plot for local area cumulative cases
        plotTot <- ggplot(CumulChart,height = 250) + 
            geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 0.5) +
            scale_colour_manual(values=c("Blue", "Red"))+
            xlab('Date') +
            ylab('Number of People') +
            theme_bw() + 
            theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
                  axis.title = element_text(face = "bold", size = 11, family = "sans"),
                  axis.text.x = element_text(angle = 60, hjust = 1), 
                  axis.line = element_line(color = "black"),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  legend.position = c(0, 1),) +
            scale_x_date(date_breaks = "1 week")
        
        plotTot <- ggplotly(plotTot)
        plotTot <- plotTot %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                          xanchor = "center",  # use center of legend as anchor
                                          x = 0.5,
                                          y = 1.2)) %>% config(displayModeBar = FALSE)
        plotTot
    })
    
    
    
    # Output Choropleth Charts ----------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create Country Plot on Summary page
    output$SummaryPlot<-renderGvis({
        DF<-cbind.data.frame(CovidConfirmedCases$State, rev(CovidConfirmedCases)[,1], rev(CovidConfirmedCases)[,1])
        colnames(DF)<-c("state","Value","LogValue")
        ChlorData<-plyr::ddply(DF, "state", numcolwise(sum))
        ChlorData<-transform(ChlorData, LogValue = round(log(LogValue, base=10),digits = 1))
        ChlorData <- transform(ChlorData, Value = as.character(format(Value,big.mark=",")))
        ChlorData<-ChlorData %>%
            mutate(state_name = state.name[match(state, state.abb)])
        ChlorData<-ChlorData[complete.cases(ChlorData$state_name), ]
        ChlorData <- transform(ChlorData, Value =paste(state_name, " Total Cases: ", Value))
        states <- data.frame(ChlorData$state_name, ChlorData$Value, ChlorData$LogValue)
        colnames(states)<-c("state_name","Cases","StateColor")
        gvisGeoChart(states, "state_name", "StateColor", hovervar = "Cases",
                     options=list(region="US",
                                  colors="['#D3D3D3', 'red']",
                                  displayMode="regions", 
                                  resolution="provinces",
                                  width=1200,
                                  height = 600,
                                  legend = "none"))
    })
    
    
    #Creates the local choropleth charts that change based on which base and radius.
    output$LocalChoroPlot<-renderPlotly({
        MyCounties<-GetCounties(input$Base,input$Radius)
        PlotLocalChoro(MyCounties, input$Base, input$TypeLocal)
    })
    
    #Choice between cases heat map or hospitalizations heat map
    output$SummaryTabChoro<-renderPlotly({
            GetHeatMap(input$MAJCOMInput, input$SummaryModelType, input$SummaryForecast, input$SummaryStatistic)
    })
    
    
    
    
    # Output Projections  ---------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    # Output AMC Analysis
    output$ProjectedEpidemicTable<-renderPlotly({
        
        baseUsed = input$Base
        
        # Read the json file and convert it to data.frame
        #myList <- fromJSON("data/shinyjson.json")
        
        df <- AMC_model
        
        df <- select(df, "DataDate", "DataType", baseUsed)
        
        colnames(df)[3]  <- "Data"
        
        myTibble <- as_tibble(df)
        
        cummInf <- myTibble %>% filter(DataType == "Cumulative Infections")
        currInf <- myTibble %>% filter(DataType == "Current Infections")
        cummDeath <- myTibble %>% filter(DataType == "Cumulative Deaths")
        
        cummDeath <- select(cummDeath, "DataDate","Data")
        currInf <- select(currInf, "DataDate","Data")
        cummInf <- select(cummInf, "DataDate", "Data")
        
        colnames(cummDeath)[2] <- "Projected Cumulative Deaths"
        colnames(currInf)[2] <- "Projected Daily Infections"
        colnames(cummInf)[2] <- "Projected Cumulative Infections"
        
        df <- merge(cummDeath, currInf, by="DataDate")
        df <- merge(df, cummInf, by="DataDate")
        
        Chart2DataSub <- melt(data.table(df), id=c("DataDate"))
        
        #Plotting the Line Graph
        p <- ggplot(Chart2DataSub) + 
            geom_line(aes(x=DataDate,  y=value, colour = variable, linetype = variable), 
                      size = 0.5) +
            scale_colour_manual(values=c("Blue", "Orange", "Red", "Black"))+
            scale_linetype_manual(values=c("dashed", "solid", "solid", "solid"))+
            geom_vline(aes(xintercept = as.numeric(lubridate::ymd(Sys.Date())), linetype = "Current Day"), color = "Black") +
            xlab('Date') +
            ylab('Number of People') +
            theme_bw() + 
           theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
                  axis.title = element_text(face = "bold", size = 11, family = "sans"),
                  axis.text.x = element_text(angle = 60, hjust = 1), 
                  axis.line = element_line(color = "black"),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),) +
           scale_x_date(date_breaks = "1 week") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
        
        p2 <- ggplotly(p)
        p2 <- p2 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                          xanchor = "center",
                                          x = 0.5,
                                          y = -0.5
                                          )) %>% config(displayModeBar = FALSE)
        p2 <- p2 %>% layout(xaxis = list(showgrid = F),
                            yaxis = list(gridcolor = "lightgray"),margin = list(t = 50), title=baseUsed) %>% config(displayModeBar = FALSE)
        p2
        
    })
    
    
    output$ProjPeakInfDate<-renderValueBox({
        
        baseUsed = input$Base

        df <- AMC_model
        
        datePeak <- tryCatch({
        
            df <- select(df, "DataDate", "DataType", baseUsed)
            
            colnames(df)[3]  <- "Data"
            
            myTibble <- as_tibble(df)
            
            currInf <- myTibble %>% filter(DataType == "Current Infections")
            
            datePeak = format(currInf$DataDate[which.max(currInf$Data)], format = "%B %d")
            
            }, error = function(err) {
                datePeak = "No Model Data Available"
                return(datePeak)
            })

        valueBox(subtitle = "Projected Peak Infection Date",
                     paste(datePeak),
                     color = "light-blue")
    })
    
    output$ProjTotInf<-renderValueBox({
        
        baseUsed = input$Base
        
        df <- AMC_model
        
        InfTot <- tryCatch({
            
            df <- select(df, "DataDate", "DataType", baseUsed)
            
            colnames(df)[3]  <- "Data"
            
            myTibble <- as_tibble(df)
            
            cummInf <- myTibble %>% filter(DataType == "Cumulative Infections")
            
            InfTot = round(max(cummInf$Data))
            
            }, error = function(err) {
                InfTot = "No Model Data Available"
                return(InfTot)
        })
        

        
        valueBox(subtitle = "Projected Total Infections",
                 paste(InfTot),
                 color = "blue")
    })
    
    output$ProjTotDeaths<-renderValueBox({
        
        baseUsed = input$Base
        
        df <- AMC_model
        
        DeathsTot <- tryCatch({
        
            df <- select(df, "DataDate", "DataType", baseUsed)
            
            colnames(df)[3]  <- "Data"
            
            myTibble <- as_tibble(df)
            
            cummDeath <- myTibble %>% filter(DataType == "Cumulative Deaths")
            
            DeathsTot = round(max(cummDeath$Data))
        
        }, error = function(err) {
            x = "No Model Data Available"
            return(x)
        })
        
        valueBox(subtitle = "Projected Total Fatalities",
                 paste(DeathsTot),
                 color = "navy")
    })

    # #Create IHME plot by State projected hospitalization 
    # output$IHME_State_Hosp<-renderPlotly({
    # 
    #     IncludedHospitals<-GetHospitals(input$Base, input$Radius)
    #     MyCounties <- GetCounties(input$Base, input$Radius)
    #     IHMELocalProjections(MyCounties, IncludedHospitals, input$Base, input$StatisticType, input$proj_days)
    #     
    #     
    # })
    # 
    # 
    # #Output the SEIAR CHIME projections with a max, min, and expected value
    # output$SEIARProjection<-renderPlotly({
    #     BaseState<-dplyr::filter(AFBaseLocations, Base == input$Base)
    #     IncludedCounties<-GetCounties(input$Base,input$Radius)
    #     if (is.null(input$SocialDistanceValue) ){social_dist<-1}
    # 
    #     CS      <- "CS"       %in% input$SocialDistanceValue
    #     CB    <- "CB"     %in% input$SocialDistanceValue
    #     SD <- "SD"  %in% input$SocialDistanceValue
    # 
    #     if (CS & CB & SD){
    #         social_dist <- 27
    #     } else if (CS & CB){
    #         social_dist <- 12
    #     } else if (CS & SD){
    #         social_dist <-19
    #     } else if (SD & CB){
    #         social_dist <-23
    #     } else if (CS) {
    #         social_dist <- 4
    #     }  else if (CB) {
    #         social_dist <- 8
    #     }  else if (SD) {
    #         social_dist <- 15
    #     }
    #     
    #     CHIMELocalPlot(social_dist, input$proj_days, IncludedCounties, input$StatisticType)
    # 
    # })
    
    output$CHIMENationalProj<-renderPlotly({
        
        if (is.null(input$SocialDistanceValueNational) ){social_dist_national<-1}
        
        CSN      <- "CSN"       %in% input$SocialDistanceValueNational
        CBN    <- "CBN"     %in% input$SocialDistanceValueNational
        SDN <- "SDN"  %in% input$SocialDistanceValueNational
        
        if (CSN & CBN & SDN){
            social_dist_national <- 27
        } else if (CSN & CBN){
            social_dist_national <- 12
        } else if (CSN & SDN){
            social_dist_national <-19
        } else if (SDN & CBN){
            social_dist_national <-23
        } else if (CSN) {
            social_dist_national <- 4
        }  else if (CBN) {
            social_dist_national <- 8
        }  else if (SDN) {
            social_dist_national <- 15
        }
        CHIMENationalPlot(social_dist_national, input$proj_days_national)
    })
    
    output$NationalPlotOverlay<-renderPlotly({
        if (is.null(input$SocialDistanceValueNational) ){social_dist_national<-1}
        
        CSN      <- "CSN"       %in% input$SocialDistanceValueNational
        CBN    <- "CBN"     %in% input$SocialDistanceValueNational
        SDN <- "SDN"  %in% input$SocialDistanceValueNational
        
        if (CSN & CBN & SDN){
            social_dist_national <- 27
        } else if (CSN & CBN){
            social_dist_national <- 12
        } else if (CSN & SDN){
            social_dist_national <-19
        } else if (SDN & CBN){
            social_dist_national <-23
        } else if (CSN) {
            social_dist_national <- 4
        }  else if (CBN) {
            social_dist_national <- 8
        }  else if (SDN) {
            social_dist_national <- 15
        }
        NationalOverlayPlot(social_dist_national, input$proj_days_national)
    })
    
    output$IHMENationaProj<-renderPlotly({
        
        IHMENationalProjections(input$proj_days_national) 
    })
    
    observe({
        if(input$selectall == 0) return(NULL) 
        else if (input$selectall%%2 == 0)
        {
            updateCheckboxGroupInput(session,"ModelSelectionValue","Forecasting Model(s): ",choices=c("IHME"="IHME","LANL"="LANL","CHIME SC"="CHIME1","CHIME NE"="CHIME2","CHIME SC+NE"="CHIME3",
                                                                                                      "CHIME SD"="CHIME4","CHIME SC+SD"="CHIME5","CHIME NE+SD"="CHIME6","CHIME SC+NE+SD"="CHIME7",                                                                                                                                
                                                                                                      "Columbia No Intervetion"="CUNI","Columbia 20% SC Reduction"="CU20SC","Columbia 30% SC Reduction"="CU30SC",
                                                                                                      "Columbia 40% SC Reduction"="CU40SC"))
        }
        else
        {
            updateCheckboxGroupInput(session,"ModelSelectionValue","Forecasting Model(s):",choices=c("IHME"="IHME","LANL"="LANL","CHIME SC"="CHIME1","CHIME NE"="CHIME2","CHIME SC+NE"="CHIME3",
                                                                                                     "CHIME SD"="CHIME4","CHIME SC+SD"="CHIME5","CHIME NE+SD"="CHIME6","CHIME SC+NE+SD"="CHIME7",                                                                                                                                
                                                                                                     "Columbia No Intervetion"="CUNI","Columbia 20% SC Reduction"="CU20SC","Columbia 30% SC Reduction"="CU30SC",
                                                                                                     "Columbia 40% SC Reduction"="CU40SC"),
                                     selected=c("IHME"="IHME","LANL"="LANL","CHIME SC"="CHIME1","CHIME NE"="CHIME2","CHIME SC+NE"="CHIME3",
                                                "CHIME SD"="CHIME4","CHIME SC+SD"="CHIME5","CHIME NE+SD"="CHIME6","CHIME SC+NE+SD"="CHIME7",                                                                                                                                
                                                "Columbia No Intervetion"="CUNI","Columbia 20% SC Reduction"="CU20SC",
                                                "Columbia 30% SC Reduction"="CU30SC","Columbia 40% SC Reduction"="CU40SC"))
        }
    })
    
    
    #Overlay Projected Plots
    output$OverlayPlots<-renderPlotly({
        #if (is.null(input$SocialDistanceValue)){social_dist<-1}
        #(4,8,12,15,19,23,27)
        ModelID<-"Past Data"
        
        if ("IHME" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"IHME")}
        if ("LANL" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"LANL")}
        if ("CHIME1" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CHIME_4%_SD")}
        if ("CHIME2" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CHIME_8%_SD")}
        if ("CHIME3" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CHIME_12%_SD")}
        if ("CHIME4" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CHIME_15%_SD")}
        if ("CHIME5" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CHIME_19%_SD")}
        if ("CHIME6" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CHIME_23%_SD")}
        if ("CHIME7" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CHIME_27%_SD")}                                                                                                                                
        if ("CUNI" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CU_No Intervention")}
        if ("CU20SC" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CU_20%_SD")}
        if ("CU30SC" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CU_30%_SD")}
        if ("CU40SC" %in% input$ModelSelectionValue){ModelID<-cbind(ModelID,"CU_40%_SD")}
        
        # CS  <- "CS" %in% input$SocialDistanceValue
        # CB  <- "CB" %in% input$SocialDistanceValue
        # SD  <- "SD" %in% input$SocialDistanceValue
        # 
        # if (CS & CB & SD){
        #     social_dist <- 27
        # } else if (CS & CB){
        #     social_dist <- 12
        # } else if (CS & SD){
        #     social_dist <-19
        # } else if (SD & CB){
        #     social_dist <-23
        # } else if (CS) {
        #     social_dist <- 4
        # }  else if (CB) {
        #     social_dist <- 8
        # }  else if (SD) {
        #     social_dist <- 15
        # }
        MyCounties<-GetCounties(input$Base,input$Radius)
        MyHospitals<-GetHospitals(input$Base,input$Radius)
        PlotOverlay(input$Base, MyCounties, MyHospitals,social_dist,ModelID,input$proj_days, input$StatisticType)
    })
    
    
    # Output any data tables ------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Render National Data Table on summary page
    output$NationalDataTable1<-DT::renderDataTable({
        NationalDataTable <- DT::datatable(data.frame(NationalDataTable),rownames = FALSE, options = list(dom = 'ft',ordering = F,"pageLength" = 51))
        NationalDataTable
    })
    
    output$CountyDataTable1<-DT::renderDataTable({
        MyCounties<-GetCounties(input$Base,input$Radius)
        dt<-GetLocalDataTable(MyCounties)
        dt<-DT::datatable(dt, rownames = FALSE, options = list(dom = 't',ordering = F, "pageLength"=100))
        dt
    })
    
    
    output$ForecastDataTable<-DT::renderDataTable({
        if (input$MAJCOMInput == "All") {
            if(input$SummaryStatistic == "Cases") {
                ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast)
                dt<-DT::datatable(ForecastDataTableCases, rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))   
                dt
            } else {
                ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast)
                dt<-DT::datatable(ForecastDataTable, rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))
                dt
            }
        } else if(input$MAJCOMInput=="Active Duty"){
            if(input$SummaryStatistic == "Cases") {
                ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast)
                dt<-DT::datatable(filter(ForecastDataTableCases, !MAJCOM %in% c("AFRC","ANG")), rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))
                dt
            } else {
                ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast)
                dt<-DT::datatable(filter(ForecastDataTable, !MAJCOM %in% c("AFRC","ANG")), rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))
                dt
            }
        }
        else {
            if(input$SummaryStatistic == "Cases") {
                ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast)
                dt<-DT::datatable(filter(ForecastDataTableCases, MAJCOM == input$MAJCOMInput), rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))
                dt
            } else {
                ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast)
                dt<-DT::datatable(filter(ForecastDataTable, MAJCOM == input$MAJCOMInput), rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))
                dt
            }
        }
    })
    
    
    

    
    
    output$HotSpotData <- downloadHandler(
        filename = function() { 
            paste("HotspotDataset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(Top15Report, file)
            
        })
    
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste("SummaryDataset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(ForecastDataTable, file)
            
        })
    
    output$HotSpot <- renderPlot({
            
        HotspotPlot(CovidConfirmedCases, CovidDeaths,input$MAJCOMInput)
    })
    
    # Output Report ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = function(){
            paste0('CHAD_report(',paste(Sys.Date(),sep = '_'),')','.html')
        },
        content = function(file) {
            
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            
            # tempReport <- file.path(tempdir(), "TestReport.Rmd")
            # file.copy("TestReport.Rmd", tempReport, overwrite = TRUE)
            
            # src <- normalizePath("TestReport2.Rmd")
            # owd <- setwd(tempdir())
            # on.exit(setwd(owd))
            # file.copy(src, "TestReport2.Rmd", overwrite = TRUE)
            # out <- render("TestReport2.Rmd", html_document())
            # file.rename(out, file)
            
            # # Set up parameters to pass to Rmd document
            params <- list(radius = input$Radius,
                           base = input$Base,
                           pjDays = input$proj_days,
                           socDis = input$SocialDistanceValue)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render("www/7_other_resources/TestReport.Rmd", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
                              )
            
        }
    )
    
    
    
    
    # Step Three
    ###################################################################################################################################################
    
    #Step three provides input information for annotation of the overall app such as inputs, sources, and calculations.
    observeEvent(input$overviewInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "OVERVIEW",
                OverviewLink)
        )
    })
    
    observeEvent(input$inputInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "USER INPUTS",
                InfoLink)
        )
    })
    observeEvent(input$projInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "PROJECTIONS",
                ProjLink)
        )
    })
    
    observeEvent(input$calcInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "CALCULATIONS",
                CalcLink)
        )
    })
    
    observeEvent(input$sourceInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "SOURCES",
                SourceLink)
        )
    })
    
    
    




    
    
}
