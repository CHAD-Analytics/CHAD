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
# Test of git commit

# Define server logic, within this all ouputs and reactive variables are generated. 
server <- function(input, output,session) {
  
  # Step One
  ###################################################################################################################################################
  
  addedCounties<-reactiveVal(value = NULL)
  deletedCounties<-reactiveVal(value=NULL)
  
  # observeEvent(input$BranchP, {
  #   addedCounties(NULL)
  #   deletedCounties(NULL)
  # })
  # 
  # observeEvent(input$OperationalInputP, {
  #     addedCounties(NULL)
  #     deletedCounties(NULL)
  # })
  # observeEvent(input$Base, {
  #     addedCounties(NULL)
  #     deletedCounties(NULL)
  # })
  # observeEvent(input$Radius, {
  #   addedCounties(NULL)
  #   deletedCounties(NULL)
  # })
  # 
  # observeEvent(event_data("plotly_click", source = "TEST"), {
  #   county_clicked <- event_data("plotly_click", source = "TEST")
  #   #df<-GetCounties(input$Base,input$Radius, NULL, NULL)
  #   df<-MyCounties()
  #   newCounty<-getNewCounty(df,input$Base, as.integer(county_clicked$curveNumber))
  #   # print("Just Clicked Something")
  #   # print(newCounty[[1]])
  #   # print(addedCounties())
  #   # print(deletedCounties())
  #   if(is.null(addedCounties())){ #do nothing
  #   }else{ # see if new county is in addedcounties
  #     myFIPS<-newCounty[[1]][["FIPS"]]
  #     myRow<-which(addedCounties() == myFIPS, arr.ind = TRUE)#which(grepl(myFIPS, deletedCounties()))
  #     if(is_empty(myRow)){
  #     }else{
  #       tempcounty<-addedCounties()
  #       tempcounty<-tempcounty[-c(myRow[1,1]),] #remove it from addedCounties
  #       if(nrow(tempcounty)==0){addedCounties(NULL)}else{addedCounties(tempcounty)}            }
  #   }
  #   if(is.null(deletedCounties())){ #do nothing
  #   }else{ # see if new county is in addedcounties
  #     myFIPS<-newCounty[[1]][["FIPS"]]
  #     myRow<-which(deletedCounties() == myFIPS, arr.ind = TRUE)#which(grepl(myFIPS, deletedCounties()))
  #     if(is_empty(myRow)){
  #     }else{
  #       tempcounty<-deletedCounties()
  #       tempcounty<-tempcounty[-c(myRow[1,1]),] #remove it from addedCounties
  #       if(nrow(tempcounty)==0){deletedCounties(NULL)}else{deletedCounties(tempcounty)}
  #     }
  #   }
  #   # print("Removed Clicked Something")
  #   # print(addedCounties())
  #   # print(deletedCounties())
  #   if(newCounty[[2]]==1){
  #     if(is.null(addedCounties())){
  #       addedCounties(newCounty[[1]])
  #     }else{
  #       tempcounty<-rbind(addedCounties(),newCounty[[1]])
  #       addedCounties(tempcounty)
  #     }
  #     
  #     #     #tempcounty<-subset(CountyInfo, FIPS %in% newCounty[[1]])
  #     #     if(is.null(deletedCounties())){
  #     #     }else{
  #     #         oldDeleted<-deletedCounties()
  #     #         myFIPS<-newCounty[[1]][["FIPS"]]
  #     #         print(myFIPS)
  #     #         myFIPS<-which(deletedCounties() == myFIPS, arr.ind = TRUE)#which(grepl(myFIPS, deletedCounties()))
  #     #         print(myFIPS)
  #     #         if(is_empty(myFIPS)){}else{
  #     #             oldDeleted<-oldDeleted[-c(myFIPS[1,1]),]
  #     #             print(oldDeleted)
  #     #         }
  #     #         deletedCounties(oldDeleted)
  #     #     }
  #     #     addedCounties(newCounty[[1]])
  #     # }else{
  #     #     if(is.null(deletedCounties())){
  #     #     }else{
  #     #         oldDeleted<-deletedCounties()
  #     #         myFIPS<-newCounty[[1]][["FIPS"]]
  #     #         print(myFIPS)
  #     #         myFIPS<-which(deletedCounties() == myFIPS, arr.ind = TRUE)#which(grepl(myFIPS, deletedCounties()))
  #     #         print(myFIPS)
  #     #         if(is_empty(myFIPS)){}else{
  #     #             oldDeleted<-oldDeleted[-c(myFIPS[1,1]),]
  #     #             print(oldDeleted)
  #     #         }
  #     #         deletedCounties(oldDeleted)
  #     #     }
  #     #}
  #   }else{
  #     checkAdded(newCounty[[1]],addedCounties(), deletedCounties())
  #     if(is.null(deletedCounties())){
  #       deletedCounties(newCounty[[1]])
  #     }else{
  #       tempcounty<-rbind(deletedCounties(),newCounty[[1]])
  #       deletedCounties(tempcounty)
  #     }
  #     # if(is.null(deletedCounties())){
  #     #    # tempcounty<-subset(CountyInfo, FIPS %in% newCounty[[1]])
  #     #     deletedCounties(newCounty[[1]])
  #     # }else{
  #     #     if(is.null(addedCounties())){
  #     #     }else{
  #     #         oldAdded<-addedCounties()
  #     #         myFIPS<-newCounty[[1]][["FIPS"]]
  #     #         myFIPS<-which(addedCounties() == myFIPS, arr.ind = TRUE) #which(grepl(myFIPS, addedCounties()))
  #     #         if(is_empty(myFIPS)){}else{
  #     #             oldAdded<-oldAdded[-c(myFIPS[1,1]),]
  #     #         }
  #     #         addedCounties(oldAdded)
  #     #     }
  #     #     tempcounty<-rbind(deletedCounties(), newCounty[[1]])
  #     #     deletedCounties(tempcounty)
  #     # }
  #   }
  #   # print("added or removed Clicked Something")
  #   # print(newCounty[[2]])
  #   # print(addedCounties())
  #   # print(deletedCounties())
  # })  
  # 
  
  # Step Two
  ###################################################################################################################################################
  MyCounties<-reactive({
    GetCounties(input$Base,input$Radius, addedCounties(), deletedCounties())    
  })
  
  # Output common statistics -------------------------------------------------------------------------------------------------------------------------------------------
  
  #Finds which counties in given radius. Also Give county statistics
  output$TotalPopulation <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    valueBox(subtitle = "Total Regional Population",
             comma(CalculateCounties(MyCounties())),
             #icon = icon("list-ol"),
             color = "light-blue"
    )
    
  })
  
  # Finds Covid Cases and statistics on covid per county
  output$CovidCases <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    valueBox(subtitle = "Total Confirmed Cases",
             comma(CalculateCovid(MyCounties())),
             #icon = icon("list-ol"),
             color = "light-blue"
    )
    
  })
  
  # Finds Covid Cases per 1,000
  output$CasesPer1000 <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    valueBox(subtitle = "Total Confirmed Cases per 1,000",
             comma(CalculateCovid1000(MyCounties())),
             #icon = icon("list-ol"),
             color = "teal"
    )
    
  })
  
  #Outputs change in covid cases per day
  output$CaseChangeLocal <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    df<-MyCounties()
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% df$FIPS)
    changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
    
    valueBox(paste("+",toString(changeC)),
             subtitle = "New Confirmed Cases", 
             color = "light-blue")
  })
  
  
  # Finds Covid deaths and statistics on covid per county
  output$LocalCovidDeaths <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    valueBox(subtitle = "Total Fatalities",
             comma(CalculateDeaths(MyCounties())),
             #icon = icon("skull"),
             color = "light-blue"
    )
  })
  
  #Outputs change in deaths per day   
  output$DeathChangeLocal <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    df<-MyCounties()
    CovidCounties<-subset(CovidDeaths, CountyFIPS %in% df$FIPS)
    changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
    
    valueBox(paste("+",toString(changeC)),
             subtitle = "New Fatalities", 
             color = "light-blue")
  })
  
  #Finds hospital information within a given 100 mile radius. Calculates number of total hospital beds. Can compare to number of cases
  output$HospitalUtilization <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    valueBox(subtitle = "Estimated Local Hospital Bed Utilization",
             HospitalIncreases(MyCounties()),
             #icon = icon("hospital"),
             color = "navy")
  })
  
  
  output$CaseDbRate <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    valueBox(paste(CaseDblRate(MyCounties()),"days"),
             subtitle = "Case Doubling Rate",
             color = "teal")
  })
  
  
  output$Rt_Estimate <- renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    valueBox(paste(Estimate_Rt(MyCounties())),
             subtitle = "Estimated Virus Reproduction Rate",
             color = "navy")
  })
  
  
  ###################################################################################################
  
  output$CHIMEPeakDate<-renderValueBox({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    if (is.null(input$SocialDistanceValue) ){social_dist<-1}
    
    CS <- "CS"  %in% input$SocialDistanceValue
    CB <- "CB"  %in% input$SocialDistanceValue
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
    Peak<-CalculateCHIMEPeak(MyCounties(), input$Base, input$Radius, social_dist, input$proj_days, input$StatisticType)
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
    
    #MyCounties <-GetCounties(input$Base,input$Radius)
    DailyChart <- CovidCasesPerDayChart(MyCounties())
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
                                                    y = 1.2)) #%>% config(displayModeBar = FALSE)
    plotDaily
  })
  
  
  #Create local health plot for Daily Cases 
  output$LocalHealthPlot3day<-renderPlotly({
    
    #MyCounties<-GetCounties(input$Base,input$Radius)
    DailyChart <- CovidCasesPer3DayAverageChart(MyCounties())
    DailyChart <- dplyr::filter(DailyChart, ForecastDate >= DailyChart$ForecastDate[1] + 35)
    
    plotDaily <- ggplot(DailyChart) + 
      geom_col(aes(x=ForecastDate, y=value, colour = variable), size = 0.5) +
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
                                                    y = 1.2)) #%>% config(displayModeBar = FALSE)
    plotDaily
  })
  
  
  output$LocalHealthPlotWeeklyGrowth<-renderPlotly({
    
    #MyCounties<-GetCounties(input$Base,input$Radius)
    DailyChart <- CovidCasesWeeklyGrowth(MyCounties())
    
    plotDaily <- ggplot(DailyChart) + 
      geom_col(aes(x=`Forecast Date`, 
                   y=value, 
                   fill = ifelse(value>0, 
                                 "Growth Increase", 
                                 "Growth Decrease")), 
               size = 0.5) +
      scale_fill_manual(values=c("Green", "Red"),
                        name = "Case Growth") +
      geom_hline(aes(yintercept = 0),
                 colour = "black",
                 linetype = "dashed") +
      xlab('Date') +
      ylab('Weekly Growth Rate') +
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
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      #ylim(0,max(Chart1DataSub$value) * 1.1) +
      labs(color='')
    
    plotDaily <- ggplotly(plotDaily)
    plotDaily <- plotDaily %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                                    xanchor = "center",  # use center of legend as anchor
                                                    x = 0.5,
                                                    y = 1.2)) #%>% config(displayModeBar = FALSE)
    plotDaily
  })
  
  
  #Create Plot of Total Cases
  output$LocalHealthPlot2<-renderPlotly({
    
    #MyCounties<-GetCounties(input$Base,input$Radius)
    CumulChart <- CovidCasesCumChart(MyCounties())
    CumulChart <- dplyr::filter(CumulChart, ForecastDate >= CumulChart$ForecastDate[1] + 35)
    
    #Plot for local area cumulative cases
    plotTot <- ggplot(CumulChart) + 
      geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 0.5) +
      scale_colour_manual(values=c("Blue", "Red", "Green"))+
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
                                                y = 1.2)) #%>% config(displayModeBar = FALSE)
    plotTot
  })
  
  
  
  # Output Choropleth Charts ----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  #Create Country Plot on Summary page
  output$SummaryPlot<-renderGvis({
    
    USlist = list(region="US",
                  displayMode = "regions",
                  resolution = "provinces",
                  colors="['#52E74B','blue','#85050a']", #green to dark red
                  width=1200,
                  height = 600,
                  if (input$MapScale == "Log"){
                    title='National Impact Map - Log Scale of Cases'
                  } else {
                    title='National Impact Map - Case Count'
                  },
                  legendPosition='bottom'
    ) 

    NAlist = list(region="021",
                    displayMode = "regions",
                    colors="['#52E74B','blue','#85050a']", #green to dark red
                    width=1200,
                    height = 600,
                    if (input$MapScale == "Log"){
                      title='North America Impact Map - Log Scale of Cases'
                    } else {
                      title='North America Impact Map - Case Count'
                    },
                    legendPosition='bottom'
    )    
    CAlist = list(region="013",
                  displayMode = "regions",
                  colors="['#52E74B','blue','#85050a']", #green to dark red
                  width=1200,
                  height = 600,
                  if (input$MapScale == "Log"){
                    title='central America Impact Map - Log Scale of Cases'
                  } else {
                    title='Central America Impact Map - Case Count'
                  },
                  legendPosition='bottom'
    )        
    SAlist = list(region="005",
                    displayMode = "regions",
                    colors="['#52E74B','blue','#85050a']", #green to dark red
                    width=1200,
                    height = 600,
                    if (input$MapScale == "Log"){
                      title='South America Impact Map - Log Scale of Cases'
                    } else {
                      title='South America Impact Map - Case Count'
                    },
                    legendPosition='bottom'
    )    
    EUROlist = list(region="150",
                    displayMode = "regions",
                    colors="['#52E74B','blue','#85050a']", #green to dark red
                    width=1200,
                    height = 600,
                    if (input$MapScale == "Log"){
                      title='Europe Impact Map - Log Scale of Cases'
                    } else {
                      title='Europe Impact Map - Case Count'
                    },
                    legendPosition='bottom'
    )
    MElist = list(region="145",
                    displayMode = "regions",
                    colors="['#52E74B','blue','#85050a']", #green to dark red
                    width=1200,
                    height = 600,
                    if (input$MapScale == "Log"){
                      title='Middle East Impact Map - Log Scale of Cases'
                    } else {
                      title='Middle East Impact Map - Case Count'
                    },
                    legendPosition='bottom'
    )    
    AFRICAlist = list(region="002",
                    displayMode = "regions",
                    colors="['#52E74B','blue','#85050a']", #green to dark red
                    width=1200,
                    height = 600,
                    if (input$MapScale == "Log"){
                      title='Europe Impact Map - Log Scale of Cases'
                    } else {
                      title='Europe Impact Map - Case Count'
                    },
                    legendPosition='bottom'
    )    
    
    ASIAlist = list(region="142",
                    displayMode = "regions",
                    colors="['#52E74B','blue','#85050a']", #green to dark red
                    width=1200,
                    height = 600,
                    if (input$MapScale == "Log"){
                      title='Asia Impact Map - Log Scale of Cases'
                    } else {
                      title='Asia Impact Map - Case Count'
                    },
                    legendPosition='bottom'
    )
    OCEANIAlist = list(region="009",
                     displayMode = "province",
                     colors="['#52E74B','blue','#85050a']", #green to dark red
                     width=1200,
                     height = 600,
                     if (input$MapScale == "Log"){
                       title='Oceania Impact Map - Log Scale of Cases'
                     } else {
                       title='Oceania Impact Map - Case Count'
                     },
                     legendPosition='bottom'
    )    
    WORLDlist = list(region="world",
                    displayMode = "province",
                    colors="['#52E74B','blue','#85050a']", #green to dark red
                    width=1200,
                    height = 600,
                    if (input$MapScale == "Log"){
                      title='Global Impact Map - Log Scale of Cases'
                    } else {
                      title='Global Impact Map - Case Count'
                    },
                    legendPosition='bottom'
    )
    
    if (input$MapView == "Europe"){
      MapChoice = EUROlist
      MapFilter = "Europe"
    }else if (input$MapView == "Asia"){
      MapChoice = ASIAlist
      MapFilter = "Asia"
    }else if (input$MapView == "US"){
      MapChoice = USlist
      MapFilter = "North America"
    # }else if (input$MapView == "Middle East"){
    #   MapChoice = MElist
    #   MapFilter = "Middle East"      
    # }else if (input$MapView == "Central America"){
    #   MapChoice = CAlist
    #   MapFilter = "Central America"
    # }else if (input$MapView == "South America"){
    #   MapChoice = SAlist
    #   MapFilter = "South America"
    # }else if (input$MapView == "North America"){
    #   MapChoice = NAlist
    #   MapFilter = "North America"
    }else if (input$MapView == "Africa"){
      MapChoice = AFRICAlist
      MapFilter = "Africa"
    }else if (input$MapView == "Oceania"){
      MapChoice = OCEANIAlist
      MapFilter = "Oceania"      
    }else {
      MapChoice = WORLDlist
      MapFilter = "World"      
    }
    
    # if (MapFilter != "World"){
    #   DF<-dplyr::filter(ContinentMap, Continent == MapFilter)
    # } else {
    #   DF<-ContinentMap
    #   select <- DF$Continent == "North America"
    #   DF$State[select] <- "United States"
    # }
    # DF<-cbind.data.frame(DF$State, rev(DF)[,1], rev(DF)[,1])
    # colnames(DF)<-c("state","Value","LogValue")
    # ChlorData<-plyr::ddply(DF, "state", numcolwise(sum))
    # if (input$MapScale == "Log"){ChlorData<-transform(ChlorData, LogValue = round(log(LogValue, base=10),digits = 1))}
    # ChlorData <- transform(ChlorData, Value = as.character(format(Value,big.mark=",")))
    # ChlorData<-ChlorData %>%
    #   mutate(state_name = state.name[match(state, state.abb)])
    # ChlorData$state_name <- ifelse(is.na(ChlorData$state_name), as.character(ChlorData$state), ChlorData$state_name)
    # #ChlorData<-ChlorData[complete.cases(ChlorData$state_name), ]
    # ChlorData <- transform(ChlorData, Value =paste(state_name, " Total Cases: ", Value))
    # states <- data.frame(ChlorData$state_name, ChlorData$Value, ChlorData$LogValue)
    # colnames(states)<-c("state_name","Cases","StateColor")
    # states$StateColor = ifelse(is.infinite(states$StateColor), 0, states$StateColor)
    # g = gvisGeoChart(states, locationvar = "state_name", hovervar = "Cases", colorvar = "StateColor", 
    #                  options = MapChoice
    # )
    
    # New code block to change cases to cases per 100K and add in multiple display values when hovering
    if (MapFilter != "World"){
       DF<-dplyr::filter(ContinentMap, Continent == MapFilter)
       DF<-cbind.data.frame(DF$State, rev(DF)[,1], rev(DF)[,1])
       colnames(DF)<-c("state","Value","LogValue")
       DF<-plyr::ddply(DF, "state", numcolwise(sum))
       CF<-data.frame(CountyInfo$State,CountyInfo$Population)
       CF<-plyr::ddply(CF,"CountyInfo.State", numcolwise(sum)) 
       CD <- merge(DF,CF,by.x = names(DF)[1],by.y = names(CF)[1]) 
       colnames(CD)[4]<-"Population"
       ChlorData<-data.frame(CD$state,CD$Value,CD$Value,CD$LogValue,CD$Population) 
       colnames(ChlorData)<-c("State","Value","CasesPer100K","LogValue","Population") 
    # } else if (MapFilter == "North America") {
    #   DF<-ContinentMap
    #   DF<-cbind.data.frame(DF$State,DF$Continent, rev(DF)[,1], rev(DF)[,1])
    #   colnames(DF)<-c("state","Continent","Value","LogValue")
    #   DF$Continent[is.na(DF$Continent)] <- "North America"
    #   DF<-dplyr::filter(ContinentMap, Continent == MapFilter)
    #   CF<-data.frame(CountyInfo$State,CountyInfo$Population)
    #   CF<-plyr::ddply(CF,"CountyInfo.State", numcolwise(sum)) 
    #   CD <- merge(DF,CF,by.x = names(DF)[1],by.y = names(CF)[1])
    #   colnames(CD)[5]<-"Population"
    #   ChlorData<-data.frame(CD$state,CD$Continent,CD$Value,CD$Value,CD$LogValue,CD$Population,stringsAsFactors=FALSE)
    #   colnames(ChlorData)<-c("State","Continent","Value","CasesPer100K","LogValue","Population") 
    #   ChlorData$State <- as.character(ChlorData$State)
    #   select <- which(ChlorData$Continent == "North America")
    #   ChlorData$State[select]<-"United States"
    #   ChlorData<-plyr::ddply(ChlorData, "State", numcolwise(sum))
    } else if (MapFilter == "World") {
       DF<-ContinentMap
       DF<-cbind.data.frame(DF$State,DF$Continent, rev(DF)[,1], rev(DF)[,1])
       colnames(DF)<-c("state","Continent","Value","LogValue")
       DF$Continent[is.na(DF$Continent)] <- "North America"
       CF<-data.frame(CountyInfo$State,CountyInfo$Population)
       CF<-plyr::ddply(CF,"CountyInfo.State", numcolwise(sum)) 
       CD <- merge(DF,CF,by.x = names(DF)[1],by.y = names(CF)[1])
       colnames(CD)[5]<-"Population"
       ChlorData<-data.frame(CD$state,CD$Continent,CD$Value,CD$Value,CD$LogValue,CD$Population,stringsAsFactors=FALSE)
       colnames(ChlorData)<-c("State","Continent","Value","CasesPer100K","LogValue","Population") 
       ChlorData$State <- as.character(ChlorData$State)
       select <- which(ChlorData$Continent == "North America")
       ChlorData$State[select]<-"United States"
       ChlorData<-plyr::ddply(ChlorData, "State", numcolwise(sum))
    }
    
    ChlorData$CasesPer100K<-round(ChlorData$CasesPer100K/ChlorData$Population*100000)
     
    if (input$MapScale == "Log"){ChlorData<-transform(ChlorData, LogValue = round(log(LogValue, base=10),digits = 1))}
    #else{ChlorData<-transform(ChlorData, LogValue = ChlorData$CasesPer100K)} #I don't like what this does to the color scheme
     ChlorData <- transform(ChlorData, Value = as.character(format(Value,big.mark=",")))
     ChlorData<-ChlorData %>%
     mutate(state_name = state.name[match(State, state.abb)])
     ChlorData$state_name <- ifelse(is.na(ChlorData$state_name), as.character(ChlorData$State), ChlorData$state_name)
     #ChlorData<-ChlorData[complete.cases(ChlorData$state_name), ]
     ChlorData <- transform(ChlorData, Value=paste(state_name," -- Total Cases: ",Value))
     ChlorData <- transform(ChlorData, CasesPer100K=paste(" Cases Per 100k: ",CasesPer100K))
     ChlorData <- transform(ChlorData, Population=paste(" Population: ",Population))     
     ChlorData <- transform(ChlorData, Value=paste(Value,'||',CasesPer100K,'||',Population))  #Would prefer a line break, but I tried to use sep='\n' and several other things but nothing worked          
     states <- data.frame(ChlorData$state_name,ChlorData$Value,ChlorData$LogValue)
     colnames(states)<-c("state_name","Cases","State Value")
     states$StateColor = ifelse(is.infinite(states$"State Value"), 0, states$"State Value")
     g = gvisGeoChart(states, locationvar = "state_name", hovervar = c("Cases"), colorvar = "State Value", 
                      options = MapChoice
     )
    
    
  })
  
  
  #Creates the local choropleth charts that change based on which base and radius.
  output$LocalChoroPlot<-renderPlotly({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    p = tryCatch({
      PlotLocalChoro(MyCounties(), input$Base, input$TypeLocal)
    }, error = function(err) {
      empty_plot("Map Unavailable")
    })
    p
  })
  
  #Choice between cases heat map or hospitalizations heat map
  output$SummaryTabChoro<-renderPlotly({
    GetHeatMap(input$Branch,input$OperationalInput,input$MAJCOMNAF,input$MAJCOMInput,input$NAFInput,
               input$WingInput, input$SummaryModelType, input$SummaryForecast, input$SummaryStatistic)
  })
  
  
  
  
  # Output Projections  ---------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Output AMC Analysis
  output$ProjectedEpidemicTable<-renderPlotly({
    
    baseUsed = input$AMClist
    
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
    )) #%>% config(displayModeBar = FALSE)
    p2 <- p2 %>% layout(xaxis = list(showgrid = F),
                        yaxis = list(gridcolor = "lightgray"),margin = list(t = 50), title=baseUsed)# %>% config(displayModeBar = FALSE)
    p2
    
  })
  
  
  output$ProjPeakInfDate<-renderValueBox({
    
    baseUsed = input$AMClist
    
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
    
    baseUsed = input$AMClist
    
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
    
    baseUsed = input$AMClist
    
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
  

  output$HospLine <- renderUI({
    if (input$CONUSP == "CONUS" & input$StatisticType == "Hospitalizations"){
      checkboxGroupInput("RedLine","Hospital Capacity Line ",
                         c("Show Line"="ShowLine"),
                         selected = c(""))
    }
    
  })  
  
  
  observe({
    if(input$selectall1 == 0) return(NULL) 
    else if (input$selectall1%%2 == 0)
    {
      updateCheckboxGroupInput(session,"ModelSelectionValue1","Forecasting Model(s): ",choices=c("IHME (University of Washington)"="IHME",
                                                                                                 "Center for Army Analysis"="CAA",
                                                                                                 "Youyang Gu - Independent (YYG) Model"="YYG",
                                                                                                 "CHIME: SC"="CHIME7",
                                                                                                 "University of Texas"="UT",
                                                                                                 "Columbia University: 20% SC Reduction with weekly 10% increase in contact"="CU20SCw10"))
    }
    else
    {
      updateCheckboxGroupInput(session,"ModelSelectionValue1","Forecasting Model(s):",choices=c("IHME (University of Washington)"="IHME",
                                                                                                "Center for Army Analysis"="CAA",
                                                                                                "Youyang Gu - Independent (YYG) Model"="YYG",
                                                                                                "CHIME: SC"="CHIME7",
                                                                                                "University of Texas"="UT",
                                                                                                "Columbia University: 20% SC Reduction with weekly 10% increase in contact"="CU20SCw10"),                                                                                               
                               
                               selected=c("IHME (University of Washington)"="IHME",
                                          "Center for Army Analysis"="CAA",
                                          "Youyang Gu - Independent (YYG) Model"="YYG",
                                          "CHIME: SC"="CHIME7",
                                          "University of Texas"="UT",
                                          "Columbia University: 20% SC Reduction with weekly 10% increase in contact"="CU20SCw10"))                                                                                    
    }
  })
  
  
  observe({
    if(input$selectall2 == 0) return(NULL) 
    else if (input$selectall2%%2 == 0)
    {
      updateCheckboxGroupInput(session,"ModelSelectionValue2","Forecasting Model(s): ",choices=c("DTRA 1 - Current Response"="DTRA1",
                                                                                                 "DTRA 2 - Improved Response"="DTRA2", 
                                                                                                 "DTRA 3 - Worst Case"="DTRA3",
                                                                                                "CHIME (University of Pennsylvania): SC+NE+SD"="CHIME1",
                                                                                                "CHIME: NE+SD"="CHIME2",
                                                                                                "CHIME: SC+SD"="CHIME3",                                                                
                                                                                                "CHIME: SD"="CHIME4", 
                                                                                                "CHIME: SC+NE"="CHIME5",
                                                                                                "CHIME: NE"="CHIME6",
                                                                                                "Los Alamos National Labs (LANL)"="LANL",
                                                                                                "Columbia University: 20% SC Reduction with one time 10% increase in contact "="CU20SCx10",
                                                                                                "Columbia University: 20% SC Reduction with one time 5% increase in contact"="CU20SCx5",
                                                                                                "Columbia University: 20% SC Reduction with weekly 5% increase in contact"="CU20SCw5"))                                                                                                                                                                                                
    }
    else
    {
      updateCheckboxGroupInput(session,"ModelSelectionValue2","Forecasting Model(s):",choices=c("DTRA 1 - Current Response"="DTRA1",
                                                                                                "DTRA 2 - Improved Response"="DTRA2", 
                                                                                                "DTRA 3 - Worst Case"="DTRA3", 
                                                                                               "CHIME (University of Pennsylvania): SC+NE+SD"="CHIME1",
                                                                                               "CHIME: NE+SD"="CHIME2",
                                                                                               "CHIME: SC+SD"="CHIME3",                                                                
                                                                                               "CHIME: SD"="CHIME4", 
                                                                                               "CHIME: SC+NE"="CHIME5",
                                                                                               "CHIME: NE"="CHIME6",
                                                                                               "Los Alamos National Labs (LANL)"="LANL",
                                                                                               "Columbia University: 20% SC Reduction with one time 10% increase in contact "="CU20SCx10",
                                                                                               "Columbia University: 20% SC Reduction with one time 5% increase in contact"="CU20SCx5",
                                                                                               "Columbia University: 20% SC Reduction with weekly 5% increase in contact"="CU20SCw5"),                                                                                               
                               
                               selected=c("DTRA 1 - Current Response"="DTRA1",
                                          "DTRA 2 - Improved Response"="DTRA2", 
                                          "DTRA 3 - Worst Case"="DTRA3", 
                                          "CHIME (University of Pennsylvania): SC+NE+SD"="CHIME1",
                                          "CHIME: NE+SD"="CHIME2",
                                          "CHIME: SC+SD"="CHIME3",                                                                
                                          "CHIME: SD"="CHIME4", 
                                          "CHIME: SC+NE"="CHIME5",
                                          "CHIME: NE"="CHIME6",
                                          "Los Alamos National Labs (LANL)"="LANL",
                                          "Columbia University: 20% SC Reduction with one time 10% increase in contact "="CU20SCx10",
                                          "Columbia University: 20% SC Reduction with one time 5% increase in contact"="CU20SCx5",
                                          "Columbia University: 20% SC Reduction with weekly 5% increase in contact"="CU20SCw5"))                                                                                    
    }
  })  
  
  
  observe({
    if(input$selectall3 == 0) return(NULL) 
    else if (input$selectall3%%2 == 0)
    {
      updateCheckboxGroupInput(session,"ModelSelectionValue1","Forecasting Model(s): ",choices=c("IHME (University of Washington)"="IHME",
                                                                                                 "Youyang Gu - Independent (YYG) Model"="YYG",
                                                                                                 "CHIME: SC"="CHIME7"))
    }
    else
    {
      updateCheckboxGroupInput(session,"ModelSelectionValue1","Forecasting Model(s):",choices=c("IHME (University of Washington)"="IHME",
                                                                                                "Youyang Gu - Independent (YYG) Model"="YYG",
                                                                                                "CHIME: SC"="CHIME7"),                                                                                               
                               
                               selected=c("IHME (University of Washington)"="IHME",
                                          "Youyang Gu - Independent (YYG) Model"="YYG",
                                          "CHIME: SC"="CHIME7"))                                                                                    
    }
  })
  
  
  observe({
    if(input$selectall4 == 0) return(NULL) 
    else if (input$selectall4%%2 == 0)
    {
      updateCheckboxGroupInput(session,"ModelSelectionValue2","Forecasting Model(s): ",choices=c("CHIME (University of Pennsylvania): SC+NE+SD"="CHIME1",
                                                                                                 "CHIME: NE+SD"="CHIME2",
                                                                                                 "CHIME: SC+SD"="CHIME3",                                                                
                                                                                                 "CHIME: SD"="CHIME4", 
                                                                                                 "CHIME: SC+NE"="CHIME5",
                                                                                                 "CHIME: NE"="CHIME6",
                                                                                                 "Los Alamos National Labs (LANL)"="LANL"))                                                                                                                                                                                                
    }
    else
    {
      updateCheckboxGroupInput(session,"ModelSelectionValue2","Forecasting Model(s):",choices=c("CHIME (University of Pennsylvania): SC+NE+SD"="CHIME1",
                                                                                                "CHIME: NE+SD"="CHIME2",
                                                                                                "CHIME: SC+SD"="CHIME3",                                                                
                                                                                                "CHIME: SD"="CHIME4", 
                                                                                                "CHIME: SC+NE"="CHIME5",
                                                                                                "CHIME: NE"="CHIME6",
                                                                                                "Los Alamos National Labs (LANL)"="LANL"),                                                                                               
                               
                               selected=c("CHIME (University of Pennsylvania): SC+NE+SD"="CHIME1",
                                          "CHIME: NE+SD"="CHIME2",
                                          "CHIME: SC+SD"="CHIME3",                                                                
                                          "CHIME: SD"="CHIME4", 
                                          "CHIME: SC+NE"="CHIME5",
                                          "CHIME: NE"="CHIME6",
                                          "Los Alamos National Labs (LANL)"="LANL"))                                                                                    
    }
  })   
  
  
  #Overlay Projected Plots
  output$OverlayPlots<-renderPlotly({

    #p = tryCatch({
      if (input$CONUSP == "CONUS"){
          #if ("HUtil" %in% input$Utilization){HospUtil<="Yes"}
          ModelID <- "Past Data"
          if ("IHME" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"IHME")}
          if ("CAA" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"CAA")}      
          if ("YYG" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"YYG")}
          if ("DTRA1" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"DTRA1")}
          if ("DTRA2" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"DTRA2")}
          if ("DTRA3" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"DTRA3")}          
          if ("LANL" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"LANL")}
          if ("UT" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"UT")}
          if ("CHIME7" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"CHIME_4%_SD")}
          if ("CHIME6" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_8%_SD")}
          if ("CHIME5" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_12%_SD")}
          if ("CHIME4" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_15%_SD")}
          if ("CHIME3" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_19%_SD")}
          if ("CHIME2" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_23%_SD")}
          if ("CHIME1" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_27%_SD")}
          if ("CU20SCx10" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CU20SCx10")}
          if ("CU20SCx5" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CU20SCx5")}
          if ("CU20SCw10" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"CU20SCw10")}
          if ("CU20SCw5" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CU20SCw5")}
      } else if (input$CONUSP == "OCONUS"){
          ModelID <- "Past Data"
          if ("IHME" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"IHME")}
          if ("YYG" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"YYG")}
          if ("LANL" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"LANL")}
          if ("CHIME7" %in% input$ModelSelectionValue1){ModelID<-cbind(ModelID,"CHIME_4%_SD")}
          if ("CHIME6" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_8%_SD")}
          if ("CHIME5" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_12%_SD")}
          if ("CHIME4" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_15%_SD")}
          if ("CHIME3" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_19%_SD")}
          if ("CHIME2" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_23%_SD")}
          if ("CHIME1" %in% input$ModelSelectionValue2){ModelID<-cbind(ModelID,"CHIME_27%_SD")}
      }
    
      if (is.null(input$RedLine)){
        redline = "No"
      } else{
        redline = input$RedLine
      }
    
      MyHospitals<-GetHospitals(input$Base,input$Radius)
      PlotOverlay(input$Base, MyCounties(), MyHospitals,ModelID,input$proj_days,input$StatisticType,input$CONUSP,redline)
      
      # output$PlotForecastDT<-DT::renderDataTable({
      #   PlotForecastDT <- DT::datatable(PeakValues,rownames = FALSE, options = list(fixedHeader = TRUE, 
      #                                                                               dom = 'ft',
      #                                                                               ordering = F,
      #                                                                               "pageLength" = 250))
      #   PlotForecastDT
      # })
      

    # }, error = function(err) {
    #   #empty_plot(paste(input$Utilization))
    # })
    
  })
  
  
  
  output$helptext <- renderText({"I can trigger a shinyBS::bsModal() from here, but I want to place two buttons behind `Option_1` and     `Option_2`" })
  
  
  # Output any data tables ------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  #Render National Data Table on summary page
  output$NationalDataTable1<-DT::renderDataTable({
    if (input$MapView == "World"){
      NationalDataTable1 <- DT::datatable(NationalDataTable,
                                          rownames = FALSE, 
                                          options = list(order = list(1, "asc"),
                                                         pageLength = 15))
    } else{
    
      NationalDataTable1 = dplyr::filter(NationalDataTable, Continent == input$MapView)
      NationalDataTable1 <- DT::datatable(NationalDataTable1,
                                          rownames = FALSE, 
                                          options = list(order = list(1, "asc"),
                                                         pageLength = 15))
    }
    NationalDataTable1
  })
  
  output$CountyDataTable1<-DT::renderDataTable({
    #MyCounties<-GetCounties(input$Base,input$Radius)
    dt<-GetLocalDataTable(MyCounties())
    dt<-DT::datatable(dt, rownames = FALSE, options = list(dom = 't',
                                                           ordering = F, 
                                                           "pageLength"=100))
    dt
  })
  
  
  ###### Filter installations by branch and operational status
  ###### Filter works for local and projection tabs############
  OperationalListP<- reactive({
    #Once select service, select active, guard, reserve
    OperationalListP <- dplyr::filter(AFBaseLocations,Branch  %in% input$BranchP)
    OperationalListP <- sort(unique(OperationalListP$Operational), decreasing = FALSE)
    OperationalListP <- c(OperationalListP)
  })
  observeEvent(input$BranchP,{updateSelectInput(session,"OperationalInputP",choices = OperationalListP())})  
  
  BaseListP<- reactive({
    #Once select service, select active, guard, reserve
    Bases <- dplyr::filter(AFBaseLocations,Branch %in% input$BranchP)
    Bases <- dplyr::filter(Bases,Overseas %in% input$CONUSP)    
    Bases <- dplyr::filter(Bases,Operational %in% input$OperationalInputP)    
    BaseList <- sort(unique(Bases$Base), decreasing = FALSE)
    BaseList <- c(BaseList)
  })
  observeEvent(input$BranchP,{updateSelectInput(session,"Base",choices = BaseListP())})  
  observeEvent(input$CONUSP,{updateSelectInput(session,"Base",choices = BaseListP())})    
  observeEvent(input$OperationalInputP,{updateSelectInput(session,"Base",choices = BaseListP())})
  ###################################################################
  
  
  ####### Filter MAJCOM Summary Tab###############
  OperationalList<- reactive({
    #Once select service, select active, guard, reserve
    OperationalList <- dplyr::filter(AFBaseLocations,Branch %in% input$Branch)
    OperationalList <- sort(unique(OperationalList$Operational), decreasing = FALSE)
    OperationalList <- c("All",OperationalList)
  })
  observe(updateSelectInput(session,"OperationalInput",choices = OperationalList()))  
  
  ######
  ######  Need to filter NAF and MAJCOM lists by operational status above
  ######
  
  MAJCOMList<- reactive({
    MAJCOMList <- dplyr::filter(AFBaseLocations,Branch %in% input$Branch)
    # MAJCOMList <- dplyr::filter(MAJCOMList,Operational %in% input$OperationalInput) 
    # MAJCOMList <- sort(unique(MAJCOMList$'Major Command'), decreasing = FALSE)
    # MAJCOMList <- c("All",MAJCOMList)
    if (input$OperationalInput == "Active") {
      MAJCOMList <- dplyr::filter(MAJCOMList,Operational %in% input$OperationalInput)         
      MAJCOMList <- sort(unique(MAJCOMList$'Major Command'), decreasing = FALSE)
      MAJCOMList <- c("All",MAJCOMList)
    } else if ((input$OperationalInput == "Reserve")||(input$OperationalInput == "Guard")){
      MAJCOMList <- dplyr::filter(MAJCOMList,Operational %in% input$OperationalInput)         
      MAJCOMList <- sort(unique(MAJCOMList$'Major Command'), decreasing = FALSE)
      MAJCOMList <- c(MAJCOMList)
    }else {
      MAJCOMList <- sort(unique(AFBases$'Major Command'), decreasing = FALSE)
      MAJCOMList<-c("All",MAJCOMList)               
    }   
  })
  observeEvent(input$OperationalInput,{updateSelectInput(session,"MAJCOMInput",choices = MAJCOMList())})  
  
  # NAFList<- reactive({
  #     NAFList <- sort(unique(AFNAFS$NAF), decreasing = FALSE)
  #     NAFList <- c(NAFList)             
  #   }         
  #   
  # })
  # observeEvent(input$OperationalInput,{updateSelectInput(session,"NAFInput",choices = NAFList())})  
  
  WingList<- reactive({
    if (input$NAFInput != "All"){
      #Once add additional NAFS, change NAFList to input$NAFInput
      AFWings<-dplyr::filter(AFNAFS,NAF %in% input$NAFInput)
      WingList <- sort(unique(AFWings$Wing), decreasing = FALSE)
      WingList <- c("All",WingList)
    } else {  
      AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)
      WingList <- sort(unique(AFWings$Wing), decreasing = FALSE)
      WingList <- c("All",WingList)      
    }
  })
  observeEvent(input$NAFInput,{updateSelectInput(session,"WingInput",choices = WingList())})  
  
  
  GroupList <- reactive({
    if (input$NAFInput != "All"){
      if (input$WingInput != "All") {
        AFWings<-dplyr::filter(AFNAFS,NAF %in% input$NAFInput)        
        GroupList<-dplyr::filter(AFWings,Wing %in% input$WingInput)
        GroupList<-sort(unique(GroupList$`Group`), decreasing = FALSE)
        GroupList<-c("All",GroupList)
      }else {
        AFWings<-dplyr::filter(AFNAFS,NAF %in% input$NAFInput) 
        GroupList<-sort(unique(AFWings$`Group`), decreasing = FALSE)
        GroupList<-c("All",GroupList)
      }
    } else {
      if (input$WingInput != "All") {
        AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)        
        GroupList<-dplyr::filter(AFWings,Wing %in% input$WingInput)
        GroupList<-sort(unique(GroupList$`Group`), decreasing = FALSE)
        GroupList<-c("All",GroupList)
      }else {
        AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)  
        GroupList<-sort(unique(AFWings$`Group`), decreasing = FALSE)
        GroupList<-c("All",GroupList)
      }        
    }
    
  })
  observeEvent(input$NAFInput,{updateSelectInput(session,"GroupInput",choices = GroupList())})      
  observeEvent(input$WingInput,{updateSelectInput(session,"GroupInput",choices = GroupList())})          
  
  
  
  #Choice between cases heat map or hospitalizations heat map
  output$SummaryTabChoro<-renderPlotly({
    GetHeatMap(input$Branch,input$OperationalInput,input$MAJCOMNAF,input$MAJCOMInput,input$NAFInput,
               input$WingInput, input$SummaryModelType, input$SummaryForecast, input$SummaryStatistic)
  })
  
  
  # output$HotSpot <- renderPlot({
  #   BaseHotSpot()
  # })
  
  
  output$ForecastDataTableOut<-DT::renderDT({
    forecastbaselist<-dplyr::filter(AFBaseLocations,Branch %in% input$Branch)                        
    forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
    
    if(input$SummaryStatistic == "Cases") {
      FilteredDT<-dplyr::filter(ForecastDataTableCases,Installation %in% forecastbaselist)                        
    } else if (input$SummaryStatistic == "Hospitalizations") {
      FilteredDT<-dplyr::filter(ForecastDataTable,Installation %in% forecastbaselist)                        
    }   
    
    if(input$OperationalInput != "All") {
      forecastbaselist<-dplyr::filter(AFBaseLocations,Operational %in% input$OperationalInput)                        
      forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE)         
      FilteredDT<-dplyr::filter(FilteredDT,Installation %in% forecastbaselist)
    }      
    
    if (input$MAJCOMNAF == "MAJCOM") {
      if (input$MAJCOMInput == "All") {
        FilteredDT<-FilterDataTable(FilteredDT,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
        FTPrint<-FilteredDT
        dt<-DT::datatable(FilteredDT, rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))
        dt
      } else {
        FilteredDT<-FilterDataTable(FilteredDT,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
        FTPrint<-FilteredDT
        dt<-DT::datatable(filter(FilteredDT, MAJCOM == input$MAJCOMInput), rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))
        dt
      }
    } else if (input$MAJCOMNAF == "NAF") {
      
      AFWings<-dplyr::filter(AFNAFS,NAF %in% input$NAFInput)  # We do not allow for all NAFs to be selected, too many units 
      if(input$SummaryStatistic == "Cases") {
        colset<-c(1,3,2,15,16,17,4,6,7,8,9,10,11)
      } else if (input$SummaryStatistic == "Hospitalizations") {
        colset<-c(1,3,2,14,15,16,4,6,7,8,9,10)
      }  
      
      
      if (input$WingInput == "All") {               
        
        if (input$GroupInput == "All") {                
          GroupList<-sort(unique(AFWings$`Group`), decreasing = FALSE)
          forecastbaselist<-dplyr::filter(AFWings,Group %in% GroupList)                        
          forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
          FilteredDT<-dplyr::filter(FilteredDT,Installation %in% forecastbaselist) 
          
          FilteredDT<-FilterDataTable(FilteredDT,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
          # FilteredDT<-merge(FilteredDT,AFWings, by.x = "Installation", by.y = "Base")
          # FilteredDT<-FilteredDT[, names(FilteredDT)[colset]]  
          # colnames(FilteredDT)[2]<-"State"
          FTPrint<-FilteredDT                        
          dt<-DT::datatable(FilteredDT, rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))   
          dt
          
        } else {                                    
          forecastbaselistG<-dplyr::filter(AFWings,Group %in% input$GroupInput)                        
          forecastbaselist<-sort(unique(forecastbaselistG$Base), decreasing = FALSE) 
          FilteredDT<-dplyr::filter(ForecastDataTableCases,Installation %in% forecastbaselist) 
          
          FilteredDT<-FilterDataTable(FilteredDT,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
          # FilteredDT<-merge(FilteredDT,forecastbaselistG, by.x = "Installation", by.y = "Base")
          # FilteredDT<-FilteredDT[, names(FilteredDT)[colset]]  
          # colnames(FilteredDT)[2]<-"State"
          FTPrint<-FilteredDT                        
          dt<-DT::datatable(FilteredDT, rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))   
          dt
        }
      } else {      #If one wing is selected
        
        AFWings<-dplyr::filter(AFWings,Wing %in% input$WingInput)            
        
        if (input$GroupInput == "All") {
          GroupList<-sort(unique(AFWings$`Group`), decreasing = FALSE)
          forecastbaselistG<-dplyr::filter(AFWings,Group %in% GroupList)                        
          forecastbaselist<-sort(unique(forecastbaselistG$Base), decreasing = FALSE) 
          FilteredDT<-dplyr::filter(FilteredDT,Installation %in% forecastbaselist) 
          
          FilteredDT<-FilterDataTable(FilteredDT,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
          # FilteredDT<-merge(FilteredDT,forecastbaselistG, by.x = "Installation", by.y = "Base")
          # FilteredDT<-FilteredDT[, names(FilteredDT)[colset]]  
          # colnames(FilteredDT)[2]<-"State"
          FTPrint<-FilteredDT                        
          dt<-DT::datatable(FilteredDT, rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))   
          dt
          
        } else {                                    
          forecastbaselistG<-dplyr::filter(AFWings,Group %in% input$GroupInput)                        
          forecastbaselist<-sort(unique(forecastbaselistG$Base), decreasing = FALSE) 
          FilteredDT<-dplyr::filter(FilteredDT,Installation %in% forecastbaselist) 
          
          FilteredDT<-FilterDataTable(FilteredDT,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
          # FilteredDT<-merge(FilteredDT,forecastbaselistG, by.x = "Installation", by.y = "Base")
          # FilteredDT<-FilteredDT[, names(FilteredDT)[colset]]  
          # colnames(FilteredDT)[2]<-"State"
          FTPrint<-FilteredDT                        
          dt<-DT::datatable(FilteredDT, rownames = FALSE, options = list(dom = 'ft',ordering = F, "pageLength"=200))   
          dt
          
        }
      }
    }
  })
  
  
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      if(input$SummaryStatistic == "Cases") {
        PrintDT<-ForecastDataTableCases 
        FName<-"Cases"
      } else if (input$SummaryStatistic == "Hospitalizations") {
        PrintDT<-ForecastDataTable
        FName<-"Hospitalizations"
      } 
      paste("SummaryDataset-",FName,"-",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(PrintDT, file)
    })
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() { 
      paste("FilteredSummaryDataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if (input$MAJCOMNAF == "MAJCOM") {
        if (input$MAJCOMInput == "All") {
          if(input$SummaryStatistic == "Cases") {
            ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
            FTPrint<-ForecastDataTableCases
          } else {
            ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
            FTPrint<-ForecastDataTableCases
          }
        } else if(input$MAJCOMInput=="Active Duty"){
          if(input$SummaryStatistic == "Cases") {
            ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
            FTPrint<-ForecastDataTableCases
          } else {
            ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
            FTPrint<-ForecastDataTable
          }
        }
        else {
          if(input$SummaryStatistic == "Cases") {
            ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
            FTPrint<-ForecastDataTableCases
          } else {
            ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
            FTPrint<-ForecastDataTable                    
          }
        }
      } else if (input$MAJCOMNAF == "NAF") {
        AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)  # We do not allow for all NAFs to be selected, too many units                     
        colset<-c(1,3,2,14,15,16,4,6,7,8,9,10)
        
        if (input$WingInput == "All") {     
          #AFWings<-dplyr::filter(AFWings,Wing %in% WingList)
          if (input$GroupInput == "All") {                
            GroupList<-sort(unique(AFWings$`Group`), decreasing = FALSE)
            forecastbaselist<-dplyr::filter(AFWings,Group %in% GroupList)                        
            forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
            ForecastDataTableCases<-dplyr::filter(ForecastDataTableCases,Installation %in% forecastbaselist) 
            ForecastDataTable<-dplyr::filter(ForecastDataTable,Installation %in% forecastbaselist) 
            
            if(input$SummaryStatistic == "Cases") {  #if all groups are selected
              ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
              ForecastDataTableCases<-merge(ForecastDataTableCases,AFNAFS, by.x = "Installation", by.y = "Base")
              ForecastDataTableCases<-ForecastDataTableCases[, names(ForecastDataTableCases)[colset]]  
              colnames(ForecastDataTableCases)[2]<-"State"
              FTPrint<-ForecastDataTableCases                        
            } else {                                 #if one group is selected
              ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
              ForecastDataTable<-merge(ForecastDataTable,AFNAFS, by.x = "Installation", by.y = "Base")
              ForecastDataTable<-ForecastDataTable[, names(ForecastDataTable)[colset]]  
              colnames(ForecastDataTable)[2]<-"State"
              FTPrint<-ForecastDataTable                        
            }
          } else {                                    
            forecastbaselist<-dplyr::filter(AFWings,Group %in% input$GroupInput)                        
            forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
            ForecastDataTableCases<-dplyr::filter(ForecastDataTableCases,Installation %in% forecastbaselist) 
            ForecastDataTable<-dplyr::filter(ForecastDataTable,Installation %in% forecastbaselist)                     
            
            if(input$SummaryStatistic == "Cases") {  #if all groups are selected
              ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
              ForecastDataTableCases<-merge(ForecastDataTableCases,AFNAFS, by.x = "Installation", by.y = "Base")
              ForecastDataTableCases<-ForecastDataTableCases[, names(ForecastDataTableCases)[colset]]  
              colnames(ForecastDataTableCases)[2]<-"State"
              FTPrint<-ForecastDataTableCases                        
            } else {                                 #if one group is selected
              ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
              ForecastDataTable<-merge(ForecastDataTable,AFNAFS, by.x = "Installation", by.y = "Base")
              ForecastDataTable<-ForecastDataTable[, names(ForecastDataTableCases)[colset]]
              colnames(ForecastDataTable)[2]<-"State"
              FTPrint<-ForecastDataTable
            }
          }
        } else {      #If one wing is selected
          
          AFWings<-dplyr::filter(AFWings,Wing %in% input$WingInput)            
          
          if (input$GroupInput == "All") {
            GroupList<-sort(unique(AFWings$`Group`), decreasing = FALSE)
            forecastbaselist<-dplyr::filter(AFWings,Group %in% GroupList)                        
            forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
            ForecastDataTableCases<-dplyr::filter(ForecastDataTableCases,Installation %in% forecastbaselist) 
            ForecastDataTable<-dplyr::filter(ForecastDataTable,Installation %in% forecastbaselist) 
            
            if(input$SummaryStatistic == "Cases") {  #if all groups are selected
              ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
              ForecastDataTableCases<-merge(ForecastDataTableCases,AFNAFS, by.x = "Installation", by.y = "Base")
              ForecastDataTableCases<-ForecastDataTableCases[, names(ForecastDataTableCases)[colset]]  
              colnames(ForecastDataTableCases)[2]<-"State"
              FTPrint<-ForecastDataTableCases                        
            } else {                                 #if one group is selected
              ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
              ForecastDataTable<-merge(ForecastDataTable,AFNAFS, by.x = "Installation", by.y = "Base")
              ForecastDataTable<-ForecastDataTable[, names(ForecastDataTable)[colset]]  
              colnames(ForecastDataTable)[2]<-"State"
              FTPrint<-ForecastDataTable                        
            }
          } else {                                    
            forecastbaselist<-dplyr::filter(AFWings,Group %in% input$GroupInput)                        
            forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
            ForecastDataTableCases<-dplyr::filter(ForecastDataTableCases,Installation %in% forecastbaselist) 
            ForecastDataTable<-dplyr::filter(ForecastDataTable,Installation %in% forecastbaselist)                     
            
            if(input$SummaryStatistic == "Cases") {  #if all groups are selected
              ForecastDataTableCases<-FilterDataTable(ForecastDataTableCases,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
              ForecastDataTableCases<-merge(ForecastDataTableCases,AFNAFS, by.x = "Installation", by.y = "Base")
              ForecastDataTableCases<-ForecastDataTableCases[, names(ForecastDataTableCases)[colset]]  
              colnames(ForecastDataTableCases)[2]<-"State"
              FTPrint<-ForecastDataTableCases                        
            } else {                                 #if one group is selected
              ForecastDataTable<-FilterDataTable(ForecastDataTable,input$SummaryModelType,input$SummaryForecast,input$SummaryStatistic)
              ForecastDataTable<-merge(ForecastDataTable,AFNAFS, by.x = "Installation", by.y = "Base")
              ForecastDataTable<-ForecastDataTable[, names(ForecastDataTable)[colset]]  
              colnames(ForecastDataTable)[2]<-"State"
              FTPrint<-ForecastDataTable                        
            }
          }
        }
      }        
      
      
      write.csv(FTPrint, file)
    })
  
  output$HotSpotData <- downloadHandler(
    filename = function() { 
      paste("HotspotDataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Top15Report, file)
      
    })    
  
  output$HotSpotDataOneMile <- downloadHandler(
    filename = function() { 
      paste("HotspotDatasetOneMile-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Top15ReportOneMile, file)
      
    })  
  
  output$MTFSummaryT <- downloadHandler(
    filename = function() { 
      paste("MTFSummary-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      Stat<-"Cases"
      forecastbaselist<-dplyr::filter(AFBaseLocations,Branch %in% input$Branch)                        
      forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 

      FilteredDT1<-dplyr::filter(MTFSummaryReport,Installation %in% forecastbaselist)                        

      if(input$OperationalInput != "All") {
        forecastbaselist<-dplyr::filter(AFBaseLocations,Operational %in% input$OperationalInput)                        
        forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE)         
        FilteredDT1<-dplyr::filter(FilteredDT1,Installation %in% forecastbaselist)
      }      
      
      if (input$MAJCOMInput == "All") {
        FTPrint<-FilteredDT1
      } else {
        FTPrint<-dplyr::filter(FilteredDT1,MAJCOM %in% input$MAJCOMInput)                        
      }
      
      write.csv(FTPrint, file)
    })    
  
  # output$MTFSummaryT <- downloadHandler(
  #   if (input$Branch == "Air Force"){
  #       if (input$MAJCOMInput != "All") {
  # 
  #           Stat<-"Cases"
  #           forecastbaselist<-dplyr::filter(AFBaseLocations,Branch %in% input$Branch)                        
  #           forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
  #           
  #           FilteredDT1<-dplyr::filter(MTFSummaryReport,Installation %in% forecastbaselist)                        
  #           
  #           if(input$OperationalInput != "All") {
  #             forecastbaselist<-dplyr::filter(AFBaseLocations,Operational %in% input$OperationalInput)                        
  #             forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE)         
  #             FilteredDT1<-dplyr::filter(FilteredDT1,Installation %in% forecastbaselist)
  #           }      
  #           
  #           if (input$MAJCOMInput == "All") {
  #             FTPrint<-FilteredDT1
  #           } else {
  #             FTPrint<-dplyr::filter(FilteredDT1,MAJCOM %in% input$MAJCOMInput)                        
  #           }      
  #         
  #           #Then run for loop off of final filter     
  #           for (i in 1:nrow(FTPrint)){ 
  #               filename = function() { paste(input$dataset, '.png', sep='') },
  #               content = function(file) {                
  #                   ggsave(file, plot = plotInput(), device = "png")
  #               }
  #           }
  #       }
  #   }
  # )
  
  output$HotSpot <- renderPlot({
    HotspotPlot(input$Branch,input$OperationalInput,input$MAJCOMNAF,
                input$MAJCOMInput,input$NAFInput,input$WingInput,input$GroupInput)
  })
  
  
  
  
  # Output Report ------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # output$MTFSummaryP <- downloadHandler(
  #     filename = function() {  
  #       "MTF_Plots.pptx"
  #     },   
  # 
  #     content = function(file) {     
  #         Stat<-"Cases"
  #         
  #         # iBranch  <- "Air Force"
  #         # iOperationalInput <- "Active Duty"
  #         # iMAJCOMInput <- "AFMC"
  #         
  #         forecastbaselist<-dplyr::filter(AFBaseLocations,Branch %in% input$Branch)                        
  #         forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
  #         
  #         FilteredDT1<-dplyr::filter(MTFSummaryReport,Installation %in% forecastbaselist)                        
  #         
  #         if(input$OperationalInput != "All") {
  #           forecastbaselist<-dplyr::filter(AFBaseLocations,Operational %in% input$OperationalInput)                        
  #           forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE)         
  #           FilteredDT1<-dplyr::filter(FilteredDT1,Installation %in% forecastbaselist)
  #         }      
  #         
  #         if (input$MAJCOMInput == "All") {
  #           FTPrint<-FilteredDT1
  #         } else {
  #           FTPrint<-dplyr::filter(FilteredDT1,MAJCOM %in% input$MAJCOMInput)                        
  #         }
  #         
  #       
  #         #Create a new powerpoint document
  #         doc <- read_pptx() %>%
  #                 add_slide("Title Slide", master = "Office Theme")
  #                 #add_Title(doc,"Create a PowerPoint document from R software")
  #         
  #         for (i in 1:(nrow(FTPrint))){
  #         #LocalHealthPlot1
  #           
  #             ChosenBase <- ForecastDataTableCases$Installation[i]
  #             value = NULL
  #             IncludedCounties<-GetCounties(ChosenBase,50,value,value)
  #           
  #             DailyChart <- CovidCasesPerDayChart(IncludedCounties)
  #             DailyChart <- dplyr::filter(DailyChart, ForecastDate >= DailyChart$ForecastDate[1] + 35)
  #             
  #             plotDaily <- ggplot(DailyChart) + 
  #               geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 0.5) +
  #               scale_colour_manual(values=c("Blue", "Red")) +
  #               xlab('Date') +
  #               ylab('Number of People') +
  #               theme_bw() + 
  #               theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
  #                     axis.title = element_text(face = "bold", size = 11, family = "sans"),
  #                     axis.text.x = element_text(angle = 60, hjust = 1), 
  #                     axis.line = element_line(color = "black"),
  #                     legend.position = "top",
  #                     plot.background = element_blank(),
  #                     panel.grid.major = element_blank(),
  #                     panel.grid.minor = element_blank(),
  #                     panel.border = element_blank()) +
  #               scale_x_date(date_breaks = "1 week") +
  #               labs(color='')
  #             
  #             plotDaily <- ggplotly(plotDaily)
  #             plotDaily <- plotDaily %>% layout(legend = list(orientation = "h",   # show entries horizontally
  #                                                             xanchor = "center",  # use center of legend as anchor
  #                                                             x = 0.5,
  #                                                             y = 1.2)) #%>% config(displayModeBar = FALSE)
  #             plotDaily      
  # 
  #             doc %>% add_slide(layout="Two Content",master='Office Theme') %>%
  #                     ph_with(plotDaily,location = ph_location_left())                            
  #                     #ph_with(dml(ggobj = plotDaily),location = ph_location_left())                            
  #       
  #             # #LocalHealthPlot2
  #             # CumulChart <- CovidCasesCumChart(IncludedCounties)
  #             # CumulChart <- dplyr::filter(CumulChart, ForecastDate >= CumulChart$ForecastDate[1] + 35)
  #             # 
  #             # #Plot for local area cumulative cases
  #             # plotTot <- ggplot(CumulChart) + 
  #             #   geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 0.5) +
  #             #   scale_colour_manual(values=c("Blue", "Red", "Green"))+
  #             #   xlab('Date') +
  #             #   ylab('Number of People') +
  #             #   theme_bw() + 
  #             #   theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
  #             #         axis.title = element_text(face = "bold", size = 11, family = "sans"),
  #             #         axis.text.x = element_text(angle = 60, hjust = 1), 
  #             #         axis.line = element_line(color = "black"),
  #             #         plot.background = element_blank(),
  #             #         panel.grid.major = element_blank(),
  #             #         panel.grid.minor = element_blank(),
  #             #         panel.border = element_blank(),
  #             #         legend.position = c(0, 1),) +
  #             #   scale_x_date(date_breaks = "1 week")
  #             # 
  #             # plotTot <- ggplotly(plotTot)
  #             # plotTot <- plotTot %>% layout(legend = list(orientation = "h",   # show entries horizontally
  #             #                                             xanchor = "center",  # use center of legend as anchor
  #             #                                             x = 0.5,
  #             #                                             y = 1.2)) #%>% config(displayModeBar = FALSE)
  #             # plotTot        
  #             
  #             # grid.arrange(plotDaily,plotTot)
  #             # 
  #             # grid.arrange(grobs=lapply(list(plotDaily,plotTot), grobTree), ncol=2)
  #             # 
  #             # grid.arrange(grobTree(plotDaily),grobTree(plotTot), ncol=2) 
  #             # 
  #             # grid.arrange(plotDaily,plotTot,ncol=2,top="Main Title")
  #             # 
  #             # plotDaily
  #             # plotTot
  #             # 
  #             # par(mfcol = c(1, 1))
  #             # pobj <- grid.arrange(grobs = list(plotDaily,plotTot),nrow=1)
  #             # 
  #             # doc %>% ph_with(dml(ggobj = plotTot),location = ph_location_right())
  #             # 
  #         }
  #   
  #         print(doc,target = file)
  #   }
  # )

  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){
      paste0('CHAD_report(',paste(Sys.Date(),sep = '_'),')','.pptx')
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
      rmarkdown::render("www/7_other_resources/TestReport2.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )

  
  
  # Step Three
  ###################################################################################################################################################
  
  #Step three provides input information for annotation of the overall app such as inputs, sources, and calculations.
  
  DocDisplay <- reactiveValues(data = "www/6_load_info_docs/OverviewInfo.md")
  
  observeEvent(input$overviewInfo, {
    DocDisplay$data <- "www/6_load_info_docs/OverviewInfo.md"
  })
  
  observeEvent(input$inputInfo, {
    DocDisplay$data <- "www/6_load_info_docs/InputsInfo.md"
  })
  
  observeEvent(input$calcInfo, {
    DocDisplay$data <- "www/6_load_info_docs/CalcInfo.md"
  })
  
  observeEvent(input$projInfo, {
    DocDisplay$data <- "www/6_load_info_docs/ProjInfo.md"
  })
  
  observeEvent(input$sourceInfo, {
    DocDisplay$data <- "www/6_load_info_docs/SourceInfo.md"
  })
  
  observeEvent(input$aboutInfo, {
    DocDisplay$data <- "www/6_load_info_docs/About.md"
  })
  
  
  output$Documentation <- renderUI({
    includeMarkdown(DocDisplay$data)
  })
  
  
  observeEvent(input$UpdateInfo, {
    showModal(
      modalDialog(
        size = "l",fade = TRUE, easyClose = TRUE, title = "VERSION UPDATES",
        UpdateLink)
    )
  })
  # 
  # observeEvent(input$overviewInfo, {
  #   showModal(
  #     modalDialog(
  #       size = "l",fade = TRUE, easyClose = TRUE, title = "OVERVIEW",
  #       OverviewLink)
  #   )
  # })
  # 
  # observeEvent(input$inputInfo, {
  #   showModal(
  #     modalDialog(
  #       size = "l",fade = TRUE, easyClose = TRUE, title = "USER INPUTS",
  #       InfoLink)
  #   )
  # })
  # observeEvent(input$projInfo, {
  #   showModal(
  #     modalDialog(
  #       size = "l",fade = TRUE, easyClose = TRUE, title = "PROJECTIONS",
  #       ProjLink)
  #   )
  # })
  # 
  # observeEvent(input$calcInfo, {
  #   showModal(
  #     modalDialog(
  #       size = "l",fade = TRUE, easyClose = TRUE, title = "CALCULATIONS",
  #       CalcLink)
  #   )
  # })
  # 
  # observeEvent(input$sourceInfo, {
  #   showModal(
  #     modalDialog(
  #       size = "l",fade = TRUE, easyClose = TRUE, title = "SOURCES",
  #       SourceLink)
  #   )
  # })
  
  
  
  
  
  
  
  
  
}