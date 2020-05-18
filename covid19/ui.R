##########################
##### User Interface #####
##########################

# Layout
##############################################################################################################################################
# The User Interface generates the visual of the application. It establishes location and layout of all outputs and inputs from server and user
# First:  The dashboard header shows the main title and introduction to the application
# Second: The sidebar shows all inputs that the user can change
# Third:  The body provides all visual outputs, statistics, and charts. It is updated every time a user changes the inputs
##############################################################################################################################################     




# Begin User Interface ------------------------------------------------------------------------------------------------------------------------------------------------------------



#Build UI
#Establishes the layout of the overall dashboard and how items are displayed
ui <- tagList(
  #Warning Banner
  HTML('<body text = white bgcolor = blue> <center> <font size = 3 color = black> *** Pre-Decisional // Projections Are Estimates and Pulled From Public Sources *** </p> </center></body>'),
  dashboardPage(skin = "black",title="COVID-19 Health Assessment Dashboard",
                
                # Step One - Header
                ###################################################################################################################################################
                dashboardHeader(title = div(img(src=base64enc::dataURI(file="www/7_other_resources/Logo.png", mime="image/png") ,height = '50',width = '150')),
                                titleWidth = 300,
                                dropdownMenu( 
                                  icon = tags$div(HTML('<font size = "5" color = "blue" font-weight:"bold" >Current Upates</font>  <i class="fa fa-flag fa-2x" style = "font-size:18px;"></i> <body style="background-color:green;"></body>')),
                                  headerText = "",
                                  badgeStatus = "success",
                                  tags$li(actionLink("UpdateInfo", label = "Update Information", icon = icon("flag")),class = "dropdown")
                                ),
                                dropdownMenu( 
                                  icon = tags$div(HTML('<font size = "5" color = "blue" font-weight:"bold" >More Information</font>  <i class="fa fa-info-circle" style = "font-size:18px;"></i> <body style="background-color:powderblue;"></body>')),
                                  headerText = "Want to know more?",
                                  badgeStatus = "primary",
                                  tags$li(actionLink("overviewInfo", label = "Overview", icon = icon("globe")),
                                          class = "dropdown"),
                                  tags$li(actionLink("inputInfo", label = "User Inputs", icon = icon("sliders-h")),
                                          class = "dropdown"),
                                  tags$li(actionLink("projInfo", label = "Projections", icon = icon("chart-line")),
                                          class = "dropdown"),
                                  tags$li(actionLink("calcInfo", label = "Calculations", icon = icon("calculator")),
                                          class = "dropdown"),
                                  tags$li(actionLink("sourceInfo", label = "Sources", icon = icon("user-secret")),
                                          class = "dropdown")
                                )
                ),
                
                # Step Two - Sidebar
                ###################################################################################################################################################
                dashboardSidebar(width = 350,
                                 sidebarMenu(id="tabs",
                                             tags$p(paste0("* Current as of ",format(Sys.Date(),format = "%d %B %Y")," at 0600 EST *")),

                                             conditionalPanel(condition="input.tabselected==1",
                                                                "MAJCOM Summary Inputs",
                                                                 tabName = "MAJCOMsummary",
                                                                 icon = icon("sliders-h"),
                                                                 div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                                              radioButtons("SummaryStatistic",
                                                                            "Cases or Hospitalizations: ",
                                                                            c("Cases"="Cases","Hospitalizations"="Hospitalizations"),
                                                                            selected = c("Hospitalizations")),
                                                              selectInput("Branch",
                                                                           "Service Branch:", 
                                                                           list(`Branch` = BranchList ),
                                                                           selected = c("Air Force")),
                                                              selectInput("OperationalInput",
                                                                          "Operational Status:", 
                                                                          list(`Status` = OperationalList),
                                                                          selected = c("Active")),
                                                              conditionalPanel(condition = "input.Branch == 'Air Force'",                                           
                                                                   radioButtons("MAJCOMNAF",
                                                                                "MAJCOM or NAF Filter: ",
                                                                                c("MAJCOM"="MAJCOM","NAF"="NAF"),
                                                                                selected = c("MAJCOM")), 
                                                                   conditionalPanel(condition = "input.MAJCOMNAF == 'MAJCOM'",
                                                                             selectInput("MAJCOMInput",
                                                                                 "MAJCOM:", 
                                                                                 list(`MAJCOM` = MAJCOMList ), 
                                                                                 selectize = FALSE)),
                                                                   conditionalPanel(condition = "input.MAJCOMNAF == 'NAF'",
                                                                             selectInput("NAFInput",
                                                                                 "Numbered Air Forces:", 
                                                                                 choices=NAFList,
                                                                                 selected = c("All")),
                                                                             selectInput("WingInput",
                                                                                 "Wing:", 
                                                                                 list(`Wings` = WingList),
                                                                                 selectize = FALSE),
                                                                             selectInput("GroupInput",
                                                                                 "Group:",
                                                                                 choices=NULL,
                                                                                 selectize = FALSE))),  
                                                             #actionButton("FilterBases", "Select"),
                                                             radioButtons("SummaryModelType",
                                                                          "Summary Plot Model: ",
                                                                          c("IHME"="IHME","CHIME"="CHIME"),
                                                                          selected = c("CHIME")),
                                                             radioButtons("SummaryForecast",
                                                                          "Choose Days Forecasted: ",
                                                                          c('Today'='Today',"7 Days"="Seven",
                                                                            "14 Days"="Fourteen","21 Days"="Twenty-One",
                                                                            "30 Days"="Thirty"),
                                                                          selected = c("Seven"))
                                                   ),
                                             
                                             conditionalPanel(condition="input.tabselected==2",
                                                              "National Summary",
                                                              icon = icon("sliders-h"),
                                                              div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                                              radioButtons("MapView",
                                                                           "Map Selection: ",
                                                                           c("US"="US",
                                                                             "Europe"="Europe",
                                                                             "Asia"="Asia"))
                                             ),
                                             
                                             conditionalPanel(condition="input.tabselected == 3 || input.tabselected == 4",
                                                              selectInput("BranchP",
                                                                          "Service Branch:", 
                                                                          list(`Branch` = BranchList),
                                                                          selected = c("Air Force")),
                                                              selectInput("OperationalInputP",
                                                                          "Operational Status:", 
                                                                          list(`Status` = OperationalListP),
                                                                          selected = c("Active")),
                                                              selectInput("Base",
                                                                          "Choose your base:",
                                                                          list(`Status` = BaseListP),
                                                                          selected = c("Eglin Air Force Base")),
                                                              sliderInput("Radius",
                                                                          "Choose your local radius (miles):",
                                                                          min = 10,
                                                                          max = 100,
                                                                          value = 50)),
                                             
                                             conditionalPanel(condition="input.tabselected==3",
                                                              "Current Local Health Inputs",
                                                              tabName = "localHealthInput",
                                                              icon = icon("map-marker-alt"),
                                                              div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                                              radioButtons("TypeLocal", "State or County Plot:",
                                                                           c("County"="County",
                                                                             "State"="State"))
                                             ),
                                             conditionalPanel(condition="input.tabselected==4",
                                                              "Local Health Projection Inputs",
                                                              tabName = "localHealthProj",
                                                              icon = icon("sliders-h"),
                                                              div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                                              radioButtons("StatisticType", "Choose projected statistic:",
                                                                           c("Hospitalizations"="Hospitalizations",
                                                                             "Fatalities"="Fatalities")),
                                                              sliderInput("proj_days",
                                                                          "Projection days:",
                                                                          min = 7,
                                                                          max = 30,
                                                                          value = 14),

                                                              checkboxGroupInput("Utilization","Show Hospitalization for COVID-19 Patients",
                                                                                 c("Hospital Utilization Line"="Util"),                                                                
                                                                                 selected = c("Util")),
                                                                                
                                                              checkboxGroupInput("ModelSelectionValue1","Forecasting Model(s): ",
                                                                                 c("IHME (University of Washinton)"="IHME",
                                                                                   "Youyang Gu - Independent (YYG) Model"="YYG",
                                                                                   "CHIME: SC"="CHIME7",
                                                                                   "University of Texas"="UT",
                                                                                   "Columbia University: 20% SC Reduction with weekly 10% increase in contact"="CU20SCw10"),                                                                
                                                                                 selected = c("IHME","CHIME7")),
                                                              actionLink("selectall1","Select All"),
                                                              
                                                              checkboxGroupInput("AdditionalModels","Additional Forecasting Model(s): ",
                                                                                 c("Show All"="ShowAll"),
                                                                                 selected = c("")),                                            
                                                              conditionalPanel(condition = "input.AdditionalModels== 'ShowAll'",  
                                                                              checkboxGroupInput("ModelSelectionValue2","Forecasting Model(s): ",
                                                                                                 c("DTRA 1 - Relaxed SD"="DTRA1",
                                                                                                   "DTRA 2 - Relaxed SD w/ Testing"="DTRA2",
                                                                                                   "CHIME (University of Pennsylvania): SC+NE+SD"="CHIME1",
                                                                                                   "CHIME: NE+SD"="CHIME2",
                                                                                                   "CHIME: SC+SD"="CHIME3",                                                                
                                                                                                   "CHIME: SD"="CHIME4", 
                                                                                                   "CHIME: SC+NE"="CHIME5",
                                                                                                   "CHIME: NE"="CHIME6",
                                                                                                   "Los Alamos National Labs (LANL)"="LANL",
                                                                                                   "Columbia University: 20% SC Reduction with one time 10% increase in contact "="CU20SCx10",
                                                                                                   "Columbia University: 20% SC Reduction with one time 5% increase in contact"="CU20SCx5",
                                                                                                   "Columbia University: 20% SC Reduction with weekly 10% increase in contact"="CU20SCw10",                                                                
                                                                                                   "Columbia University: 20% SC Reduction with weekly 5% increase in contact"="CU20SCw5"),
                                                                                                 selected = c("")),
                                                                              actionLink("selectall2","Select All"))
                                            ),
                                            
                                            conditionalPanel(condition="input.tabselected == 5",
                                                             selectInput("AMClist",
                                                                         "Base List:", 
                                                                         list(`AMCBase` = AMC_model_BaseList),
                                                                         selected = c(""))
                                            ),

                                       br()
                                       
                                   # menuItem(
                                   #     "National Health Projection Inputs",
                                   #     tabName = "natHealthProj",
                                   #     icon = icon("sliders-h"),
                                   #     div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                   #     sliderInput("proj_days_national",
                                   #                 "Projection days:",
                                   #                 min = 7,
                                   #                 max = 30,
                                   #                 value = 14),
                                   #     checkboxGroupInput("SocialDistanceValueNational", "National Social Distancing Actions: ",
                                   #                        c("Close Schools" = "CSN",
                                   #                          "Businesses Telework" = "CBN",
                                   #                          "Social Distance" = "SDN"))
                                   # ),
                                   
                                   # div(style="text-align:center", tags$hr(style="border-color: #444;"), "Generate & Download Report:"),
                                   # br(),
                                   # fluidRow(
                                   #   downloadButton("report", "Generate Report", class = "butt"),
                                   #   tags$style(".skin-black .sidebar .butt{background-color:#15824d;color: white;border-color:white;}"),
                                   #   align = "center"
                                   # )
                                   
                                   
                  )
    ),
    
    
    # Step Three - Body
    ###################################################################################################################################################
    dashboardBody(
      tags$head(tags$style(HTML(
        '.myClass { 
                    font-size: 20px;
                    line-height: 50px;
                    text-align: left;
                    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                    padding: 0 15px;
                    overflow: hidden;
                    color: black;
                    }
                    '))),
      tags$script(HTML('
                                   $(document).ready(function() {
                                   $("header").find("nav").append(\'<span class="myClass"> COVID-19 Health Assessment Dashboard Beta v0.9.5</span>\');
                                   })
                                   ')),
      tabsetPanel(id = "tabselected",
                  
                  ####### BEGIN SUMMARY TAB #########
                  # Mission Risk ------------------------------------------------------------
                  tabPanel(
                    value = 1,
                    title = "MAJCOM Summary",
                    fluidRow(
                      box(plotlyOutput("SummaryTabChoro", height = 600, width = 'auto')),
                      box(plotOutput("HotSpot", height = 600))),
                    box(title = "Base Summary Projections",
                        solidHeader=T, 
                        align = "left", 
                        column(width = 12, 
                               DT::dataTableOutput("ForecastDataTableOut"), 
                               style = "height:720px;overflow-y: scroll"), 
                        height = 900, 
                        width =13,
                        downloadButton('downloadData', 'Download Full Dataset'),
                        downloadButton('downloadFilteredData', 'Download Filtered Dataset (Table Above)'),
                        downloadButton('HotSpotData', 'Download Hotspot Dataset: 50 Mile Radius'),
                        downloadButton('HotSpotDataOneMile', 'Download Hotspot Dataset: Single County'))
                    
                  ),
                  ####### END Mission Risk #######
                  
                  # Summary Tab -------------------------------------------------------------
                  tabPanel(
                    value = 2,
                    title = "National Summary",
                    
                    box(title = "National Impact Map",solidHeader = T, align = "center", htmlOutput("SummaryPlot"),height=700,width=1200),
                    box(title = "National Statistics", solidHeader=T, align = "left", column(width = 12, DT::dataTableOutput("NationalDataTable1"), style = "height:400px;overflow-y: scroll;overflow-x:scroll"),width = 13, height = 500)
                    
                  ),
                  ####### END SUMMARY TAB #######
                  
                  
                  # Current Local Health ----------------------------------------------------
                  tabPanel(
                    value = 3,
                    title = "Current Local Health",
                    fluidRow(
                      # A static valueBox
                      valueBoxOutput("CovidCases", width = 3),
                      valueBoxOutput("CaseChangeLocal", width = 3),
                      valueBoxOutput("CasesPer1000", width = 3),
                      valueBoxOutput("HospitalUtilization", width = 3)
                    ),
                    fluidRow(
                      valueBoxOutput("LocalCovidDeaths", width = 3),
                      valueBoxOutput("DeathChangeLocal", width = 3),
                      valueBoxOutput("CaseDbRate", width = 3),
                      valueBoxOutput("Rt_Estimate", width = 3)
                      
                    ),
                    fluidRow( 
                      tabBox(
                        tabPanel("Daily New Cases",
                                 plotlyOutput("LocalHealthPlot1",height = 300)
                        ),
                        tabPanel("Moving 3-Day Average",
                                 plotlyOutput("LocalHealthPlot3day",height = 300)
                        )
                        # tabPanel("14-day Growth Rate",
                        #          plotlyOutput("LocalHealthPlot14dayGrowth",height = 300)
                        # )
                      ),
                      #box(title = "Daily Reports",plotlyOutput("LocalHealthPlot1",height = 300)),
                      box(title = "Total Reports",plotlyOutput("LocalHealthPlot2",height = 300))
                    ),
                    fluidRow(
                      box(title = "Local Impact Map", plotlyOutput("LocalChoroPlot", height = 250),height = 300),
                      box(title = "Local County Statistics", 
                          solidHeader=T, align = "left", 
                          column(width = 12, 
                                 DT::dataTableOutput("CountyDataTable1"), 
                                 style = "height:240px;overflow-y: scroll"), 
                          height = 300)
                    )
                  ),
                  ####### END CURRENT LOCAL HEALTH TAB #######
                  
                  ####### BEGIN LOCAL PROJECTION TAB #########
                  # Local Health Projections ------------------------------------------------
                  tabPanel(
                    value= 4,
                    title = "Local Health Projections",
                    fluidRow(
                      valueBoxOutput("TotalPopulation")
                      #valueBoxOutput("IHMEPeakDate"),
                      #valueBoxOutput("CHIMEPeakDate")
                      #valueBoxOutput("TotalPopulation"),
                      #valueBoxOutput("IHMEMinMax"),
                      #valueBoxOutput("CHIMEMinMax")
                    ),
                    # fluidRow(
                    #     box(plotlyOutput("IHME_State_Hosp",height = 400)),
                    #     box(plotlyOutput("SEIARProjection"),height = 400)),
                    box(plotlyOutput("OverlayPlots"),height=800, width=1500)
                    ),
                  ####### END PROJECTION TAB #######
                  
                  ####### BEGIN National PROJECTION TAB #########
                  # National Health Projections ------------------------------------------------
                  # tabPanel(
                  #     title = "National Health Projections",
                  #     # fluidRow(
                  #     #     valueBoxOutput("TotalPopulation_National"),
                  #     #     valueBoxOutput("CHIMEPeakDate_National"),
                  #     #     valueBoxOutput("IHMEPeakDate_National")
                  #     #),
                  #     fluidRow(
                  #         box(plotlyOutput("IHMENationaProj",height = 400)),
                  #         box(plotlyOutput("CHIMENationalProj"),height = 400)),
                  #         box(plotlyOutput("NationalPlotOverlay"), width =  900)
                  # )
                  ####### END PROJECTION TAB #######
                  
                  ####### BEGIN Aircrew TAB #########
                  # Air Force Community Projections ------------------------------------------------------------
                  tabPanel(
                    value = 5,
                    title = "AMC Infection Model Projections",
                    fluidRow(
                      valueBoxOutput("ProjPeakInfDate"),
                      valueBoxOutput("ProjTotInf"),
                      valueBoxOutput("ProjTotDeaths")
                    ),
                    fluidRow(
                      column(4,
                             includeMarkdown("www/6_load_info_docs/AMC_Desc.md")),
                      
                      box(title = "Projected Community Epidemic Curve",
                          solidHeader=T,
                          align = "left",
                          column(width = 12,
                                 plotlyOutput("ProjectedEpidemicTable"),
                                 style = "height:720px;overflow-y: scroll"),
                          height = 500,
                          width =8
                      )
                    )
                  )
      )
                  
      ) #close dash body
      
    )
  )
#)