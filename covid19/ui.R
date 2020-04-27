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
                  dashboardHeader(title = div(img(src=base64enc::dataURI(file="www/7_other_resources/AFIT_Emblem_Blue.png", mime="image/png") ,height = '50',width = '110')),
                                  titleWidth = 300,
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
                  dashboardSidebar(width = 300,
                                   sidebarMenu(id="tabs",
                                               tags$p(paste0("* Current as of ",format(Sys.Date(),format = "%d %B %Y")," at 0600 EST *")),
                                               conditionalPanel(condition="input.tabselected>=3",
                                                                selectInput(
                                                                            "Base",
                                                                            "Choose your base:", 
                                                                            list(`Installation` = sort(BaseList)), 
                                                                            selectize = FALSE),
                                                                sliderInput("Radius",
                                                                            "Choose your local radius (miles):",
                                                                            min = 10,
                                                                            max = 100,
                                                                            value = 50)
                                                                ),
                                       
                                       conditionalPanel(condition="input.tabselected==1",
                                                        "MAJCOM Summary Inputs",
                                           tabName = "MAJCOMsummary",
                                           icon = icon("sliders-h"),
                                           div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                           radioButtons("SummaryStatistic",
                                                        "Cases or Hospitalizations: ",
                                                        c("Cases"="Cases",
                                                          "Hospitalizations"="Hospitalizations")),
                                           selectInput(
                                               "MAJCOMInput",
                                               "MAJCOM:", 
                                               list(`MAJCOM` = MAJCOMList ), 
                                               selectize = FALSE),
                                           radioButtons("SummaryModelType",
                                                        "Summary Plot Model: ",
                                                        c("IHME"="IHME",
                                                          "CHIME"="CHIME"),
                                                        selected = "CHIME"),
                                           radioButtons("SummaryForecast",
                                                        "Choose Days Forecasted: ",
                                                        c('Today'='Today',
                                                          "7 Days"="Seven",
                                                          "14 Days"="Fourteen",
                                                          "21 Days"="Twenty-One",
                                                          "30 Days"="Thirty"))
                                           
                                       ),
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
                                           # checkboxGroupInput("SocialDistanceValue", "Local Social Distancing Actions: ",
                                           #                     c("Close Schools" = "CS",
                                           #                       "Businesses Telework" = "CB",
                                           #                       "Social Distance" = "SD")),
                                           checkboxGroupInput("ModelSelectionValue","Forecasting Model(s): ",
                                                              c("IHME"="IHME",
                                                                "LANL"="LANL",
                                                                "CHIME SC"="CHIME1",
                                                                "CHIME NE"="CHIME2",
                                                                "CHIME SC+NE"="CHIME3",
                                                                "CHIME SD"="CHIME4",
                                                                "CHIME SC+SD"="CHIME5",
                                                                "CHIME NE+SD"="CHIME6",
                                                                "CHIME SC+NE+SD"="CHIME7",                                                                                                                                
                                                                "Columbia No Intervetion"="CUNI",
                                                                "Columbia 20% SC Reduction"="CU20SC",
                                                                "Columbia 30% SC Reduction"="CU30SC",
                                                                "Columbia 40% SC Reduction"="CU40SC")),
                                           actionLink("selectall","Select All")
                                       ),
                                       br(),
                                       
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

                                       div(style="text-align:center", tags$hr(style="border-color: #444;"), "Generate & Download Report:"),
                                       br(),
                                       fluidRow(
                                           downloadButton("report", "Generate Report", class = "butt"),
                                           tags$style(".skin-black .sidebar .butt{background-color:#15824d;color: white;border-color:white;}"),
                                           align = "center"
                                       )

                                      
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
                                   $("header").find("nav").append(\'<span class="myClass"> COVID-19 Health Assessment Dashboard Beta v0.6</span>\');
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
                                                 DT::dataTableOutput("ForecastDataTable"), 
                                                 style = "height:720px;overflow-y: scroll"), 
                                          height = 900, 
                                          width =13,
                                          downloadButton('downloadData', 'Download Full Dataset'),
                                          downloadButton('HotSpotData', 'Download Hotspot Dataset'))
                                      
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
                                          box(title = "Daily Reports",plotlyOutput("LocalHealthPlot1",height = 300)),
                                          box(title = "Total Reports",plotlyOutput("LocalHealthPlot2",height = 300))
                                      ),
                                      fluidRow(
                                          box(title = "Local Impact Map", plotlyOutput("LocalChoroPlot", height = 250),height = 300),
                                          box(title = "Local County Statistics", solidHeader=T, align = "left", column(width = 12, DT::dataTableOutput("CountyDataTable1"), style = "height:240px;overflow-y: scroll"), height = 300)
                                      )
                                  ),
                                  ####### END CURRENT LOCAL HEALTH TAB #######
                                  
                                  ####### BEGIN LOCAL PROJECTION TAB #########
                                  # Local Health Projections ------------------------------------------------
                                  tabPanel(
                                      value= 4,
                                      title = "Local Health Projections",
                                      fluidRow(
                                          valueBoxOutput("TotalPopulation"),
                                          valueBoxOutput("IHMEPeakDate"),
                                          valueBoxOutput("CHIMEPeakDate")
                                          #valueBoxOutput("TotalPopulation"),
                                          #valueBoxOutput("IHMEMinMax"),
                                          #valueBoxOutput("CHIMEMinMax")
                                      ),
                                      # fluidRow(
                                      #     box(plotlyOutput("IHME_State_Hosp",height = 400)),
                                      #     box(plotlyOutput("SEIARProjection"),height = 400)),
                                      box(plotlyOutput("OverlayPlots",height=700, width=1500))
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
                                  

                                      ) #close dash body

                      )
                  )
    )
    

              




