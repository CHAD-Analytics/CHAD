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
  tags$head(includeHTML(("gglTracker.html"))),
  HTML('<body text = white bgcolor = blue> <center> <font size = 3 color = black> *** Pre-Decisional // Projections Are Estimates and Pulled From Public Sources *** </p> </center></body>'),
  dashboardPage(skin = "black",title="COVID-19 Health Assessment Dashboard",
                
                
                
                
                # Step One - Header
                ###################################################################################################################################################
                dashboardHeader(title = div(img(src=base64enc::dataURI(file="www/7_other_resources/Logo.png", mime="image/png") ,height = '50',width = '150')),
                                titleWidth = 300,
                                dropdownMenu( 
                                  icon = tags$div(HTML('<font size = "5" color = "blue" font-weight:"bold" >Current Updates</font>  <i class="fa fa-flag fa-2x" style = "font-size:18px;"></i> <body style="background-color:green;"></body>')),
                                  headerText = "",
                                  badgeStatus = "success",
                                  tags$li(actionLink("UpdateInfo", label = "Update Information", icon = icon("flag")),class = "dropdown")
                                )
                ),
                
                
                
                
                
                
                
                # Step Two - Sidebar
                ###################################################################################################################################################
                dashboardSidebar(width = 300,
                                 sidebarMenu(id="tabs",
                                             #tags$p(paste0("* Current as of ",format(Sys.Date(),format = "%d %B %Y")," at 0600 EST *")),
                                             
                                             
                                             
                                             
                                             # MAJCOM Summary Projection sidebar controls
                                             ######################################################################################################################
                                             
                                             
                                             conditionalPanel(condition="input.tabselected==1",
                                                              "MAJCOM Summary Inputs",
                                                              tabName = "MAJCOMsummary",
                                                              icon = icon("sliders-h"),
                                                              div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                                          
                                                          
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
                                                                                            c("MAJCOM"="MAJCOM"),
                                                                                            selected = c("MAJCOM")), 
                                                                               
                                                                               conditionalPanel(condition = "input.MAJCOMNAF == 'MAJCOM'",
                                                                                                selectInput("MAJCOMInput",
                                                                                                            "MAJCOM:", 
                                                                                                            list(`MAJCOM` = MAJCOMList ), 
                                                                                                            selectize = FALSE)),
                                                                               
                                                                               # conditionalPanel(condition = "input.MAJCOMNAF == 'NAF'",
                                                                               #                  selectInput("NAFInput",
                                                                               #                              "Numbered Air Forces:", 
                                                                               #                              choices=NAFList,
                                                                               #                              selected = c("All")),
                                                                               #                  selectInput("WingInput",
                                                                               #                              "Wing:", 
                                                                               #                              list(`Wings` = WingList),
                                                                               #                              selectize = FALSE),
                                                                               #                  selectInput("GroupInput",
                                                                               #                              "Group:",
                                                                               #                              choices=NULL,
                                                                               #                              selectize = FALSE))
                                                                               )
                                                              
                                                              
                                             ),
                                             
                                             
                                             
                                             
                                             # National/International sidebar controls
                                             ######################################################################################################################
                                             
                                             
                                             conditionalPanel(condition="input.tabselected==2",
                                                              "International/National Summary",
                                                              icon = icon("sliders-h"),
                                                              div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                                              
                                                              radioButtons("MapView",
                                                                           "Map Selection: ",
                                                                           c("World"="World",
                                                                             "United States"="United States",
                                                                             "Europe"="Europe",
                                                                             "Africa"="Africa",
                                                                             "Asia"="Asia",
                                                                             "Oceania"="Oceania"                                                                             
                                                                             ),
                                                                           selected = "United States"),
                                                              
                                                              radioButtons("Metric",
                                                                           "Metric: ",
                                                                           c("Total Cases" = "Total Cases",
                                                                             "Weekly Cases per Capita (100,000)" = "Weekly Cases",
                                                                             "Weekly Total Case Change" = "Weekly Total Change",
                                                                             "Weekly Case Change" = "Weekly Change"),
                                                                           selected = "Weekly Cases"),
                                                              
                                                              conditionalPanel(condition = "input.Metric == 'Total Cases'",
                                                                               radioButtons("MapScale",
                                                                                            "Scaling Selection: ",
                                                                                            c("Log"="Log",
                                                                                              "Linear"="Linear"),
                                                                                            selected = "Log")
                                                              )
                                                              
                                                              
                                             ),
                                             
                                             
                                             
                                             
                                             # Base/Radius selector for Local Health and Projections sidebar controls
                                             ######################################################################################################################
                                             
                                             
                                             conditionalPanel(condition="input.tabselected == 3 || input.tabselected == 4",
                                                              
                                                              selectInput("BranchP",
                                                                          "Service Branch:", 
                                                                          list(`Branch` = BranchList),
                                                                          selected = c("Air Force")),
                                                              
                                                              radioButtons("CONUSP",
                                                                           "CONUS or OCONUS: ",
                                                                           c("CONUS"="CONUS",
                                                                             "OCONUS"="OCONUS"),
                                                                           selected = "CONUS"),  
                                                              
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
                                                                          min = 1,
                                                                          max = 100,
                                                                          value = 50)  
                                              ),
                                             
                                             
                                             
                                             
                                             # State/County map selector for Local Health sidebar controls
                                             ######################################################################################################################
                                             
                                             
                                             conditionalPanel(condition="input.tabselected==3",
                                                              "Current Local Health Inputs",
                                                              tabName = "localHealthInput",
                                                              icon = icon("map-marker-alt"),
                                                              div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300))
                                             ),
                                             
                                             
                                             
                                             
                                             # Local Health Projections sidebar controls
                                             ######################################################################################################################
                                             
                                             
                                             conditionalPanel(condition="input.tabselected==4",
                                                              "Local Health Projection Inputs",
                                                              tabName = "localHealthProj",
                                                              icon = icon("sliders-h"),
                                                              div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                                              
                                                              radioButtons("StatisticType", "Choose projected statistic:",
                                                                           c("Hospitalizations"="Hospitalizations",
                                                                             "ICU Patients"="ICUPatients",
                                                                             "Ventilator Patients"="VentPatients",
                                                                             "Fatalities"="Fatalities")),
                                                              
                                                              sliderInput("proj_days",
                                                                          "Projection days:",
                                                                          min = 7,
                                                                          max = 30,
                                                                          value = 14),
                                                              
                                                              
                                                              conditionalPanel(condition = "input.CONUSP == 'CONUS'",  
                                                                                   
                                                                  checkboxGroupInput("ModelSelectionValue1","Forecasting Model(s): ",
                                                                                     c("IHME (U. of Washinton)"="IHME",
                                                                                       "Center for Army Analysis"="CAA",
                                                                                       "Torch Insight"="Torch",
                                                                                       "Youyang Gu (YYG) Model"="YYG",
                                                                                       "University of Texas"="UT",
                                                                                       "Columbia University: Current contact rates remain unchanged"="CUM1"),
                                                                                     selected = c("IHME","CAA","Torch")),
                                                                  actionLink("selectall1","Select All"),
                                                                  
                                                                  checkboxGroupInput("AdditionalModels","Additional Forecasting Model(s): ",
                                                                                     c("Show All"="ShowAll"),
                                                                                     selected = c("")),
                                                                  
                                                                  conditionalPanel(condition = "input.AdditionalModels== 'ShowAll'",  
                                                                                   checkboxGroupInput("ModelSelectionValue2","Forecasting Model(s): ",
                                                                                                      c("IHME (Best Case)"="IHME-Best",
                                                                                                        "IHME (Worse Case)"="IHME-Worse",
                                                                                                        "Los Alamos National Labs (LANL)"="LANL","Los Alamos National Labs (LANL)"="LANL",
                                                                                                        "Columbia University: One time 5% increase in social contact"="CUM2",
                                                                                                        "Columbia University: 5% weekly increase in social contact"="CUM3",
                                                                                                        "Columbia University: Current levels of social mixing remain unchanged"="CUM4",                                                                
                                                                                                        "DTRA 1 - Current Response"="DTRA1",
                                                                                                        "DTRA 2 - Improved Response"="DTRA2", 
                                                                                                        "DTRA 3 - Worst Case"="DTRA3"),
                                                                                                      selected = c("")),
                                                                                   actionLink("selectall2","Select All"))
                                                              ),
                                                              
                                                              conditionalPanel(condition = "input.CONUSP == 'OCONUS'",  
                                                                               
                                                                               checkboxGroupInput("ModelSelectionValue3","Forecasting Model(s): ",
                                                                                                  c("IHME (U. of Washinton)"="IHME",
                                                                                                    "IHME (Best Case)"="IHME-Best",
                                                                                                    "IHME (Worse Case)"="IHME-Worse",                                                                                                    
                                                                                                    "Youyang Gu (YYG) Model"="YYG",
                                                                                                    "Los Alamos National Labs (LANL)"="LANL"),                                                                
                                                                                                  selected = c("IHME","YYG","LANL")),
                                                                               actionLink("selectall3","Select All")
                                                              )
                                                                                                                                                            
                                             ),
                                             
                                             
                                             
                                             
                                             # Welcome Page sidebar controls
                                             ######################################################################################################################
                                             
                                             
                                             conditionalPanel(condition="input.tabselected == 6",
                                                              br(),br(),br(),
                                                              tags$div(style="text-align: center; font-size: 18px", actionLink("overviewInfo", label = "Overview", icon = icon("globe"))),
                                                              br(),
                                                              tags$div(style="text-align: center; font-size: 18px", actionLink("inputInfo", label = "User Inputs", icon = icon("sliders-h"))),
                                                              br(),
                                                              tags$div(style="text-align: center; font-size: 18px", actionLink("projInfo", label = "Projections", icon = icon("chart-line"))),
                                                              br(),
                                                              tags$div(style="text-align: center; font-size: 18px", actionLink("calcInfo", label = "Calculations", icon = icon("calculator"))),
                                                              br(),
                                                              tags$div(style="text-align: center; font-size: 18px", actionLink("sourceInfo", label = "Sources", icon = icon("user-secret"))),
                                                              br(),
                                                              tags$div(style="text-align: center; font-size: 18px", actionLink("aboutInfo", label = "About", icon = icon("info-circle")))
                                             )
                                             
                                             
                                             
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
                                   $("header").find("nav").append(\'<span class="myClass"> COVID-19 Health Assessment Dashboard v11  </span>\');
                                   })
                                   ')),
                  
                  tags$style(".small-box.bg-yellow { background-color: #FFFF00 !important; color: #000000 !important; }"),
                  
                  tabsetPanel(id = "tabselected",
                              
                              
                              
                              
                              # Welcome Page
                              ######################################################################################################################
                              
                              
                              tabPanel(
                                value = 6,
                                title = "Welcome",
                                 
                                tags$div(style="text-align:center;font-size: 18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; color: black",
                                        "Welcome to the Air Force Institute of Technology (AFIT) and HAF/A9 COVID-19 Health Assessment Dashboard (CHAD)"),
                                br(),
                                tags$div(style="text-align:center; font-size=16px;font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; color: black", 
                                         "Want to learn more?  Visit the links on the left for more information."),  
                                br(),
                                tags$div(style="text-align:center; font-size=16px;font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; color: black", 
                                         "Feel free to visit the link below and give us a review!"),
                                tags$div(style="text-align:center; font-size=16px;font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; color: blue",
                                         tags$a(href="https://forms.gle/NAubEc3mbmtgQd1C8","CHAD Feedback",target = "_blank")),
                                br(),
                                
                                fluidPage(
                                          tags$style('.container-fluid {
                                          background-color: #fcfcfc;}'),
                                          uiOutput("Documentation")
                                )
                              ),
                              
                              
                              
                              
                              
                              # MAJCOM Projections Page
                              ######################################################################################################################
                              
                              
                              tabPanel(
                                value = 1,
                                title = "MAJCOM Summary",
                                
                                fluidRow(
                                  column(width = 6,
                                         leafletOutput("BaseSummaryMap")
                                         #plotlyOutput("SummaryTabChoro", height = 600, width = 'auto')

                                  ),
                                  column(width = 6,
                                         tags$div(style="text-align:center;font-size: 22px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; color: black",
                                                  "COVID-19 Installation Regional Risk Summary"),
                                         br(),
                                         fluidRow(
                                           valueBoxOutput("GreenBases", width = 6),
                                           valueBoxOutput("YellowBases", width = 6)
                                         ),
                                         fluidRow(
                                           valueBoxOutput("OrangeBases", width = 6),
                                           valueBoxOutput("RedBases", width = 6)
                                         ),
                                         br(),
                                         column(width = 6,
                                                actionLink("rskLvls", "Risk Levels", style = "text-align:center;font-size: 22px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif")
                                         ),
                                         column(width = 6,
                                                tags$div(style="text-align:center;font-size: 22px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif",
                                                tags$a(href="https://globalepidemics.org/wp-content/uploads/2020/06/key_metrics_and_indicators_v4.pdf","Reference Framework",target = "_blank"))
                                         )
                                         
                                  )
                                ),
                                  #box(plotOutput("HotSpot", height = 600))),
                                  box(title = "Base Summaries",
                                      solidHeader=T,
                                      align = "left",
                                      height = 900,
                                      width =13,
                                      
                                      column(width = 12,
                                             DT::dataTableOutput("ForecastDataTableOut"),
                                             style = "height:720px;overflow-y: scroll"),
                                      
                                  )
                                
                              ),
                              
                              
                              # International/National Map Summary Page
                              ######################################################################################################################
                              
                              
                              tabPanel(
                                value = 2,
                                title = "International/National Summary",
                                
                                box(
                                  width = 12,
                                  
                                  column(9,
                                         box(title = uiOutput("ImpactTitle"),
                                             width = NULL,
      
                                             uiOutput("ImpactText"),
                                             htmlOutput("SummaryPlot")
                                         )
                                  ),
                                  
                                  column(3, 
                                         box(title = "Top 5 Values",
                                             status = "danger",
                                             solidHeader = TRUE,
                                             width = NULL,
                                             
                                             DT::dataTableOutput("TabIncreasing")
                                         ),
                                         
                                         box(title = "Bottom 5 Values",
                                             status = "success",
                                             solidHeader = TRUE,
                                             width = NULL,
                                             
                                             DT::dataTableOutput("TabDecreasing")
                                         )
                                  )
                                ),

                                DT::dataTableOutput("NationalDataTable1")
                                
                              ),
                              
                              # Current Local Health Page
                              ######################################################################################################################
                              
                              
                              tabPanel(
                                value = 3,
                                title = "Current Local Health",
                                
                                fluidRow(
                                  valueBoxOutput("CovidCases", width = 2),
                                  valueBoxOutput("CaseChangeLocal", width = 2),
                                  valueBoxOutput("CasesPer1000", width = 2),
                                  valueBoxOutput("HospitalUtilization", width = 2),
                                  valueBoxOutput("Est_Active", width = 2),
                                  valueBoxOutput("Est_Testing", width = 2)                                  
                                ),
                                
                                fluidRow(
                                  valueBoxOutput("LocalCovidDeaths", width = 2),
                                  valueBoxOutput("DeathChangeLocal", width = 2),
                                  valueBoxOutput("CaseDbRate", width = 2),
                                  valueBoxOutput("Rt_Estimate", width = 2),
                                  valueBoxOutput("Est_Recover", width = 2),
                                  valueBoxOutput("Est_TestRate", width = 2)                                                                    
                                ),
                                downloadButton("downloadplotdata"),
                                fluidRow( 
                                  tabBox(
                                    tabPanel("Daily New Cases (3-Day)",
                                             plotlyOutput("LocalHealthPlot3day",height = 300)
                                    ),
                                    tabPanel("Daily New Cases (Raw)",
                                             plotlyOutput("LocalHealthPlot1",height = 300)
                                    ),
                                    tabPanel("Weekly Growth Rate",
                                             plotlyOutput("LocalHealthPlotWeeklyGrowth",height = 300)
                                    )
                                  ),
                                  
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

                              
                              
                              
                              # Local Health Projections Page
                              ######################################################################################################################
                              
                              
                              tabPanel(
                                value= 4,
                                title = "Local Health Projections",
                                
                                fluidRow(
                                  valueBoxOutput("TotalPopulation",width = 3),
                                  # valueBoxOutput("PeakBedDate",width = 3), 
                                  # valueBoxOutput("PeakICUDate",width = 3),
                                  # valueBoxOutput("PeakVentDate",width = 3)                                  
                                ),

                                box(uiOutput("HospLine"),
                                    textOutput("line"),
                                    plotlyOutput("OverlayPlots"),height=600, width=1500)                             
                              )

                              
                  )
                  
                ) 
                
  )
)


