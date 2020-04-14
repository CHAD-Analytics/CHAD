##################
##### Global #####
##################

# Layout
##############################################################################################################################################
# The global introduces all libraries, functions, datasets, and formatting that is necessary to pass through the server.
# First:  The libraries are loaded which have built in functions are used throughout the app.
# Second: We load data from https://github.com/treypujats/COVID19/tree/master/covid19/data ,  usafacts.org , and covid19.healthdata.org
#         The github data has static information on on all air force bases, US counties, and US hospitals. 
#         The usafacts data has dynamic information that is updated daily reporting the number of cases and number of deaths daily.
#         The IHME provides projection data of the pandemic
#         After loading the data, we format headers, establish data tables for printing, and do any static changes to the dataset for the app.
# Third:  Functions are used to execute the tasks in the server. Functions in the global are not dynamic, but they take in dynamic inputs
#         Global functions are used to calculate statistics, make data tables, plot graphs, and create visuals.
##############################################################################################################################################       



# Step One
###################################################################################################################################################
#Loads in all necessary packages for the shiny app





library(markdown)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shiny)
library(geosphere)
library(scales)
library(googleVis)
library(usmap)
library(data.table)
library(plyr)
library(DT)
library(mapproj)
library(viridis)
#library(tidyverse)
library(zoo) #used for rollsum function 
library(rmarkdown)
library(rvest)
library(maps)
library(tm)
library(plotly)
library(sf)
library(tigris)



# Step Two
###################################################################################################################################################
#Define Variables and load in data up front if necessary.
#This data updates daily with CovidConfirmedCases and CovidDeaths. These numbers are updated every day.
#The static data (countyinfo, hospitalinfo, AFBaseLocations) is used to for lat and long coordinates to measure distance.
#Hospital Data allows us to determine the bed capacity of all hospitals in the nation
#AFBaseLocations provide names and coordinates of base.
#CountyInfo is used to measure population of a county and coordinates.

CovidConfirmedCases <- as.data.frame(data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"))
CountyInfo <- as.data.frame(data.table::fread("https://github.com/treypujats/CHAD/raw/master/data/countyinfo.rda"))
HospitalInfo <- as.data.frame(data.table::fread("https://github.com/treypujats/CHAD/blob/master/data/hospitalinfo.rda?raw=true"))
CovidDeaths<-as.data.frame(data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"))
HospUtlzCounty <- read.csv("https://github.com/treypujats/CHAD/raw/master/data/county_hospitals.csv")
CountyHospRate <- read.csv("https://github.com/treypujats/CHAD/raw/master/data/CountyHospRateCalc.csv")
#himd <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/himd.rda?raw=true"))
#cimd <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/cimd.rda?raw=true"))
#AFBaseLocations <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/raw/master/covid19/data/baseinfo.rda"))


#Updated data frames to read in
githubURL <- "https://github.com/treypujats/CHAD/blob/master/data/cimd.RData?raw=true"
load(url(githubURL))

githubURL <- "https://github.com/treypujats/CHAD/blob/master/data/himd.RData?raw=true"
load(url(githubURL))

githubURL <- "https://github.com/treypujats/CHAD/blob/master/data/baseinfo.RData?raw=true"
load(url(githubURL))




#Updating data frames to ensure they are filled and match the data we reference later in the scripts
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"
colnames(CovidDeaths)[1]<-"CountyFIPS"
HospitalInfo$BEDS <- ifelse(HospitalInfo$BEDS < 0, 0, HospitalInfo$BEDS)
CovidConfirmedCases[is.na(CovidConfirmedCases)]<-0
CovidDeaths[is.na(CovidDeaths)]<-0
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"


#Read in IHME data for projecting data in the future
temp <- tempfile()
download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", temp, mode="wb")
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name[2]
IHME_Model <- read.table(unz(temp, csv_file), header = T, sep = ",")
unlink(temp)
IHME_Model$date <- as.Date(IHME_Model$date, format = "%Y-%m-%d")
StateList <- data.frame(state.name, state.abb)
IHME_Model <- merge(IHME_Model, StateList, by.x = names(IHME_Model)[2], by.y = names(StateList)[1])
names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"


#Create list of hospitals, bases, and counties.
BaseList<-sort(AFBaseLocations$Base, decreasing = FALSE)
HospitalList <- HospitalInfo$NAME
CountyList <- CountyInfo$County
MAJCOMList <- sort(unique(AFBaseLocations$`Major Command`), decreasing = FALSE)
MAJCOMList<-c("All",'Active Duty',MAJCOMList)

#Calculate county case doubling rate for most recent day
CovidConfirmedCases <- dplyr::filter(CovidConfirmedCases, CountyFIPS != 0)
CovidConfirmedCases <- head(CovidConfirmedCases,-1)

currCount = 0

v <- rep(0, as.numeric(ncol(CovidConfirmedCases)))

for (i in 1:nrow(CovidConfirmedCases)){
    
    j = 0
    cases = CovidConfirmedCases[i,ncol(CovidConfirmedCases)]
    
    if (cases != 0){
        
        while (cases/2 < CovidConfirmedCases[i,ncol(CovidConfirmedCases)-j])
        {
            j = j + 1
        }
        
        days = j
    } 
    else{
        days = 0
    }
    
    v[i] <- days
    
}

CovidConfirmedCasesRate <- cbind(CovidConfirmedCases,v)


######################Data Specific to plotting counties and states as choropleth

#Input the Included Counties as factors
# PlottingCountyData<- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
#                               header = TRUE, stringsAsFactors = FALSE)
# PlottingCountyData$county <- tolower(gsub("([A-Za-z]+).*", "\\1", PlottingCountyData$County.Name))
# PlottingCountyData$county <- gsub("^(.*) parish, ..$","\\1", PlottingCountyData$county)
# #Creating state name in addition to state abb
# PlottingCountyData<-PlottingCountyData %>% 
#     mutate(state_name = tolower(state.name[match(State, state.abb)]))
# #Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
# county_df <- map_data("county")
# names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
# county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
# county_df$state_name <- NULL
# #Calling in state data so we can map it correctly
# state_df <- map_data("state", projection = "albers", parameters = c(39, 45))
# colnames(county_df)[6]<-"State"
#Input the Included Counties as factors
PlottingCountyData<- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
                              header = TRUE, stringsAsFactors = FALSE)

# stopwords = "County"     #Your stop words file
# x  = PlottingCountyData$County.Name        #Company column data
# x  =  removeWords(x,stopwords)     #Remove stopwords
# 
# df$company_new <- x     #Add the list as new column and check


PlottingCountyData$county <- tolower(removeWords(PlottingCountyData$County.Name,"County"))
PlottingCountyData$county <-gsub(" ", "" ,PlottingCountyData$county)
PlottingCountyData$county <- gsub("^(.*) parish, ..$","\\1", PlottingCountyData$county)

#Creating state name in addition to state abb
PlottingCountyData<-PlottingCountyData %>% 
  mutate(state_name = tolower(state.name[match(State, state.abb)]))
PlottingCountyData<-data.frame(PlottingCountyData[,1],rev(PlottingCountyData)[,3])
colnames(PlottingCountyData)<-c("GEOID","Cases")
#Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
county_df<-counties(state = NULL, cb = TRUE, resolution = "5m")




#######################Create National Data table on summary page

NationalDataTable<-CovidConfirmedCases
NationalDataTable$State<-as.factor(NationalDataTable$State)
NationalDataTable<-NationalDataTable[,-c(1,2,4)]
NationalDataTable<-aggregate(.~State, NationalDataTable, sum)
RateofCovidChange<-rev(NationalDataTable)[c(1:7)]
RateofCovidChange<-ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)

NationalDeathTable<-CovidDeaths
NationalDeathTable$State<-as.factor(NationalDeathTable$State)
NationalDeathTable<-NationalDeathTable[,-c(1,2,4)]
NationalDeathTable<-aggregate(.~State, NationalDeathTable, sum)
RateofDeathChange<-rev(NationalDeathTable)[c(1:7)]
RateofDeathChange<-ceiling(rowSums(RateofDeathChange[1:6]-RateofDeathChange[2:7])/6)

NationalDataTable<-data.frame(NationalDataTable$State, NationalDataTable[,length(NationalDataTable)],RateofCovidChange, NationalDeathTable[,length(NationalDeathTable)], RateofDeathChange)
colnames(NationalDataTable)<-c("State","Total Cases","Average New Cases Per Day", "Total Deaths","Average New Deaths Per Day")
NationalDataTable$`Cases Per 100,000 People`<-c(731545,4903185,3017825,7278717,39512223,5758736,3565287,705749,973764,21477737,10617423,1415872,3155070,1787065,12671821,6732219,2913314,4467673,4648794,6949503,6045680,1344212,9986857,5639632,6137428,2976149,1068778,10488084,762062,1934408,1359711,8882190,2096829,3080156,19453561,11689100,3956971,4217737,12801989,1059361,5148714,884659,6833174,28995881,3205958,8535519,623989,7614893,5822434,1792147,578759)
NationalDataTable$`Cases Per 100,000 People`<-round(NationalDataTable$`Total Cases`/(NationalDataTable$`Cases Per 100,000 People`/100000))



# beds <- read.csv('beds.csv')
# pops <- read.csv('pops.csv')
# source('acme_support.R')
##########################################################################################################
##########################################################################################################
##########################################################################################################
############################################################################################################################################



#Use army models to create projections for the local area around the base
#Establish function for army SEIAR model. This allows us to pass though a simple function to gather all statistics when we plot
SEIAR_Model_Run<-function(num_init_cases, Pop.At.Risk, incub_period, latent_period, 
                          doubling, recovery_days, social_rate, hospital_rate,
                          icu_rate, ventilated_rate, hospital_dur, icu_dur, ventilated_dur, n_days, 
                          secondary_cases = 2.5, distribution_e_to_a = 0.5){
    
    ###DEFINING COMPARTMENTS OF THE MODEL
    total_infections <- num_init_cases / (hospital_rate/100) 
    I <- total_infections 
    S <- (Pop.At.Risk - I)
    E <- (total_infections*secondary_cases) * distribution_e_to_a #Assuming that each infectious person will generate 2.5 secondary cases
    A <- (total_infections*secondary_cases) * (1-distribution_e_to_a) ##Distributing the secondary cases between the E and A compartments
    R <- 0
    
    ###DEFINING PARAMETERS
    intrinsic_growth_rate <- 2 ^(1 / doubling) -1
    sigma <- 1/latent_period #Latent period (approx 2 days)
    gamma_1 <- 1/(incub_period - latent_period)
    gamma_2 <- 1/(recovery_days - latent_period - (incub_period - latent_period))
    beta <- (intrinsic_growth_rate + (1/recovery_days)) / S * (1-social_rate/100)
    r_t <- beta / (1/recovery_days) * S 
    r_0 <- r_t / (1 - social_rate/100)
    doubling_time_t <- 1 / log2(beta*S - (1/recovery_days) + 1)
    
    ###ITERATIVE LISTING OF MODEL
    myList <- list()
    myList$total_infections <- total_infections
    myList$S <- S
    myList$E <- E
    myList$A <- A
    myList$I <- I
    myList$R <- R
    myList$intrinsic_growth_rate <- intrinsic_growth_rate
    myList$sigma <- sigma
    myList$gamma_1 <- gamma_1
    myList$gamma_2 <- gamma_2
    myList$beta <- beta
    myList$r_t <- r_t
    myList$r_0 <- r_0
    myList$doubling_time_t <- doubling_time_t
    
    #initial values
    N = S + E + A + I + R
    hos_add <- ((A+I) * hospital_rate/100)
    hos_cum <- ((A+I) * hospital_rate/100)
    icu_add <- (hos_add * icu_rate/100)
    icu_cum <- (hos_cum * icu_rate/100)
    
    #create the data frame
    sir_data <- data.frame(t = 1,
                           S = S,
                           E = E,
                           A = A,
                           I = I,
                           R = R,
                           hos_add = hos_add,
                           hos_cum = hos_cum,
                           icu_add = hos_add * icu_rate/100,
                           icu_cum = hos_cum * icu_rate/100,
                           vent_add = icu_add * ventilated_rate/100,
                           vent_cum = icu_cum * ventilated_rate/100,
                           Id = 0
    )
    
    for(i in 2:n_days){
        y <- seiar(S,E,A,I,R, beta, sigma, gamma_1, gamma_2, N)
        S <- y$S
        E <- y$E
        A <- y$A
        I <- y$I
        R <- y$R
        
        #calculate new infections
        Id <- (sir_data$S[i-1] - S)
        
        #portion of the the newly infected that are in the hospital, ICU, and Vent
        hos_add <- Id * hospital_rate/100
        hos_cum <- sir_data$hos_cum[i-1] + hos_add
        
        icu_add <- hos_add * icu_rate/100
        icu_cum <- sir_data$icu_cum[i-1] + icu_add
        
        vent_add <- icu_add * ventilated_rate/100 
        vent_cum <- sir_data$vent_cum[i-1] + vent_add
        
        temp <- data.frame(t = i,
                           S = S,
                           E = E,
                           A = A,
                           I = I,
                           R = R,
                           hos_add = hos_add,
                           hos_cum = hos_cum,
                           icu_add = icu_add,
                           icu_cum = icu_cum,
                           vent_add = vent_add,
                           vent_cum = vent_cum,
                           Id = Id
        )
        
        sir_data <- rbind(sir_data,temp)
    }
    
    #doing some weird stuff to get a rolling sum of hospital impacts based on length of stay (los)
    if(n_days > hospital_dur){
        h_c <- rollsum(sir_data$hos_add,hospital_dur)
        sir_data$hos_cum <- c(sir_data$hos_cum[1:(n_days - length(h_c))],h_c)
    } 
    if(n_days > icu_dur){
        i_c <- rollsum(sir_data$icu_add,icu_dur)
        sir_data$icu_cum <- c(sir_data$icu_cum[1:(n_days - length(i_c))],i_c)
    } 
    if(n_days > ventilated_dur){
        v_c <- rollsum(sir_data$vent_add,ventilated_dur)
        sir_data$vent_cum <- c(sir_data$vent_cum[1:(n_days - length(v_c))],v_c)
    } 
    
    #write.csv(sir_data, file = 'test.csv') # for testing
    h_m <- round(max(sir_data$hos_cum), 0)
    i_m <- round(max(sir_data$icu_cum), 0)
    v_m <- round(max(sir_data$vent_cum), 0)
    myList$sir <- sir_data
    myList$hos_max <- h_m
    myList$icu_max <- i_m
    myList$vent_max <- v_m 
    
    h_m <- sir_data$t[which.max(sir_data$hos_cum)][1]
    i_m <- sir_data$t[which.max(sir_data$icu_cum)][1]
    v_m <- sir_data$t[which.max(sir_data$vent_cum)][1]
    myList$hos_t_max <- h_m
    myList$icu_t_max <- i_m
    myList$vent_t_max <- v_m 
    
    h_m <- round(max(sir_data$hos_add), 0)
    i_m <- round(max(sir_data$icu_add), 0)
    v_m <- round(max(sir_data$vent_add), 0)
    
    myList$hos_add <- h_m
    myList$icu_add <- i_m
    myList$vent_add <- v_m 
    return(myList)
}

seiar<-function(S,E,A,I,R, beta, sigma, gamma_1, gamma_2, N){
    Sn <- (-beta * S * (A + I)) + S
    En <- ((beta * S * (A + I)) - (sigma * E)) + E
    An <- ((sigma * E) - (gamma_1 * A)) + A
    In <- ((gamma_1 * A) - (gamma_2 * I)) + I
    Rn <- (gamma_2 * I) + R
    
    if(Sn < 0) Sn = 0
    if(En < 0) En = 0
    if(An < 0) An = 0
    if(In < 0) In = 0
    if(Rn < 0) Rn = 0
    
    scale = N / (Sn + En + An + In + Rn)
    myListSIR <- list()
    myListSIR$S <- (Sn * scale)
    myListSIR$E <- (En * scale)
    myListSIR$A <- (An * scale)
    myListSIR$I <- (In * scale)
    myListSIR$R <- (Rn * scale)
    return(myListSIR)
}


#######################################################
############### Helper Functions ######################
#######################################################

GetCounties<-function(base,radius){
    
    #Find counties in radius
    CountyInfo$DistanceMiles = cimd[,as.character(base)]
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius)
    IncludedCounties
}

GetHospitals<-function(base,radius){
  
    #Find number of hospitals in radius
    HospitalInfo$DistanceMiles = himd[,as.character(base)]
    IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= radius))
    IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
    IncludedHospitals
}



###################################################################################################################################################
# Statistics for Local Health Page -------------------------------------------------------------------------------------------------------------------------------------


CalculateCounties<-function(IncludedCounties){
  
    #Get the total population in the selected region
    TotalPopulation <-  sum(IncludedCounties$Population)
    TotalPopulation
}

CalculateCovid<-function(IncludedCounties){
  
    #Get total confirmed cases in the selected region
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    sum(rev(CovidCounties)[,1])
}

CalculateDeaths<-function(IncludedCounties){
  
    #Get total deaths in the selected region
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    sum(rev(CovidCountiesDeath)[,1])
}

HospitalIncreases<-function(IncludedCounties){
    
    #Find hospitals in selected region
    hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
    
    #Calculate total beds and weighted average utilization
    TotalBeds<-sum(hospCounty$num_staffed_beds)
    hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
    totalUsedBeds <- sum(hospCounty$bedsUsed)
    baseUtlz <- totalUsedBeds/TotalBeds
    
    #Get COVID cases and county demographic hospitalization rates
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)

    #Estimate current hospital utilization
    TotalHospital<-sum(rev(CovidCounties)[,1]*CovidCountiesHospRate$HospRate)
    NotHospital<-sum(rev(CovidCounties)[,7]**CovidCountiesHospRate$HospRate)
    StillHospital<-ceiling((TotalHospital-NotHospital))
    Utilz<- round(((StillHospital)/TotalBeds+baseUtlz)*100,0)
    
    paste(Utilz," %", sep = "") 
}


# HospitalUtlzChng <- function(IncludedCounties){
#   
#     #Find hospitals in selected region
#     hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
#     
#     #Calculate total beds and weighted average utilization
#     TotalBeds<-sum(hospCounty$num_staffed_beds)
#     hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
#     totalUsedBeds <- sum(hospCounty$bedsUsed)
#     baseUtlz <- totalUsedBeds/TotalBeds
#     
#     #Get COVID cases and county demographic hospitalization rates
#     CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
#     CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
#     
# 
#     #Estimate current hospital utilization
#     TotalHospital<-sum(rev(CovidCounties)[,1]*CovidCountiesHospRate$HospRate)
#     NotHospital<-sum(rev(CovidCounties)[,7]**CovidCountiesHospRate$HospRate)
#     StillHospital<-ceiling((TotalHospital-NotHospital))
#     Utilz<- round((baseUtlz - (StillHospital)/TotalBedsbaseUtlz)*100,0)
#     
#     # # Yesterday's utilization
#     # TotalHospitaly<-sum(rev(CovidCounties)[,2]*CovidCountiesHospRate$HospRate)
#     # NotHospitaly<-sum(rev(CovidCounties)[,8]*CovidCountiesHospRate$HospRate)
#     # StillHospitaly<-ceiling((TotalHospitaly-NotHospitaly))
#     # Utilzy<-(signif(((StillHospitaly)/TotalBeds+baseUtlz)*100,3))
#     # 
#     # # find change
#     # chng <- round((Utilz-Utilzy)/2, 1)
#     
#     # if (chng < 0) {
#     #   sign <- ""
#     # } else {
#     #   sign <- "+"
#     # }
#     # 
#     paste(Utilz,"%")
# }


CalculateCHIMEPeak<-function(IncludedCounties, ChosenBase, ChosenRadius, SocialDistance, ProjectedDays, StatisticType){
  if (StatisticType == "Hospitalizations") {

    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases","Maximum Daily Cases")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-ProjectedDays
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData<-DailyData[-1,]
    DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
    
    Peak<-which.max(DailyData$`Expected Daily Cases`)
    Peak<-DailyData[Peak,2]
    round(Peak)
  } else {
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidDeathsHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidDeathsHist[,5:length(CovidDeathsHist)])
    
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-ProjectedDays
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Fatalities")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData$`Expected Fatalities` <- round(DailyData$`Expected Fatalities`*(.25/5.5),0)
    DailyData<-DailyData[-1,]
    DailyData$`Expected Fatalities`<-cumsum(DailyData$`Expected Fatalities`)
    DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
    max(DailyData$`Expected Fatalities`)

  }
    
}

# CalculateCHIMEMinMax<-function(IncludedCounties, ChosenBase, ChosenRadius, SocialDistance, ProjectedDays){
#     BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
#     #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
#     #We include State, county, population in those counties, cases, fatalities, doubling rate
#     CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
#     HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
#     HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
#     HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
#     colnames(HistoricalData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases","Maximum Daily Cases")
#     
#     DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
#     CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
#     CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
#     CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
#     colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
#     
#     #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
#     #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
#     ActiveCases<-rev(CovidCounties)[1:7]
#     ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
#     colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
#     SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
#     colnames(SIRinputs)<-c("cases","pop","doubling")
#     
#     
#     ####################################################################################
#     #Mean Estimate
#     
#     #Next we use the calculated values, along with estimated values from the Estimated Values. 
#     #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
#     cases<-SIRinputs$cases
#     pop<-SIRinputs$pop
#     doubling<-8
#     
#     #Established Variables at the start for every county or populations
#     Ro<-2.5
#     incubationtime<-5
#     latenttime<-2
#     recoverydays<-14
#     socialdistancing<-SocialDistance
#     hospitalizationrate<-5
#     icurate<-6
#     ventilatorrate<-3
#     hospitaltime<-3.5
#     icutime<-4
#     ventilatortime<-7
#     daysforecasted<-ProjectedDays
#     
#     
#     #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
#     #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
#     SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
#                                socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
#                                ventilatortime,daysforecasted,Ro, .5)
#     
#     MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
#     DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
#     TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
#     colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
#     colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
#     DailyData<-DailyData[-1,]
#     DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
#     
#     Peak<-which.max(DailyData$`Expected Daily Cases`)
#     Max<-DailyData[Peak,4]
#     Min<-DailyData[Peak,3]
#     paste("(Min: ", Min, ", Max: ", Max, ")")
#     
# }

CalculateIHMEPeak<-function(ChosenBase, IncludedHospitals, radius, StatisticType){
    
    if (StatisticType == "Hospitalizations") {
      #Creating the stats and dataframes determined by the base we choose to look at.
      BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
      IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
      TotalBedsCounty <- sum(IncludedHospitals$BEDS)
      
      #Get regional and state populations
      CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
      IncludedCounties <- dplyr::filter(CountyInfo, DistanceMiles <= radius)
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
      RegPop <- sum(IncludedCounties$Population)
      StPop <- sum(StPopList$Population)
      
      # Use Population ratio to scale IHME
      PopRatio <- RegPop/StPop
      
      # Get total hospital bed number across state
      IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
      TotalBedsState <- sum(IncludedHospitalsST$BEDS)
      
      # Calculate bed ratio
      BedProp <- TotalBedsCounty/TotalBedsState
      
      # Apply ratio's to IHME data
      IHME_Region <- IHME_State
      IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean)
      
      PeakDate<-which.max(IHME_Data$IHME_Region.allbed_mean)
      Peak<-IHME_Data[PeakDate,2]
      round(Peak)
    } else {
      BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
      IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
      TotalBedsCounty <- sum(IncludedHospitals$BEDS)
      MyCounties<-GetCounties(ChosenBase, radius)
      #Get regional and state populations
      CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
      CovidDeathsHist<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
      HistoricalData<-colSums(CovidDeathsHist[,5:length(CovidDeathsHist)])
      HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
      HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
      colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
      
      
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
      RegPop <- sum(MyCounties$Population)
      StPop <- sum(StPopList$Population)
      
      # Use Population ratio to scale IHME
      PopRatio <- RegPop/StPop
      
      # Get total hospital bed number across state
      IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
      TotalBedsState <- sum(IncludedHospitalsST$BEDS)
      
      # Calculate bed ratio
      BedProp <- TotalBedsCounty/TotalBedsState
      
      # Apply ratio's to IHME data
      IHME_Region <- IHME_State
      IHME_Region$deaths_mean = round(IHME_State$totdea_mean*PopRatio)
      IHME_Region$deaths_lower = round(IHME_State$totdea_lower*PopRatio)
      IHME_Region$deaths_upper = round(IHME_State$totdea_upper*PopRatio)
      IHME_Region<-data.frame(IHME_Region$date, IHME_Region$deaths_mean, IHME_Region$deaths_lower, IHME_Region$deaths_upper)
      colnames(IHME_Region)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
      max(IHME_Region$`Expected Fatalities`)
    }
    
}

# CalculateIHMEMinMax<-function(ChosenBase, IncludedHospitals, radius){
# 
#     #Creating the stats and dataframes determined by the base we choose to look at.
#     BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
#     IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
#     TotalBedsCounty <- sum(IncludedHospitals$BEDS)
# 
#     #Get regional and state populations
#     CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
#     IncludedCounties <- dplyr::filter(CountyInfo, DistanceMiles <= radius)
#     StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
#     RegPop <- sum(IncludedCounties$Population)
#     StPop <- sum(StPopList$Population)
# 
#     # Use Population ratio to scale IHME
#     PopRatio <- RegPop/StPop
# 
#     # Get total hospital bed number across state
#     IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
#     TotalBedsState <- sum(IncludedHospitalsST$BEDS)
# 
#     # Calculate bed ratio
#     BedProp <- TotalBedsCounty/TotalBedsState
# 
#     # Apply ratio's to IHME data
#     IHME_Region <- IHME_State
#     IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean*PopRatio, IHME_Region$allbed_lower*PopRatio, IHME_Region$allbed_upper*PopRatio)
#     colnames(IHME_Data)<-c("Date", "Mean", "Lower", "Upper")
#     
#     PeakDate<-which.max(IHME_Data$Mean)
#     Max<-round(IHME_Data[PeakDate,4])
#     Min<-round(IHME_Data[PeakDate,3])
#     paste("Min:", Min, ", Max:", Max)
# }




##########################################################################################################
##########################################################################################################
##########################################################################################################
# Create Charts for plotting lines showing trends among the virus  ------------------------------------------------------------------------------------------------------------------



#Create charts for Local Health Tab


#This function creates the dataframe for plotting daily cases, deaths, estimated hospitalizations in selected region
CovidCasesPerDayChart<-function(IncludedCounties){
      
    #Get cases and deaths in selected region
    CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
    
    #Find Daily new cases
    DailyNewCases <- CovidCountiesCases[,6:length(CovidCountiesCases)] -
      CovidCountiesCases[,5:(length(CovidCountiesCases)-1)]
    DailyNewCasesT <- colSums(DailyNewCases)
    
    #Find New Deaths
    DailyNewDeaths <- CovidCountiesDeath[,6:length(CovidCountiesDeath)] -
      CovidCountiesDeath[,5:(length(CovidCountiesDeath)-1)]
    DailyNewDeathsT <- colSums(DailyNewDeaths)
    
    #Estimation for new hospitalizations
    DailyNewHospitalizations<-ceiling(colSums(DailyNewCases*CovidCountiesHospRate$HospRate))
    
    #Clean up the dataset to prepare for plotting
    ForecastDate<- seq(as.Date("2020-1-23"), length=length(DailyNewCases), by="1 day")
    Chart1Data<-cbind.data.frame(ForecastDate,DailyNewCasesT,DailyNewHospitalizations,DailyNewDeathsT)
    colnames(Chart1Data)<-c("ForecastDate","New Cases","New Hospitalizations","New Fatalities")
    Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
}



#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesCumChart<-function(IncludedCounties){
    
    #Find counties in radius
    CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
    
    #Compute cumlative cases and deaths in selected counties
    CumDailyCovid<-colSums(CovidCountiesCases[,5:length(CovidCountiesCases)])
    CumDailyDeaths<-colSums(CovidCountiesDeath[5:length(CovidCountiesDeath)])
    
    #Estimation for total hospitalizations
    CumHospitalizations<-ceiling(colSums(CovidCountiesCases[,5:length(CovidCountiesCases)]*CovidCountiesHospRate$HospRate))
    
    #Clean up the dataset to get ready to plot it
    ForecastDate<- seq(as.Date("2020-1-23"), length=length(CumDailyCovid), by="1 day")
    Chart2Data<-cbind.data.frame(ForecastDate,CumDailyCovid,CumHospitalizations,CumDailyDeaths)
    colnames(Chart2Data)<-c("ForecastDate","Total Cases","Total Hospitalizations","Total Fatalities")
    Chart2DataSub <- melt(data.table(Chart2Data), id=c("ForecastDate"))
}

#Create charts for projecting local health data
##########################################################################################################
##########################################################################################################
##########################################################################################################
PlotOverlay<-function(ChosenBase, IncludedCounties, IncludedHospitals, SocialDistance, DaysProjected, StatisticType){
    if (StatisticType == "Hospitalizations") {
        
      #Establish initial inputs such as base, counties, and filter IHME model
      BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
      IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
      hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
      TTBCounty <- sum(IncludedHospitals$BEDS)
      
      #Get covid cases and hospitalization rates for county
      CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
      CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
      
      #Get past data in daily hospital use
      #This will use a 5 day hospital stay as the average
      HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] -
        CovidCounties[,5:(length(CovidCounties)-5)]
      HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)
      
      #Create dataframe to hold daily hospitalizations
      HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
      HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
      colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      
      #Get regional and state populations
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
      RegPop <- sum(IncludedCounties$Population)
      StPop <- sum(StPopList$Population)
      
      # Use Population ratio to scale IHME
      PopRatio <- RegPop/StPop
      
      
      # Apply ratio's to IHME data
      IHME_Region <- IHME_State
      IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
      IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
      IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
      
      DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
      CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
      CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
      CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
      colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
      
      #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
      #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
      ActiveCases<-rev(CovidCounties)[1:7]
      ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
      colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
      SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
      colnames(SIRinputs)<-c("cases","pop","doubling")
      
      
      ####################################################################################
      #Mean Estimate
      
      #Next we use the calculated values, along with estimated values from the Estimated Values. 
      #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
      cases<-SIRinputs$cases
      pop<-SIRinputs$pop
      doubling<-8
      
      #Established Variables at the start for every county or populations
      Ro<-2.5
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      socialdistancing<-SocialDistance
      hospitalizationrate<-5
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-3.5
      icutime<-4
      ventilatortime<-7
      daysforecasted<-DaysProjected
      
      
      #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
      #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
      SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                 socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                                 ventilatortime,daysforecasted,Ro, .5)
      
      MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
      DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
      TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
      colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
      colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
      
      
      ####################################################################################
      #Lower Estimate
      
      #Next we use the calculated values, along with estimated values from the Estimated Values. 
      #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
      cases<-SIRinputs$cases
      pop<-SIRinputs$pop
      doubling<-10
      
      #Established Variables at the start for every county or populations
      Ro<-2.5
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      socialdistancing<-SocialDistance
      hospitalizationrate<-5
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-3.5
      icutime<-4
      ventilatortime<-7
      daysforecasted<-DaysProjected
      
      
      
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
      cases<-SIRinputs$cases
      pop<-SIRinputs$pop
      doubling<-7
      
      #Established Variables at the start for every county or populations
      Ro<-2.5
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      socialdistancing<-SocialDistance
      hospitalizationrate<-5.5
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-3.5
      icutime<-4
      ventilatortime<-7
      daysforecasted<-DaysProjected
      
      #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
      #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
      SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                 socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                 icutime,ventilatortime,daysforecasted,Ro, .5)
      
      DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
      TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
      colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Estimate","Upper Estimate")
      colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
      
      DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
      DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
      DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
      DailyData<-DailyData[-1,]
      colnames(IHME_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      DailyData$ID<-rep("CHIME",nrow(DailyData))
      IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
      HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
      HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
      OverlayData<-rbind(DailyData,IHME_Data)
      OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
      
      OverlayData<- dplyr::filter(OverlayData, ForecastDate >= Sys.Date() & ForecastDate <= (Sys.Date() + DaysProjected))
      
      OverlayData<-rbind(HistoricalData, OverlayData)
      
      
      
      hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
      #Finds number of hospitals in radius
      TotalBeds<-sum(hospCounty$num_staffed_beds)
      #get historic utilization
      hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
      totalUsedBeds <- sum(hospCounty$bedsUsed)
      baseUtlz <- totalUsedBeds/TotalBeds
      
      
      projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
        geom_line() + 
        scale_colour_manual(values=c("tan", "blue", "black","red"))+
        scale_fill_manual(values = c("tan4", "cadetblue", "gray", "red"))+
        scale_linetype_manual(values=c("dashed", "dashed", "solid", "solid"))+
        geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                    alpha = .2) +
        geom_hline(aes(yintercept = TotalBeds * (1-baseUtlz),
                       linetype = "Max Available Hospital Beds"),
                       colour = "red") +
        ggtitle("Projected Daily Hospital Bed Utilization")+
        ylab("Daily Beds Needed")+
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
        labs(color = "ID")
      
      
      projections <- ggplotly(projections)
      projections <- projections %>% config(displayModeBar = FALSE)
      projections
        
    } else {
        
        #Creating the stats and dataframes determined by the base we choose to look at.
        BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        TotalBedsCounty <- sum(IncludedHospitals$BEDS)
        
        #Get regional and state populations
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
        RegPop <- sum(IncludedCounties$Population)
        StPop <- sum(StPopList$Population)
        
        # Use Population ratio to scale IHME
        PopRatio <- RegPop/StPop
        
        # Get total hospital bed number across state
        IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
        TotalBedsState <- sum(IncludedHospitalsST$BEDS)
        
        # Calculate bed ratio
        BedProp <- TotalBedsCounty/TotalBedsState
        
        # Apply ratio's to IHME data
        IHME_Region <- IHME_State
        IHME_Region$deaths_mean = round(IHME_State$totdea_mean*PopRatio)
        IHME_Region$deaths_lower = round(IHME_State$totdea_lower*PopRatio)
        IHME_Region$deaths_upper = round(IHME_State$totdea_upper*PopRatio)
        
        IHME_Data<-data.frame(IHME_Region$date,IHME_Region$deaths_mean, IHME_Region$deaths_lower, IHME_Region$deaths_upper)
        
        BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
        #We include State, county, population in those counties, cases, fatalities, doubling rate
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
        CovidDeathHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
        HistoricalData<-colSums(CovidDeathHist[,5:length(CovidDeathHist)])
        HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        
        DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
        CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
        CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
        CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
        colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
        
        #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
        #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
        ActiveCases<-rev(CovidCounties)[1:7]
        ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
        colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
        SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
        colnames(SIRinputs)<-c("cases","pop","doubling")
        
        
        ####################################################################################
        #Mean Estimate
        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-8
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-SocialDistance
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-DaysProjected
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                                   ventilatortime,daysforecasted,Ro, .5)
        
        MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
        DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
        TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
        
        
        ####################################################################################
        #Lower Estimate
        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-10
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-SocialDistance
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-DaysProjected
        
        
        
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
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-7
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-SocialDistance
        hospitalizationrate<-5.5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-DaysProjected
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
        
        DailyData$`Expected Fatalities` <- round(DailyData$`Expected Fatalities`*(.25/5.5),0)
        DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`*(.15/4),0)
        DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`*(1/8),0)
        DailyData<-DailyData[-1,]
        DailyData$`Expected Fatalities`<-cumsum(DailyData$`Expected Fatalities`)
        DailyData$`Lower Estimate`<-cumsum(DailyData$`Lower Estimate`)
        DailyData$`Upper Estimate`<-cumsum(DailyData$`Upper Estimate`)
        
        colnames(IHME_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        DailyData$ID<-rep("CHIME",nrow(DailyData))
        IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        OverlayData<-rbind(DailyData,IHME_Data)
        OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
        
        OverlayData<- dplyr::filter(OverlayData, ForecastDate >= Sys.Date() & ForecastDate <= (Sys.Date() + DaysProjected) )
        
        OverlayData<-rbind(HistoricalData, OverlayData)
        
        
        
        
        projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Fatalities`, color = ID, fill = ID, linetype = ID)) +
            geom_line() + 
            scale_colour_manual(values=c("tan", "blue", "black"))+
            scale_fill_manual(values = c("tan4", "cadetblue", "gray"))+
            scale_linetype_manual(values = c("dashed", "dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            ggtitle("Projected Fatalities")+
            ylab("Fatalities")+
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
            labs(color = "ID")
        
        
        projections <- ggplotly(projections)
        projections <- projections %>% config(displayModeBar = FALSE)
        projections
    }
        
    }
    




# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------

#Create Data Table for local statistics
GetLocalDataTable<-function(IncludedCounties){
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    CountyDataTable
}



# Create choropleth functions -------------------------------------------------------------------------------------------------------------------------------------------------------

#Create plot of Covid Cases by County
PlotLocalChoro<-function(IncludedCounties, ChosenBase, TypofPlot){
  
  if (TypofPlot == "County") {
    choropleth <- st_as_sf(county_df)
    choropleth <- st_transform(choropleth, crs = 4326)
    choropleth<-choropleth %>% 
      mutate(GEOID = as.numeric(GEOID))
    choropleth<-subset(choropleth, GEOID %in% IncludedCounties$FIPS)
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    Base_point<-st_point(c(BaseStats$Long, BaseStats$Lat)) #COrdinates for base
    Base_point<-st_sfc(Base_point, crs=4326)
    Base_point<-st_sf(BaseStats, geometry = Base_point)
    
    ## Join the cases to spatial file by FIPS (GEOID)
    choropleth<-merge(choropleth, PlottingCountyData, by= "GEOID")
    
    #plot it
    PlotCovidLocal<-ggplot()+
      geom_sf(data = choropleth,aes(fill=Cases)) +
      geom_sf(data = Base_point, color = "red", size = 3,show.legend ="Null")+
      #geom_text(data = Base_point, 
      #          aes(x = Long, y = Lat, 
      #              label = Base), hjust = .5) +
      ggtitle("COVID-19 Cases by County (County View)")+ 
      coord_sf() +
      theme_minimal() +
      theme(axis.line = element_blank(), axis.text = element_blank(),
            axis.ticks = element_blank(), axis.title = element_blank())+
      scale_fill_viridis(choropleth$Cases)
    
    PlotCovidLocal <- ggplotly(PlotCovidLocal)
    PlotCovidLocal <- PlotCovidLocal %>% config(displayModeBar = FALSE)
    PlotCovidLocal
    
  } else  {
    choropleth <- st_as_sf(county_df)
    choropleth <- st_transform(choropleth, crs = 4326)
    choropleth<-choropleth %>% 
      mutate(STATEFP = state.fips$abb[match(as.numeric(STATEFP), as.numeric(state.fips$fips))])
    choropleth<-choropleth %>% 
      mutate(GEOID = as.numeric(GEOID))
    choropleth<-subset(choropleth, STATEFP %in% IncludedCounties$State)
    choropleth<-merge(choropleth, PlottingCountyData, by= "GEOID")
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    Base_point<-st_point(c(BaseStats$Long, BaseStats$Lat)) #COrdinates for base
    Base_point<-st_sfc(Base_point, crs=4326)
    Base_point<-st_sf(BaseStats, geometry = Base_point)
    
    ## Join the cases to spatial file by FIPS (GEOID)
    
    
    
    PlotCovidLocal<-ggplot()+
      geom_sf(data = choropleth,aes(fill=Cases)) +
      geom_sf(data = Base_point, color = "red", size = 3,show.legend ="Null")+
      #geom_text(data = Base_point, 
      #          aes(x = Long, y = Lat, 
      #              label = Base), hjust = .5) +
      ggtitle("COVID-19 Cases by County (County View)")+ 
      coord_sf() +
      theme_minimal() +
      theme(axis.line = element_blank(), axis.text = element_blank(),
            axis.ticks = element_blank(), axis.title = element_blank())+
      scale_fill_viridis(choropleth$Cases)
    PlotCovidLocal
  }
  
  
}



# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------

#Create Data Table for local statistics
GetLocalDataTable<-function(IncludedCounties){
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1],round(CountyDataTable$DistanceMiles,0))
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)", "Distance" )
    CountyDataTable1 <- CountyDataTable[order(CountyDataTable$Distance),]
    CountyDataTable1
}



# Create choropleth functions -------------------------------------------------------------------------------------------------------------------------------------------------------

# #Create plot of Covid Cases by County
# PlotLocalChoro<-function(IncludedCounties, ChosenBase, TypofPlot){
#     
#     if (TypofPlot == "County") {
#         BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
#         #Creating the choropleth dataset so we have all info in one data set and can plot it together
#         choropleth <- merge(county_df, PlottingCountyData, by = c("county", "State"))
#         colnames(choropleth)[7]<-"CountyFIPS"
#         choropleth <- choropleth[order(choropleth$order), ]
#         choropleth$state_name<-NULL
#         choropleth<-data.frame(choropleth$county, choropleth$State, choropleth$CountyFIPS, choropleth$group, choropleth$lat, choropleth$long, rev(choropleth)[,1])
#         colnames(choropleth)<-c("County","State","CountyFIPS","group","lat","long","Cases")
#         choropleth<-subset(choropleth, CountyFIPS %in% IncludedCounties$FIPS)
#         
#         #Plot the data
#         PlotCovidLocal<-ggplot(choropleth, aes(long, lat, group = group)) +
#             geom_polygon(aes(fill = Cases)) +
#             coord_fixed() +
#             theme_minimal() +
#             ggtitle("COVID-19 Cases by County (County View)") +
#             geom_point(data = BaseStats, aes(x=Long, y=Lat, group = 1),
#                        color = 'red', size = 5)+
#             theme(axis.line = element_blank(), axis.text = element_blank(),
#                   axis.ticks = element_blank(), axis.title = element_blank()) +
#             scale_fill_viridis("Cases")
#         
#         ggplotly(PlotCovidLocal)
#         
#     } else  {
#         BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
#         #Creating the choropleth dataset so we have all info in one data set and can plot it together
#         choropleth <- merge(county_df, PlottingCountyData, by = c("county", "State"))
#         colnames(choropleth)[7]<-"CountyFIPS"
#         choropleth <- choropleth[order(choropleth$order), ]
#         choropleth$state_name<-NULL
#         choropleth<-data.frame(choropleth$county, choropleth$State, choropleth$CountyFIPS, choropleth$group, choropleth$lat, choropleth$long, rev(choropleth)[,1])
#         colnames(choropleth)<-c("County","State","CountyFIPS","group","lat","long","Cases")
#         choropleth<-subset(choropleth, State %in% IncludedCounties$State)
#         
#         #Plot the data
#         PlotCovidLocal<-ggplot(choropleth, aes(long, lat, group = group)) +
#             geom_polygon(aes(fill = log(Cases))) +
#             coord_fixed() +
#             theme_minimal() +
#             ggtitle("COVID-19 Cases by County (State View)") +
#             geom_point(data = BaseStats, aes(x= Long, y= Lat, group = 1),
#                        color = 'red', size = 5)+
#             theme(axis.line = element_blank(), axis.text = element_blank(),
#                   axis.ticks = element_blank(), axis.title = element_blank()) +
#             scale_fill_viridis("log(Cases)")
#         
#         ggplotly(PlotCovidLocal)
#         
#     }
#     
#     
# }


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
  
  
  ####################################################################################
  #Mean Estimate
  NationalPop <-  sum(CountyInfo$Population)
  NationalCases<-sum(rev(CovidConfirmedCases)[1]-rev(CovidConfirmedCases)[8])
  
  
  #Next we use the calculated values, along with estimated values from the Estimated Values. 
  #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
  cases<-NationalCases*.05
  pop<-NationalPop
  doubling<-8
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14
  socialdistancing<-SocialDistance
  hospitalizationrate<-15
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7
  daysforecasted<-DaysForecasted
  
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
  cases<-NationalCases*.05
  pop<-NationalPop
  doubling<-10
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14
  socialdistancing<-SocialDistance
  hospitalizationrate<-10
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7
  daysforecasted<-DaysForecasted
  
  
  
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
  cases<-NationalCases*.05
  pop<-NationalPop
  doubling<-7
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14
  socialdistancing<-SocialDistance
  hospitalizationrate<-20
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7
  daysforecasted<-DaysForecasted
  
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
  
  OverlayData<-rbind(DailyData,IHMENationalData)
  OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
  OverlayData<- dplyr::filter(OverlayData, ForecastDate >= Sys.Date() & ForecastDate <= (Sys.Date() + DaysForecasted))
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



CHIMENationalPlot<-function(SocialDistance, DaysForecasted){
    ####################################################################################
  #Get past data in daily hospital use
  #This will use a 5 day hospital stay as the average
  HistoricalDataDaily <- CovidConfirmedCases[,(5+5):length(CovidConfirmedCases)] -
    CovidConfirmedCases[,5:(length(CovidConfirmedCases)-5)]
  HistoricalDataHosp<-colSums(HistoricalDataDaily*CountyHospRate$HospRate)
  
  #Create dataframe to hold daily hospitalizations
  HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
  HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
  colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
  
  
  ####################################################################################
  #Mean Estimate
  NationalPop <-  sum(CountyInfo$Population)
  NationalCases<-sum(rev(CovidConfirmedCases)[1]-rev(CovidConfirmedCases)[8])
  
  
  #Next we use the calculated values, along with estimated values from the Estimated Values. 
  #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
  cases<-NationalCases*.05
  pop<-NationalPop
  doubling<-8
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14
  socialdistancing<-SocialDistance
  hospitalizationrate<-15
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7
  daysforecasted<-DaysForecasted
  
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
  cases<-NationalCases*.05
  pop<-NationalPop
  doubling<-10
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14
  socialdistancing<-SocialDistance
  hospitalizationrate<-10
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7
  daysforecasted<-DaysForecasted
  
  
  
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
  cases<-NationalCases*.05
  pop<-NationalPop
  doubling<-7
  
  #Established Variables at the start for every county or populations
  Ro<-2.5
  incubationtime<-5
  latenttime<-2
  recoverydays<-14
  socialdistancing<-SocialDistance
  hospitalizationrate<-20
  icurate<-6
  ventilatorrate<-3
  hospitaltime<-7
  icutime<-5
  ventilatortime<-7
  daysforecasted<-DaysForecasted
  
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
    DailyData$ID<-rep("CHIME", nrow(DailyData))
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    
    OverlayData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
    
    OverlayData<-rbind(HistoricalData, OverlayData)
    
    
    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
        geom_line() +
        scale_colour_manual(values=c("tan","black"))+
        scale_fill_manual(values = c("tan4", "gray"))+
        scale_linetype_manual(values = c("dashed", "solid"))+
        geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                    alpha = .2) +
        #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Daily Beds Required') +
        ggtitle("CHIME Projected Daily Hospital Bed Utilization") +
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
        OverlayData<- dplyr::filter(IHMENationalData, ForecastDate >= Sys.Date(), ForecastDate <= (Sys.Date() + DaysProjected))
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



CHIMELocalPlot<-function(SocialDistance, ForecastedDays, IncludedCounties, StatisticType){
    
    if (StatisticType == "Hospitalizations") {
        
        #Get covid cases and hospitalization rates for county
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
        CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
        
        #Get past data in daily hospital use
        #This will use a 5 day hospital stay as the average
        HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] -
          CovidCounties[,5:(length(CovidCounties)-5)]
        HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)
        
        #Create dataframe to hold daily hospitalizations
        HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Daily Hospitalizations","Lower Estimate","Upper Estimate")
        
        DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
        CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
        CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
        CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
        colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
        
        #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
        #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
        ActiveCases<-rev(CovidCounties)[1:7]
        ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
        colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
        SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
        colnames(SIRinputs)<-c("cases","pop","doubling")
        
        
        ####################################################################################
        #Mean Estimate
        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-8
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-SocialDistance
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-ForecastedDays
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                                   ventilatortime,daysforecasted,Ro, .5)
        
        MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
        DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
        TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
        
        
        ####################################################################################
        #Lower Estimate
        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-10
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
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
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-7
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        
        hospitalizationrate<-5.5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Hospitalizations","Lower Estimate","Upper Estimate")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
        
        DailyProjectionsSub <- melt(data.table(DailyData), id=c("ForecastDate"))
        TotalProjectionsSub <- melt(data.table(TotalData), id=c("ForecastDate"))
        
        DailyData$`Expected Daily Hospitalizations` <- round(DailyData$`Expected Daily Hospitalizations`,0)
        DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
        DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
        DailyData<-DailyData[-1,]
        
        
        DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
        DailyData$ID<-rep("CHIME", nrow(DailyData))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        
        PlottingData<-rbind(HistoricalData, DailyData)
        
        
        hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
        #Finds number of hospitals in radius
        TotalBeds<-sum(hospCounty$num_staffed_beds)
        #get historic utilization
        hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
        totalUsedBeds <- sum(hospCounty$bedsUsed)
        baseUtlz <- totalUsedBeds/TotalBeds
        
        
        
        #Plot for local area cumulative cases
        projections <- ggplot(PlottingData, aes(x=ForecastDate, y=`Expected Daily Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
            geom_line() +
            scale_colour_manual(values=c("tan","black"))+
            scale_fill_manual(values = c("tan4", "gray"))+
            scale_linetype_manual(values = c("dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
            xlab('Date') +
            ylab('Daily Beds Needed') +
            ggtitle("CHIME Projected Daily Hospital Bed Utilization") +
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
        
    } else {
        
        #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
        #We include State, county, population in those counties, cases, fatalities, doubling rate
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
        CovidDeathsHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
        HistoricalData<-colSums(CovidDeathsHist[,5:length(CovidDeathsHist)])

        HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
        
        DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
        CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
        CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
        CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
        colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
        
        #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
        #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
        ActiveCases<-rev(CovidCounties)[1:7]
        ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
        colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
        SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
        colnames(SIRinputs)<-c("cases","pop","doubling")
        
        
        ####################################################################################
        #Mean Estimate
        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-8
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-SocialDistance
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-ForecastedDays
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                                   ventilatortime,daysforecasted,Ro, .5)
        
        MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
        DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
        TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
        
        
        ####################################################################################
        #Lower Estimate
        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-10
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        
        hospitalizationrate<-4.5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
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
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-7
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        
        hospitalizationrate<-5.5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
        
        DailyProjectionsSub <- melt(data.table(DailyData), id=c("ForecastDate"))
        TotalProjectionsSub <- melt(data.table(TotalData), id=c("ForecastDate"))
        
        DailyData$`Expected Fatalities` <- round(DailyData$`Expected Fatalities`*(.25/5.5),0)
        DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`*(.15/5),0)
        DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`*(1/8),0)
        DailyData<-DailyData[-1,]
        DailyData$`Expected Fatalities`<-cumsum(DailyData$`Expected Fatalities`)
        DailyData$`Lower Estimate`<-cumsum(DailyData$`Lower Estimate`)
        DailyData$`Upper Estimate`<-cumsum(DailyData$`Upper Estimate`)
        
        DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
        DailyData$ID<-rep("CHIME", nrow(DailyData))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        
        PlottingData<-rbind(HistoricalData, DailyData)
        
        
        #Plot for local area cumulative cases
        projections <- ggplot(PlottingData, aes(x=ForecastDate, y=`Expected Fatalities`, color = ID, fill = ID, linetype = ID)) +
            geom_line() +
            scale_colour_manual(values=c("tan","black"))+
            scale_fill_manual(values = c("tan4", "gray"))+
            scale_linetype_manual(values = c("dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
            xlab('Date') +
            ylab('Fatalities') +
            ggtitle("CHIME Projected Fatalities") +
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
        
        projections<- ggplotly(projections)
        projections <- projections %>% config(displayModeBar = FALSE)
        projections
    }
    
}


IHMELocalProjections<-function(MyCounties, IncludedHospitals, ChosenBase, StatisticType, DaysProjected){
    if (StatisticType == "Hospitalizations") {
      
        #Establish initial inputs such as base, counties, and filter IHME model
        BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        hospCounty <- subset(HospUtlzCounty, fips %in% MyCounties$FIPS)
        
        #Get covid cases and hospitalization rates for county
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% MyCounties$FIPS)
        
        #Get past data in daily hospital use
        #This will use a 5 day hospital stay as the average
        HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] -
          CovidCounties[,5:(length(CovidCounties)-5)]
        HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)
        
        #Create dataframe to hold daily hospitalizations
        HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        
        #Get population information to build scaling ratio
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
        RegPop <- sum(MyCounties$Population)
        StPop <- sum(StPopList$Population)
        
        #Use Population ratio to scale IHME
        PopRatio <- RegPop/StPop
      
        #Finds number of hospitals in radius
        TotalBeds<-sum(hospCounty$num_staffed_beds)
        #get historic utilization
        hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
        totalUsedBeds <- sum(hospCounty$bedsUsed)
        baseUtlz <- totalUsedBeds/TotalBeds
        TT <- sum(IncludedHospitals$BEDS)
        
        # Apply ratio's to IHME data
        IHME_Region <- IHME_State
        IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
        IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
        IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
        IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
        colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
        IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date() & ForecastDate <= (Sys.Date() + DaysProjected))
        IHME_Region$ID<-rep("IHME", nrow(IHME_Region))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        
        IHME_Region<-rbind(HistoricalData,IHME_Region)
        IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
        
        r1 <- ggplot(IHME_Region, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
            geom_line() +
            scale_colour_manual(values=c("blue","black"))+
            scale_fill_manual(values = c("cadetblue", "gray"))+
            scale_linetype_manual(values = c("dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
            xlab('Date') +
            ylab('Daily Beds Needed') +
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
        
        r1<- ggplotly(r1)
        r1 <- r1 %>% config(displayModeBar = FALSE)
        r1
        
    } else {
        #Creating the stats and dataframes determined by the base we choose to look at.
        BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        TotalBedsCounty <- sum(IncludedHospitals$BEDS)
        
        #Get regional and state populations
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        CovidDeathsHist<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
        HistoricalData<-colSums(CovidDeathsHist[,5:length(CovidDeathsHist)])
        HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        
        
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
        RegPop <- sum(MyCounties$Population)
        StPop <- sum(StPopList$Population)
        
        # Use Population ratio to scale IHME
        PopRatio <- RegPop/StPop
        
        # Get total hospital bed number across state
        IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
        TotalBedsState <- sum(IncludedHospitalsST$BEDS)
        
        # Calculate bed ratio
        BedProp <- TotalBedsCounty/TotalBedsState
        
        # Apply ratio's to IHME data
        IHME_Region <- IHME_State
        IHME_Region$deaths_mean = round(IHME_State$totdea_mean*PopRatio)
        IHME_Region$deaths_lower = round(IHME_State$totdea_lower*PopRatio)
        IHME_Region$deaths_upper = round(IHME_State$totdea_upper*PopRatio)
        IHME_Region<-data.frame(IHME_Region$date, IHME_Region$deaths_mean, IHME_Region$deaths_lower, IHME_Region$deaths_upper)
        colnames(IHME_Region)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date() & ForecastDate <= (Sys.Date() + DaysProjected))
        IHME_Region$ID<-rep("IHME", nrow(IHME_Region))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        
        IHME_Region<-rbind(HistoricalData,IHME_Region)
        IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
        
        r1 <- ggplot(IHME_Region, aes(x=ForecastDate, y=`Expected Fatalities`, color = ID, fill = ID, linetype = ID)) +
            geom_line() +
            scale_colour_manual(values=c("blue","black"))+
            scale_fill_manual(values = c("cadetblue", "gray"))+
            scale_linetype_manual(values = c("dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
            xlab('Date') +
            ylab('Fatalities') +
            ggtitle("IHME Projected Fatalities") +
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
        
        r1 <- ggplotly(r1)
        r1 <- r1 %>% config(displayModeBar = FALSE)
        r1
    }
}



# Output Projections ---------------------------------------------------------------------------------------------------------------------------------------------------------------
AFrow = nrow(AFBaseLocations)
ForecastDataTable <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                                                                        "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                        "14D IHME Forecast","14D IHME Peak","14D IHME Peak  Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                        "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                                                                        "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date") )

ForecastDataTableCases <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Cases Per 100,000", "Cases Per 10,000", "New Cases",
                                                                        "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                        "14D IHME Forecast","14D IHME Peak","14D IHME Peak  Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                        "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                                                                        "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date") )


for (i in 2:AFrow){
  #Create Number of current cases and cases per 100,000 in a local area
  MyCounties<-GetCounties(AFBaseLocations$Base[i],50)
  CovidDataCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
  NewCases<-sum(rev(CovidDataCounties)[,1]-rev(CovidDataCounties)[,2])
  NewHospitalizations<-round(NewCases*.2)
  TotalPop<-CalculateCounties(MyCounties)
  TotalCases<-CalculateCovid(MyCounties)
  CasesPer100000<-round(TotalCases/TotalPop*100000)
  CasesPer10000<-round(TotalCases/TotalPop*10000)
  HospitalizationsPer100000<-round(CasesPer100000*.2)
  HospitalizationsPer10000<-round(HospitalizationsPer100000/10)
  
  
  #Create a datatable with just the forecasted values for every installation
  #Creating the stats and dataframes determined by the base we choose to look at.
  #IHME_Model is the initial import data table from global.R
  #BaseState<-AFBaseLocations$State[i] #dplyr::filter(AFBaseLocations, Base == baseinput)
  #IncludedHospitals<-GetHospitals() 
  #GetHospitals
  HospitalInfo$DistanceMiles = himd[,as.character(AFBaseLocations$Base[i])]
  MyHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= 50))
  MyHospitals<-dplyr::filter(MyHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
  
  IHME_State <- dplyr::filter(IHME_Model, State == AFBaseLocations$State[i])
  TotalBedsCounty <- sum(MyHospitals$BEDS)
  
  
  hospCounty <- subset(HospUtlzCounty, fips %in% MyCounties$FIPS)
  #Finds number of hospitals in radius
  TotalBeds<-sum(hospCounty$num_staffed_beds)
  #get historic utilization
  hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
  totalUsedBeds <- sum(hospCounty$bedsUsed)
  baseUtlz <- totalUsedBeds/TotalBeds
  
  #Get regional and state populations
  #MyCounties <- GetCounties()
  #GetCounties
  CountyInfo$DistanceMiles = cimd[,as.character(AFBaseLocations$Base[i])]
  MyCounties<-dplyr::filter(CountyInfo, DistanceMiles <= 50)
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
  HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
  HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
  HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21) #, HistoricalData*.15, HistoricalData*.27)
  colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations") #, "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
  
  StPopList <- dplyr::filter(CountyInfo, State == AFBaseLocations$State[i])
  RegPop <- sum(MyCounties$Population)
  StPop <- sum(StPopList$Population)
  
  # Use Population ratio to scale IHME
  PopRatio <- RegPop/StPop
  
  # Get total hospital bed number across state
  IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == AFBaseLocations$State[i])
  TotalBedsState <- sum(IncludedHospitalsST$BEDS)
  
  # Calculate bed ratio
  BedProp <- TotalBedsCounty/TotalBedsState
  
  # Apply ratio's to IHME data
  IHME_Region <- IHME_State
  IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
  #IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
  #IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
  IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean) #, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
  colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations") #, "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
  IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date())
  
  IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
  IHME_Region <- dplyr::arrange(IHME_Region,ForecastDate)
  
  DeathCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
  CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% MyCounties$FIPS)
  CountyDataTable<-cbind(MyCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
  CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
  colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
  
  ####################################################################################
  #Mean Estimate
  
  #Next we use the calculated values, along with estimated values from the Estimated Values. 
  #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
  #CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS) 
  ActiveCases<-rev(CovidCounties)[1:7]
  ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1],MyCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
  colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
  SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`)) 
  colnames(SIRinputs)<-c("cases","pop","doubling")
  
  #Established Variables at the start for every county or populations 
  cases<-SIRinputs$cases
  pop<-SIRinputs$pop
  
  if(nrow(IHME_Region) == 0 || pop == 0){
    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0)
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak  Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                      "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date")
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000", "New Cases",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak  Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                      "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date")
    
    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
  }else{ 
    incubationtime<-5
    latenttime<-2
    doubling<-8 
    recoverydays<-14
    socialdistancing<-15
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    Ro<-2.5
    
    daysforecasted<-90
    #SEIARProj<-SEIAR_Model_Run(cases,pop,5,2,8,14,.15,5,6,3,3.5,4,7,daysforecasted,2.5,.5) 
    SEIARProj<-SEIAR_Model_Run(cases,pop,incubationtime,latenttime,doubling,recoverydays,socialdistancing,hospitalizationrate,
                               icurate,ventilatorrate,hospitaltime,icutime,ventilatortime,daysforecasted,Ro,.5)
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate","Expected Hospitalizations")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData<-DailyData[-1,]
    DailyData<- dplyr::filter(DailyData, ForecastDate > Sys.Date())
    ########################################################################################
    SevDayVal<-round(DailyData$`Expected Hospitalizations`[7])
    FourteenDayVal<-round(DailyData$`Expected Hospitalizations`[14])
    ThirtyDayVal<-round(DailyData$`Expected Hospitalizations`[30])
    SixtyDayVal<-round(DailyData$`Expected Hospitalizations`[60])
    PeakSevDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:7]))
    PeakFourteenDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:14]))
    PeakThirtyDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:30]))
    PeakSixtyDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:60]))
    PeakDateSevDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:7])
    PeakDateFourteenDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:14])
    PeakDateThirtyDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:30])
    PeakDateSixtyDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:60])
    PeakDateSevDayVal<-format(DailyData$ForecastDate[PeakDateSevDayVal], format="%b-%d")
    PeakDateFourteenDayVal<-format(DailyData$ForecastDate[PeakDateFourteenDayVal], format="%b-%d")
    PeakDateThirtyDayVal<-format(DailyData$ForecastDate[PeakDateThirtyDayVal], format="%b-%d")
    PeakDateSixtyDayVal<-format(DailyData$ForecastDate[PeakDateSixtyDayVal], format="%b-%d")
    
    
    
    #BEGIN IHME CALCS
    I1 = round(IHME_Region$`Expected Hospitalizations`[7])
    I2 = round(IHME_Region$`Expected Hospitalizations`[14])
    I3 = round(IHME_Region$`Expected Hospitalizations`[30])
    I4 = round(IHME_Region$`Expected Hospitalizations`[60])
    
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:7])
    Peak<-IHME_Region[PeakDate,2]
    PI1<-round(Peak)
    PID1<-IHME_Region[PeakDate,1]
    PID1<-format(PID1, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:14])
    Peak<-IHME_Region[PeakDate,2]
    PI2<-round(Peak)
    PID2<-IHME_Region[PeakDate,1]
    PID2<-format(PID2, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:30])
    Peak<-IHME_Region[PeakDate,2]
    PI3<-round(Peak)
    PID3<-IHME_Region[PeakDate,1]
    PID3<-format(PID3, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:60])
    Peak<-IHME_Region[PeakDate,2]
    PI4<-round(Peak)
    PID4<-IHME_Region[PeakDate,1]
    PID4<-format(PID4, format="%b-%d")
    
    
    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), HospitalizationsPer100000, HospitalizationsPer10000, NewHospitalizations,
                        I1,PI1,PID1,SevDayVal,PeakSevDayVal,PeakDateSevDayVal,
                        I2,PI2,PID2,FourteenDayVal,PeakFourteenDayVal,PeakDateFourteenDayVal,
                        I3,PI3,PID3,ThirtyDayVal,PeakThirtyDayVal,PeakDateThirtyDayVal,
                        I4,PI4,PID4,SixtyDayVal,PeakSixtyDayVal,PeakDateSixtyDayVal) 
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000","New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak  Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                      "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date")
    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), CasesPer100000, CasesPer10000, NewCases,
                        I1/.2,PI1/.2,PID1,SevDayVal/.2,PeakSevDayVal/.2,PeakDateSevDayVal,
                        I2/.2,PI2/.2,PID2,FourteenDayVal/.2,PeakFourteenDayVal/.2,PeakDateFourteenDayVal,
                        I3/.2,PI3/.2,PID3,ThirtyDayVal/.2,PeakThirtyDayVal/.2,PeakDateThirtyDayVal,
                        I4/.2,PI4/.2,PID4,SixtyDayVal/.2,PeakSixtyDayVal/.2,PeakDateSixtyDayVal) 
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000","New Cases",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak  Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date",
                      "60D IHME Forecast","60D IHME Peak","60D IHME Peak Date","60D SEIAR Forecast","60D SEIAR Peak","60D SEIAR Peak Date")
    ForecastDataTableCases <- rbind(ForecastDataTableCases,NewDFCases)
    
  }
}

ForecastDataTable$Installation<-as.character(ForecastDataTable$Installation)
ForecastDataTable<-ForecastDataTable %>% arrange(ForecastDataTable$Installation)

ForecastDataTableCases$Installation<-as.character(ForecastDataTableCases$Installation)
ForecastDataTableCases<-ForecastDataTableCases %>% arrange(ForecastDataTableCases$Installation)

#This just filters the data table based on IHME or CHIME
FilterDataTable<-function(dt, ModelType){
  if (ModelType == "IHME") {
    cols<-c(1:10,14,15,16,20,21,22,26,27,28)
    dt[, names(dt)[cols]]
  } else {
    cols<-c(1:7,11,12,13,17,18,19,23,24,25,29,30,31)
    dt[, names(dt)[cols]]
  }
}


######################## Summary Tab Heat Map
HeatMapForecast<-merge(AFBaseLocations, ForecastDataTable, by.x = "Base", by.y = "Installation")
HeatMapForecast<-data.frame(HeatMapForecast$Base, HeatMapForecast$Location, HeatMapForecast$State.x, HeatMapForecast$`Major Command`, HeatMapForecast$Lat, HeatMapForecast$Long,HeatMapForecast$`Available Beds`,HeatMapForecast$`Hopitalization Per 100,000`,HeatMapForecast$`Hopitalization Per 10,000`,HeatMapForecast$`New Hospitalizations`,HeatMapForecast$`New Hospitalizations` ,HeatMapForecast$`7D SEIAR Forecast`, HeatMapForecast$`7D IHME Forecast`,HeatMapForecast$`14D SEIAR Forecast`,  HeatMapForecast$`14D IHME Forecast`,  HeatMapForecast$`30D SEIAR Forecast`, HeatMapForecast$`30D IHME Forecast`, HeatMapForecast$`60D SEIAR Forecast`, HeatMapForecast$`60D IHME Forecast`)
colnames(HeatMapForecast)<-c("Base","City","State","MAJCOM","Lat","Long","Beds","Hospitalizations Per 100,000","Hospitalizations Per 10,000","Today.CHIME","Today.IHME", "Seven.IHME","Seven.CHIME","Fourteen.IHME","Fourteen.CHIME", "Thirty.IHME","Thirty.CHIME","Sixty.IHME","Sixty.CHIME")
HeatMapForecast<-reshape(HeatMapForecast, direction='long', 
                         varying=c('Today.CHIME','Today.IHME','Seven.IHME', 'Seven.CHIME', 'Fourteen.IHME', 'Fourteen.CHIME','Thirty.IHME','Thirty.CHIME', 'Sixty.IHME','Sixty.CHIME'), 
                         timevar='Days',
                         times=c('Today','Seven', 'Fourteen',"Thirty","Sixty"),
                         v.names=c('CHIME', 'IHME'),
                         idvar=c('Base','City','State','MAJCOM','Lat','Long','Beds',"Hospitalizations Per 100,000","Hospitalizations Per 10,000"))
HeatMapForecast<-transform(HeatMapForecast,IHMEID=ifelse((Beds)>=IHME,"Under Capacity","Over Capacity"))
HeatMapForecast<-transform(HeatMapForecast,CHIMEID=ifelse((Beds)>=CHIME,"Under Capacity","Over Capacity"))


HeatMapForecastCases<-merge(AFBaseLocations, ForecastDataTableCases, by.x = "Base", by.y = "Installation")
HeatMapForecastCases<-data.frame(HeatMapForecastCases$Base, HeatMapForecastCases$Location, HeatMapForecastCases$State.x, HeatMapForecastCases$`Major Command`, HeatMapForecastCases$Lat, HeatMapForecastCases$Long,HeatMapForecastCases$`Available Beds`,HeatMapForecastCases$`Cases Per 100,000`,HeatMapForecastCases$`Cases Per 10,000`,HeatMapForecastCases$`New Cases`,HeatMapForecastCases$`New Cases` ,HeatMapForecastCases$`7D SEIAR Forecast`, HeatMapForecastCases$`7D IHME Forecast`,HeatMapForecastCases$`14D SEIAR Forecast`,  HeatMapForecastCases$`14D IHME Forecast`,  HeatMapForecastCases$`30D SEIAR Forecast`, HeatMapForecastCases$`30D IHME Forecast`, HeatMapForecastCases$`60D SEIAR Forecast`, HeatMapForecastCases$`60D IHME Forecast`)
colnames(HeatMapForecastCases)<-c("Base","City","State","MAJCOM","Lat","Long","Beds","Cases Per 100,000","Cases_Per_10000","Today.CHIME","Today.IHME", "Seven.IHME","Seven.CHIME","Fourteen.IHME","Fourteen.CHIME", "Thirty.IHME","Thirty.CHIME","Sixty.IHME","Sixty.CHIME")
HeatMapForecastCases<-reshape(HeatMapForecastCases, direction='long', 
                              varying=c('Today.CHIME','Today.IHME','Seven.IHME', 'Seven.CHIME', 'Fourteen.IHME', 'Fourteen.CHIME','Thirty.IHME','Thirty.CHIME', 'Sixty.IHME','Sixty.CHIME'), 
                              timevar='Days',
                              times=c('Today','Seven', 'Fourteen',"Thirty","Sixty"),
                              v.names=c('CHIME', 'IHME'),
                              idvar=c('Base','City','State','MAJCOM','Lat','Long','Beds',"Cases Per 100,000","Cases_Per_10000"))
HeatMapForecastCases<-transform(HeatMapForecastCases,IHMEID=ifelse((Cases_Per_10000*10000*.05)>=IHME,"Under 5% Population","Over 5% Population"))
HeatMapForecastCases<-transform(HeatMapForecastCases,CHIMEID=ifelse((Cases_Per_10000*10000*.05)>=CHIME,"Under 5% Population","Over 5% Population"))


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
    
    fig <- plot_geo(HeatMap, locationmode = 'USA-states', sizes = c(20, 300))
    fig <- fig %>% add_markers(
      x = ~Long, y = ~Lat, size = ~IHME, color = ~IHMEID, colors = c("red","#228B22"), hoverinfo = "text",
      text = ~paste(HeatMap$Base, "<br />", HeatMap$IHME)
    )
    fig <- fig %>% layout(title = Banner , geo = g)
    
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
    
    fig <- plot_geo(HeatMap, locationmode = 'USA-states', sizes = c(20, 300))
    fig <- fig %>% add_markers(
      x = ~Long, y = ~Lat, size = ~CHIME, color = ~CHIMEID, colors = c("red","#228B22"), hoverinfo = "text",
      text = ~paste(HeatMap$Base, "<br />", HeatMap$CHIME)
    )
    fig <- fig %>% layout(title = Banner , geo = g)
    
    fig
    
  }
  
}
####################################################################




# Identify Info Pages
# Inputs
InfoLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/InputsInfo.md")
CalcLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/CalcInfo.md")
SourceLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/SourceInfo.md")
OverviewLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/OverviewInfo.md")
ProjLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/ProjInfo.md")





# EXTRA EXTRA
# Find a better way later to replace this
# probably have the overlay function return a list with two objects. 
# need the dataframe from overlay in the report
PlotOverlay2<-function(ChosenBase, IncludedCounties, IncludedHospitals, SocialDistance, DaysProjected, StatisticType){
  if (StatisticType == "Hospitalizations") {
    
    #Creating the stats and dataframes determined by the base we choose to look at.
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
    TotalBedsCounty <- sum(IncludedHospitals$BEDS)
    
    #Get regional and state populations
    StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
    RegPop <- sum(IncludedCounties$Population)
    StPop <- sum(StPopList$Population)
    
    # Use Population ratio to scale IHME
    PopRatio <- RegPop/StPop
    
    # Get total hospital bed number across state
    IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
    TotalBedsState <- sum(IncludedHospitalsST$BEDS)
    
    # Calculate bed ratio
    BedProp <- TotalBedsCounty/TotalBedsState
    
    # Apply ratio's to IHME data
    IHME_Region <- IHME_State
    IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
    IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
    IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
    IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
    
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    
    
    ####################################################################################
    #Lower Estimate
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-10
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    
    
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
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-7
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5.5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Estimate","Upper Estimate")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
    
    DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
    DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
    DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
    DailyData<-DailyData[-1,]
    colnames(IHME_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
    DailyData$ID<-rep("CHIME",nrow(DailyData))
    IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    OverlayData<-rbind(DailyData,IHME_Data)
    OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
    
    OverlayData<- dplyr::filter(OverlayData,ForecastDate >= Sys.Date())
    
    OverlayData<-rbind(HistoricalData, OverlayData)
    
  } else {
    
    #Creating the stats and dataframes determined by the base we choose to look at.
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
    TotalBedsCounty <- sum(IncludedHospitals$BEDS)
    
    #Get regional and state populations
    StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
    RegPop <- sum(IncludedCounties$Population)
    StPop <- sum(StPopList$Population)
    
    # Use Population ratio to scale IHME
    PopRatio <- RegPop/StPop
    
    # Get total hospital bed number across state
    IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
    TotalBedsState <- sum(IncludedHospitalsST$BEDS)
    
    # Calculate bed ratio
    BedProp <- TotalBedsCounty/TotalBedsState
    
    # Apply ratio's to IHME data
    IHME_Region <- IHME_State
    IHME_Region$deaths_mean = round(IHME_State$totdea_mean*PopRatio)
    IHME_Region$deaths_lower = round(IHME_State$totdea_lower*PopRatio)
    IHME_Region$deaths_upper = round(IHME_State$totdea_upper*PopRatio)
    
    IHME_Data<-data.frame(IHME_Region$date,IHME_Region$deaths_mean, IHME_Region$deaths_lower, IHME_Region$deaths_upper)
    
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidDeathHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidDeathHist[,5:length(CovidDeathHist)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    
    
    ####################################################################################
    #Lower Estimate
    
    #Next we use the calculated values, along with estimated values from the Estimated Values. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-10
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    
    
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
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-7
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5.5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
    
    DailyData$`Expected Fatalities` <- round(DailyData$`Expected Fatalities`*(.25/5.5),0)
    DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`*(.15/4),0)
    DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`*(1/8),0)
    DailyData<-DailyData[-1,]
    DailyData$`Expected Fatalities`<-cumsum(DailyData$`Expected Fatalities`)
    DailyData$`Lower Estimate`<-cumsum(DailyData$`Lower Estimate`)
    DailyData$`Upper Estimate`<-cumsum(DailyData$`Upper Estimate`)
    
    colnames(IHME_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
    DailyData$ID<-rep("CHIME",nrow(DailyData))
    IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    OverlayData<-rbind(DailyData,IHME_Data)
    OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
    
    OverlayData<- dplyr::filter(OverlayData, ForecastDate >= Sys.Date() & ForecastDate <= (Sys.Date() + DaysProjected))
    
    OverlayData<-rbind(HistoricalData, OverlayData)
  
  }

}






####################################################
############# Non-Used Functions ###################
####################################################



# SIR_Model_Run<-function(num_init_cases, Pop.At.Risk, detect_prob, 
#                         doubling, recovery_days, social_rate, hospital_rate,
#                         icu_rate, ventilated_rate, hospital_dur, icu_dur, ventilated_dur, n_days){
#     #create parameters for model
#     total_infections <- num_init_cases / (hospital_rate/100)
#     I <- total_infections / (detect_prob / 100) 
#     S <- (Pop.At.Risk - I)
#     R <- 0
#     intrinsic_growth_rate = 2 ^(1 / doubling) -1
#     recovery_days <- recovery_days
#     gamma <- 1 / recovery_days
#     beta <- (intrinsic_growth_rate + gamma) / S * (1-social_rate/100)
#     r_t <- beta / gamma * S 
#     r_0 <- r_t / (1 - social_rate/100)
#     doubling_time_t <- 1 / log2(beta*S - gamma + 1)
#     myList <- list()
#     myList$total_infections <- total_infections
#     myList$S <- S
#     myList$I <- I
#     myList$R <- R
#     myList$intrinsic_growth_rate <- intrinsic_growth_rate
#     myList$recovery_days <- recovery_days
#     myList$gamma <- gamma
#     myList$beta <- beta
#     myList$r_t <- r_t
#     myList$r_0 <- r_0
#     myList$doubling_time_t <- doubling_time_t
#     
#     
#     
#     
#     #initial values
#     N = S + I + R
#     hos_add <- (I * hospital_rate/100)
#     hos_cum <- (I * hospital_rate/100)
#     icu_add <- (hos_add * icu_rate/100)
#     icu_cum <- (hos_cum * icu_rate/100)
#     
#     #create the data frame
#     sir_data <- data.frame(t = 1,
#                            S = S,
#                            I = I,
#                            R = R,
#                            hos_add = hos_add,
#                            hos_cum = hos_cum,
#                            icu_add = hos_add * icu_rate/100,
#                            icu_cum = hos_cum * icu_rate/100,
#                            vent_add = icu_add * ventilated_rate/100,
#                            vent_cum = icu_cum * ventilated_rate/100,
#                            Id = 0
#     )
#     
#     for(i in 2:n_days){
#         y <- sir(S,I,R, beta, gamma, N)
#         S <- y$S
#         I <- y$I
#         R <- y$R
#         
#         #calculate new infections
#         Id <- (sir_data$S[i-1] - S)
#         
#         #portion of the the newly infected that are in the hospital, ICU, and Vent
#         hos_add <- Id * hospital_rate/100
#         hos_cum <- sir_data$hos_cum[i-1] + hos_add
#         
#         icu_add <- hos_add * icu_rate/100
#         icu_cum <- sir_data$icu_cum[i-1] + icu_add
#         
#         vent_add <- icu_add * ventilated_rate/100 
#         vent_cum <- sir_data$vent_cum[i-1] + vent_add
#         
#         temp <- data.frame(t = i,
#                            S = S,
#                            I = I,
#                            R = R,
#                            hos_add = hos_add,
#                            hos_cum = hos_cum,
#                            icu_add = icu_add,
#                            icu_cum = icu_cum,
#                            vent_add = vent_add,
#                            vent_cum = vent_cum,
#                            Id = Id
#         )
#         
#         sir_data <- rbind(sir_data,temp)
#     }
#     
#     #doing some weird stuff to get a rolling sum of hospital impacts based on length of stay (los)
#     if(n_days > hospital_dur){
#         h_c <- rollsum(sir_data$hos_add,hospital_dur)
#         sir_data$hos_cum <- c(sir_data$hos_cum[1:(n_days - length(h_c))],h_c)
#     } 
#     if(n_days > icu_dur){
#         i_c <- rollsum(sir_data$icu_add,icu_dur)
#         sir_data$icu_cum <- c(sir_data$icu_cum[1:(n_days - length(i_c))],i_c)
#     } 
#     if(n_days > ventilated_dur){
#         v_c <- rollsum(sir_data$vent_add,ventilated_dur)
#         sir_data$vent_cum <- c(sir_data$vent_cum[1:(n_days - length(v_c))],v_c)
#     } 
#     #write.csv(sir_data, file = 'test.csv') # for testing
#     h_m <- round(max(sir_data$hos_cum), 0)
#     i_m <- round(max(sir_data$icu_cum), 0)
#     v_m <- round(max(sir_data$vent_cum), 0)
#     myList$sir <- sir_data
#     myList$hos_max <- h_m
#     myList$icu_max <- i_m
#     myList$vent_max <- v_m 
#     
#     h_m <- sir_data$t[which.max(sir_data$hos_cum)][1]
#     i_m <- sir_data$t[which.max(sir_data$icu_cum)][1]
#     v_m <- sir_data$t[which.max(sir_data$vent_cum)][1]
#     myList$hos_t_max <- h_m
#     myList$icu_t_max <- i_m
#     myList$vent_t_max <- v_m 
#     
#     h_m <- round(max(sir_data$hos_add), 0)
#     i_m <- round(max(sir_data$icu_add), 0)
#     v_m <- round(max(sir_data$vent_add), 0)
#     
#     myList$hos_add <- h_m
#     myList$icu_add <- i_m
#     myList$vent_add <- v_m 
#     return(myList)
# }
# 
# sir<-function(S,I,R, beta, gamma, N){
#     Sn <- (-beta * S * I) + S
#     In = (beta * S * I - gamma * I) + I
#     Rn = gamma * I + R
#     if(Sn < 0) Sn = 0
#     if(In < 0) In = 0
#     if(Rn < 0) Rn = 0
#     
#     scale = N / (Sn + In + Rn )
#     myListSIR <- list()
#     myListSIR$S <- (Sn * scale)
#     myListSIR$I <- (In * scale)
#     myListSIR$R <- (Rn * scale)
#     return(myListSIR)
# }

