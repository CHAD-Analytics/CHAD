#' @description From Columbia University, These files contain 42 day 
#'              projections which they update on Sunday evenings.
#'              
#' @source https://github.com/shaman-lab/COVID-19Projection/tree/master/
#' 
#' Columbia does not provide overseas forecasting


#Projections are generated for daily new confirmed case, daily new infection (both reported and unreported), cumulative demand of hospital beds, ICU and ventilators as well as daily mortality (2.5, 25, 50, 75 and 97.5 percentiles).

#Projections for daily new confirmed case and daily new infection are reported in Projection_*.csv

#We have initiated the following scenarios:
  
#1) nochange - Assumes that current contact rates will remain unchanged in the future.
#2) 5_1x - We project a one-time 5% increase in contact rates due to loosening restrictions as states continue to reopen economically, applied at the beginning of the projection.
#3) 5_2xhold - This scenario assumes a weekly 5% increase in contact rates for two weeks. The following week, the reproduction number R is set to 1 for the remainder of the projection.
#4) season4 - This scenario assumes that current levels of social mixing will remain unchanged in the future. In addition, it assumes a seasonal decrease in disease transmission leading to a weekly 4% decrease in reproductive number R(t).

CUM1 <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_June21/bed_5_1x.csv")
CUM2 <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_June21/bed_5_2xhold.csv")
CUM3 <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_June21/bed_nochange.csv")
CUM4 <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_June21/bed_season4.csv")

# CUM1 <-vroom::vroom("www/4_load_external_data/data_files/bed_80contact1x10p.csv")
# CUM1 <-room::vroom("www/4_load_external_data/data_files/bed_80contact1x5p.csv")
# CUM1 <-vroom::vroom("www/4_load_external_data/data_files/bed_80contactw10p.csv")
# CUM1 <-vroom::vroom("www/4_load_external_data/data_files/bed_80contactw5p.csv")

#CUM1 <-CUM1 %>% separate(county,c("County","State"), extra = "drop", fill = "right")
#CUM2 <-CUM2 %>% separate(county,c("County","State"), extra = "drop", fill = "right")
#CUM3 <-CUM3 %>% separate(county,c("County","State"), extra = "drop", fill = "right")
#CUM4 <-CUM4 %>% separate(county,c("County","State"), extra = "drop", fill = "right")

CUM1$fips<-as.numeric(CUM1$fips)
CUM2$fips<-as.numeric(CUM2$fips)
CUM3$fips<-as.numeric(CUM3$fips)
CUM4$fips<-as.numeric(CUM4$fips)

CUM1 <- merge(CUM1,StateList,by.x = names(CUM1)[3],by.y = names(StateList)[1])
CUM2 <- merge(CUM2,StateList,by.x = names(CUM2)[3],by.y = names(StateList)[1])
CUM3 <- merge(CUM3,StateList,by.x = names(CUM3)[3],by.y = names(StateList)[1])
CUM4 <- merge(CUM4,StateList,by.x = names(CUM4)[3],by.y = names(StateList)[1])

names(CUM1)[names(CUM1)=="state.abb"] <- "State"
names(CUM2)[names(CUM2)=="state.abb"] <- "State"
names(CUM3)[names(CUM3)=="state.abb"] <- "State"
names(CUM4)[names(CUM4)=="state.abb"] <- "State"

#CU20_1x10PSD<-subset(CU20_1x10PSD, select = -c(report_2.5,report_97.5,total_2.5,total_25,total_50,total_75,total_97.5))
#CU20_1x5PSD<-subset(CU20_1x5PSD, select = -c(report_2.5,report_97.5,total_2.5,total_25,total_50,total_75,total_97.5))
#CU20_w10PSD<-subset(CU20_w10PSD, select = -c(report_2.5,report_97.5,total_2.5,total_25,total_50,total_75,total_97.5))
#CU20_w5PSD<-subset(CU20_w5PSD, select = -c(report_2.5,report_97.5,total_2.5,total_25,total_50,total_75,total_97.5))