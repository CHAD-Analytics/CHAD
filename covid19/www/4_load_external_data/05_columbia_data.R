#' @description From Columbia University, These files contain 42 day 
#'              projections which they update on Sunday evenings.
#'              
#' @source https://github.com/shaman-lab/COVID-19Projection/tree/master/
#' 
#' Columbia does not provide overseas forecasting


#Projections are generated for daily new confirmed case, daily new infection (both reported and unreported), cumulative demand of hospital beds, ICU and ventilators as well as daily mortality (2.5, 25, 50, 75 and 97.5 percentiles).

#Projections for daily new confirmed case and daily new infection are reported in Projection_*.csv

CU20_1x10PSD <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_June11/Projection_nochange.csv")
CU20_1x5PSD <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_June11/Projection_80x5pcontact.csv")
CU20_w10PSD <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_June11/Projection_80contact.csv")
CU20_w5PSD <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_June11/Projection_80w5pcontact.csv")

# CU20_1x10PSD <-vroom::vroom("www/4_load_external_data/data_files/bed_80contact1x10p.csv")
# CU20_1x5PSD<-vroom::vroom("www/4_load_external_data/data_files/bed_80contact1x5p.csv")
# CU20_w10PSD<-vroom::vroom("www/4_load_external_data/data_files/bed_80contactw10p.csv")
# CU20_w5PSD<-vroom::vroom("www/4_load_external_data/data_files/bed_80contactw5p.csv")

CU20_1x10PSD<-CU20_1x10PSD %>% separate(county,c("County","State"), extra = "drop", fill = "right")
CU20_1x5PSD<-CU20_1x5PSD %>% separate(county,c("County","State"), extra = "drop", fill = "right")
CU20_w10PSD<-CU20_w10PSD %>% separate(county,c("County","State"), extra = "drop", fill = "right")
CU20_w5PSD<-CU20_w5PSD %>% separate(county,c("County","State"), extra = "drop", fill = "right")

CU20_1x10PSD$fips<-as.numeric(CU20_1x10PSD$fips)
CU20_1x5PSD$fips<-as.numeric(CU20_1x5PSD$fips)
CU20_w10PSD$fips<-as.numeric(CU20_w10PSD$fips)
CU20_w5PSD$fips<-as.numeric(CU20_w5PSD$fips)

CU20_1x10PSD<-subset(CU20_1x10PSD, select = -c(report_2.5,report_97.5,total_2.5,total_25,total_50,total_75,total_97.5))
CU20_1x5PSD<-subset(CU20_1x5PSD, select = -c(report_2.5,report_97.5,total_2.5,total_25,total_50,total_75,total_97.5))
CU20_w10PSD<-subset(CU20_w10PSD, select = -c(report_2.5,report_97.5,total_2.5,total_25,total_50,total_75,total_97.5))
CU20_w5PSD<-subset(CU20_w5PSD, select = -c(report_2.5,report_97.5,total_2.5,total_25,total_50,total_75,total_97.5))