#' @description From Columbia University, These files contain 42 day 
#'              projections which they update on Sunday evenings.
#'              
#' @source https://github.com/shaman-lab/COVID-19Projection/tree/master/
#' 

CU20_1x10PSD <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_May10/bed_80contact1x10p.csv")
CU20_1x5PSD <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_May10/bed_80contact1x5p.csv")
CU20_w10PSD <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_May10/bed_80contactw10p.csv")
CU20_w5PSD <-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_May10/bed_80contactw5p.csv")

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

CU20_1x10PSD<-subset(CU20_1x10PSD, select = -c(hosp_need_2.5,
                            hosp_need_97.5,ICU_need_2.5,
                            ICU_need_25,ICU_need_50,
                            ICU_need_75, ICU_need_97.5,
                            vent_need_2.5,vent_need_25,
                            vent_need_50,vent_need_75,
                            vent_need_97.5,death_2.5,
                            death_97.5))

CU20_1x5PSD<-subset(CU20_1x5PSD, select = -c(hosp_need_2.5,
                            hosp_need_97.5,ICU_need_2.5,
                            ICU_need_25,ICU_need_50,
                            ICU_need_75,ICU_need_97.5,
                            vent_need_2.5,vent_need_25,
                            vent_need_50,vent_need_75,
                            vent_need_97.5,death_2.5,
                            death_97.5))

CU20_w10PSD<-subset(CU20_w10PSD, select = -c(hosp_need_2.5,
                            hosp_need_97.5,ICU_need_2.5,
                            ICU_need_25,ICU_need_50,
                            ICU_need_75,ICU_need_97.5,
                            vent_need_2.5,vent_need_25,
                            vent_need_50,vent_need_75,
                            vent_need_97.5,death_2.5,
                            death_97.5))

CU20_w5PSD<-subset(CU20_w5PSD, select = -c(hosp_need_2.5,
                            hosp_need_97.5,ICU_need_2.5,
                            ICU_need_25,ICU_need_50,
                            ICU_need_75,ICU_need_97.5,
                            vent_need_2.5,vent_need_25,
                            vent_need_50,vent_need_75,
                            vent_need_97.5,death_2.5,
                            death_97.5))