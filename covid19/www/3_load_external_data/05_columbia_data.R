#' @description From Columbia University, These files contain 42 day 
#'              projections which they update on Sunday evenings.
#'              
#' @source https://github.com/shaman-lab/COVID-19Projection/tree/master/
#' 
CU40PSD<-vroom::vroom("www/3_load_external_data/data_files/bed_60contact.csv")
CU30PSD<-vroom::vroom("www/3_load_external_data/data_files/bed_70contact.csv")
CU20PSD<-vroom::vroom("www/3_load_external_data/data_files/bed_80contact.csv")
CU00PSD<-vroom::vroom("www/3_load_external_data/data_files/bed_nointerv.csv")

CU40PSD<-CU40PSD %>% separate(county,c("County","State"), extra = "drop", fill = "right")
CU30PSD<-CU30PSD %>% separate(county,c("County","State"), extra = "drop", fill = "right")
CU20PSD<-CU20PSD %>% separate(county,c("County","State"), extra = "drop", fill = "right")
CU00PSD<-CU00PSD %>% separate(county,c("County","State"), extra = "drop", fill = "right")

CU40PSD<-subset(CU40PSD, 
                select = -c(hosp_need_2.5,
                            hosp_need_97.5,
                            ICU_need_2.5,
                            ICU_need_25,
                            ICU_need_50,
                            ICU_need_75,
                            ICU_need_97.5,
                            vent_need_2.5,
                            vent_need_25,
                            vent_need_50,
                            vent_need_75,
                            vent_need_97.5,
                            death_2.5,death_97.5))

CU30PSD<-subset(CU30PSD, 
                select = -c(hosp_need_2.5,
                            hosp_need_97.5,
                            ICU_need_2.5,
                            ICU_need_25,
                            ICU_need_50,
                            ICU_need_75,
                            ICU_need_97.5,
                            vent_need_2.5,
                            vent_need_25,
                            vent_need_50,
                            vent_need_75,
                            vent_need_97.5,
                            death_2.5,
                            death_97.5))

CU20PSD<-subset(CU20PSD, 
                select = -c(hosp_need_2.5,
                            hosp_need_97.5,
                            ICU_need_2.5,
                            ICU_need_25,
                            ICU_need_50,
                            ICU_need_75,
                            ICU_need_97.5,
                            vent_need_2.5,
                            vent_need_25,
                            vent_need_50,
                            vent_need_75,
                            vent_need_97.5,
                            death_2.5,
                            death_97.5))

CU00PSD<-subset(CU00PSD, 
                select = -c(hosp_need_2.5,
                            hosp_need_97.5,
                            ICU_need_2.5,
                            ICU_need_25,
                            ICU_need_50,
                            ICU_need_75,
                            ICU_need_97.5,
                            vent_need_2.5,
                            vent_need_25,
                            vent_need_50,
                            vent_need_75,
                            vent_need_97.5,
                            death_2.5,death_97.5))
