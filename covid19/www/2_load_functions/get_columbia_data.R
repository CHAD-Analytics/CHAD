#'
#' @importFrom vroom vroom
#' @export
get_columbia_data <- function(){

CU40PSD<-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_April19/bed_60contact.csv")
CU30PSD<-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_April19/bed_70contact.csv")
CU20PSD<-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_April19/bed_80contact.csv")
CU00PSD<-vroom::vroom("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_April19/bed_nointerv.csv")

CU40PSD<-CU40PSD %>% separate(county,c("County","State"))
CU30PSD<-CU30PSD %>% separate(county,c("County","State"))
CU20PSD<-CU20PSD %>% separate(county,c("County","State"))
CU00PSD<-CU00PSD %>% separate(county,c("County","State"))

CU40PSD<-subset(CU40PSD, select=-c(hosp_need_2.5,hosp_need_97.5,ICU_need_2.5,ICU_need_25,ICU_need_50,ICU_need_75,ICU_need_97.5,
                                   vent_need_2.5,vent_need_25,vent_need_50,vent_need_75,vent_need_97.5,death_2.5,death_97.5))
CU30PSD<-subset(CU30PSD, select=-c(hosp_need_2.5,hosp_need_97.5,ICU_need_2.5,ICU_need_25,ICU_need_50,ICU_need_75,ICU_need_97.5,
                                   vent_need_2.5,vent_need_25,vent_need_50,vent_need_75,vent_need_97.5,death_2.5,death_97.5))
CU20PSD<-subset(CU20PSD, select=-c(hosp_need_2.5,hosp_need_97.5,ICU_need_2.5,ICU_need_25,ICU_need_50,ICU_need_75,ICU_need_97.5,
                                   vent_need_2.5,vent_need_25,vent_need_50,vent_need_75,vent_need_97.5,death_2.5,death_97.5))
CU00PSD<-subset(CU00PSD, select=-c(hosp_need_2.5,hosp_need_97.5,ICU_need_2.5,ICU_need_25,ICU_need_50,ICU_need_75,ICU_need_97.5,
                                   vent_need_2.5,vent_need_25,vent_need_50,vent_need_75,vent_need_97.5,death_2.5,death_97.5))

return(list(CU40PSD = CU40PSD,
            CU30PSD = CU30PSD,
            CU20PSD = CU20PSD,
            CU00PSD = CU00PSD))

}