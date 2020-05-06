#' #' @description From Columbia University, These files contain 42 day 
#' #'              projections which they update on Sunday evenings.
#' #'              
#' #' @source https://github.com/youyanggu/covid19_projections/tree/master/
#' #' 
#' #' 
#' YYG_Model<-vroom::vroom("www/4_load_external_data/data_files/YYG_projections.csv")
#' 
#' YYG_Model<-YYG_Model %>% separate(county,c("County","State"), extra = "drop", fill = "right")
#' YYG_Model$fips<-as.numeric(CU40PSD$fips)
#' 
#' YYG_Model<-subset(YYG_Model, 
#'                 select = -c(hosp_need_2.5,
#'                             hosp_need_97.5,
#'                             ICU_need_2.5,
#'                             ICU_need_25,
#'                             ICU_need_50,
#'                             ICU_need_75,
#'                             ICU_need_97.5,
#'                             vent_need_2.5,
#'                             vent_need_25,
#'                             vent_need_50,
#'                             vent_need_75,
#'                             vent_need_97.5,
#'                             death_2.5,death_97.5))
