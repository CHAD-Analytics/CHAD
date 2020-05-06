#' @description From youyang Gu, independent data scientist, projects cases and deaths 
#' @source https://github.com/youyanggu/covid19_projections/tree/master/
#' 
#' 
#YYG_Model<-vroom::vroom("www/4_load_external_data/data_files/YYG_projections.csv")

Date<-Sys.Date() - 1

YYG_Model<-vroom::vroom(paste0("https://raw.githubusercontent.com/youyanggu/covid19_projections/master/projections/combined/",Date,"_us.csv"))
YYG_Model$date <- as.Date(YYG_Model$date, format = "%Y-%m-%d")
#State abbreviation already included

closeAllConnections()