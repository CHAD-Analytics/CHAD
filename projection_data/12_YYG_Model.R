#' @description From youyang Gu, independent data scientist, projects cases and deaths 
#' @source https://github.com/youyanggu/covid19_projections/tree/master/
#' 
#' 
#YYG_Model<-vroom::vroom("www/4_load_external_data/data_files/YYG_projections.csv")

YYG_ModelU<-vroom::vroom("https://raw.githubusercontent.com/youyanggu/covid19_projections/master/projections/combined/latest_us.csv")
#YYG_ModelS<-vroom::vroom("https://raw.githubusercontent.com/youyanggu/covid19_projections/master/projections/combined/latest_subregion.csv")
YYG_ModelG<-vroom::vroom("https://raw.githubusercontent.com/youyanggu/covid19_projections/master/projections/combined/latest_global.csv")
YYG_ModelG$date <- as.Date(YYG_ModelG$date, format = "%Y-%m-%d")   #No region breakdown, only full country projections
YYG_ModelU$date <- as.Date(YYG_ModelU$date, format = "%Y-%m-%d")
#State abbreviation already included

closeAllConnections()