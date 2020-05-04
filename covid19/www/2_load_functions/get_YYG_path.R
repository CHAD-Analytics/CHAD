# YYG.path = function(repo_url = "https://github.com/youyanggu/COVID-19Projection") {
# 
#   repo_code = xml2::read_html(repo_url)
#   
#   anchors = rvest::html_nodes(repo_code, "a")
#   
#   anchor_titles = rvest::html_attr(anchors, name = "title")
#   
#   projection_folder_names = grep(anchor_titles, 
#                                  pattern = "([Pp]rojection_)(\\S*\\d+$)", 
#                                  value = T)
#   
#   cut_off_Projection_ = gsub(pattern = "[Pp]rojection_",
#                              replacement = "",
#                              projection_folder_names)
#   
#   o = order(as.Date(cut_off_Projection_, format = "%B%d"))
#   
#   return(projection_folder_names[o][length(o)])
#   
# }


#'  Get data from YYG
#' @param front front of url
#' @param middle middle of url
#' @param end end of url 
#' @param date Have to adjust the integer value based on the date of the most recent forecast file
#' @export
#' @importFrom vroom vroom
get_YYG_path <- function(Front = 'https://github.com/youyanggu/covid19_projections/blob/master/projections/combined/',
                          End = '_us.csv',
                          Date = 2020-04-30)  #Sys.Date()-4)
{
  
  YYG_Model<-vroom::vroom(paste0(Front,Date,End))

  return(YYG_Model)
  
}