get.shaman.lab.path = function(repo_url = "https://github.com/shaman-lab/COVID-19Projection") {
  
  repo_code = xml2::read_html(repo_url)
  
  anchors = rvest::html_nodes(repo_code, "a")
  
  anchor_titles = rvest::html_attr(anchors, name = "title")
  
  projection_folder_names = grep(anchor_titles, 
                                 pattern = "([Pp]rojection_)(\\S*\\d+$)", 
                                 value = T)
  
  cut_off_Projection_ = gsub(pattern = "[Pp]rojection_",
                             replacement = "",
                             projection_folder_names)
  
  o = order(as.Date(cut_off_Projection_, format = "%B%d"))
  
  return(projection_folder_names[o][length(o)])
  
}