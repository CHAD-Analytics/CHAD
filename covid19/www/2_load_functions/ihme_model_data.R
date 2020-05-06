#' Read in IHME data for projecting data in the future
#'
#' @importFrom utils download.file unzip read.table
#' @importFrom tools file_ext
ihme_model_data <- function()
{

  temp = tempfile()
  utils::download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", temp, mode="wb")
  zipdf <- utils::unzip(temp, list = TRUE)
  csv_file <- zipdf$Name[which(tools::file_ext(zipdf$Name) == "csv")]
  
  IHME_Model <- vroom::vroom(unz(temp, csv_file), delim = ",")
  unlink(temp)
  IHME_Model$date <- as.Date(IHME_Model$date, format = "%Y-%m-%d")
  StateList <- data.frame(state.name, state.abb)
  IHME_Model <- merge(IHME_Model, 
                      StateList, 
                      by.x = names(IHME_Model)[2], 
                      by.y = names(StateList)[1])
  
  names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"

  return(IHME_Model)
  
}
