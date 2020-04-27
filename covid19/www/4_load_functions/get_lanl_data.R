#'  Get data from Los Alamos National Lab
#' @param front front of url
#' @param middle middle of url
#' @param end end of url 
#' @param date Have to adjust the integer value based on the date of the most recent forecast file
#' @export
#' @importFrom vroom vroom
get_lanl_data <- function(front = 'https://covid-19.bsvgateway.org/forecast/us/files',
                          middle = '/confirmed/',
                          end = '_confirmed_quantiles_us.csv',
                          date = Sys.Date()-4)
{
  
LANL_Data<-vroom::vroom(paste0(Front,Date,Middle,date,End))

LANL_Data<-subset(LANL_Data, 
                  select = -c(simple_state,q.01,q.025,q.05,q.10,q.15,q.20,q.30,q.35,q.40,q.45,q.55,q.60,q.65,q.70,q.80,q.85,q.90,q.95,q.975,q.99,truth_confirmed,fcst_date))

LANL_Data <- merge(LANL_Data, 
                   StateList, 
                   by.x = names(LANL_Data)[6], 
                   by.y = names(StateList)[1])

names(LANL_Data)[names(LANL_Data)=="state.abb"] <- "State"

return(LANL_Data)

}
