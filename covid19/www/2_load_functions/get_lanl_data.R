#'  Get data from Los Alamos National Lab
#' @param front front of url
#' @param middle middle of url
#' @param end end of url 
#' @param date Have to adjust the integer value based on the date of the most recent forecast file
#' @export
#' @importFrom vroom vroom
get_lanl_data <- function(front = 'https://covid-19.bsvgateway.org/forecast/us/files',
                          middle1 = '/confirmed/',middle2 = '/deaths/',
                          end1 = '_confirmed_quantiles_us.csv',end2 = '_deaths_quantiles_us.csv',
                          date = Sys.Date()-4)
{
  
  LANLC_Data<-vroom::vroom(paste0(Front,Date,Middle1,date,End1))
  LANLD_Data<-vroom::vroom(paste0(Front,Date,Middle2,date,End2))
  
  LANLC_Data<-subset(LANLC_Data, 
                    select = -c(simple_state,q.01,q.025,q.05,q.10,q.15,q.20,q.30,q.35,q.40,q.45,q.55,q.60,q.65,q.70,q.80,q.85,q.90,q.95,q.975,q.99,truth_confirmed,fcst_date))
  LANLC_Data <- merge(LANLC_Data, 
                      StateList, 
                      by.x = names(LANLC_Data)[6], 
                      by.y = names(StateList)[1])
  
  LANLD_Data<-subset(LANLD_Data, 
                     select = -c(simple_state,q.01,q.025,q.05,q.10,q.15,q.20,q.30,q.35,q.40,q.45,q.55,q.60,q.65,q.70,q.80,q.85,q.90,q.95,q.975,q.99,truth_deaths,fcst_date))  
  LANLD_Data <- merge(LANLD_Data, 
                     StateList, 
                     by.x = names(LANLD_Data)[6], 
                     by.y = names(StateList)[1])
  
  names(LANLC_Data)[names(LANLC_Data)=="state.abb"] <- "State"
  names(LANLD_Data)[names(LANLD_Data)=="state.abb"] <- "State"  

  return(list(LANLC_Data,LANLD_Data))
  
}