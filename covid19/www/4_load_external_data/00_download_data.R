try(test_date <- as.Date(file.info("www/7_other_resources/forecast_metadata.json")$ctime))

if (is.na(test_date)) { test_date <- Sys.Date()-1}

if(test_date < Sys.Date()) {
  
  print("data is not current. downloading curent data...")
  
  # R.utils::downloadFile("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
  #                       filename = "www/4_load_external_data/data_files/time_series_covid19_confirmed_US.csv",
  #                       skip = F,
  #                       overwrite = T)
  # R.utils::downloadFile("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv",
  #                       filename = "www/4_load_external_data/data_files/time_series_covid19_deaths_US.csv",
  #                       skip = F,
  #                       overwrite = T)
  R.utils::downloadFile("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", 
                        filename = "www/4_load_external_data/data_files/ihme-covid19.zip", 
                        overwrite = T)  
  
  # R.utils::downloadFile("https://open-covid-19.github.io/data/data.csv",
  #                       filename = "www/4_load_external_data/data_files/data.csv",
  #                       skip = F,
  #                       overwrite = T)

  GlobalData = as.data.frame(data.table::fread("https://open-covid-19.github.io/data/data.csv"))
  GlobalData1 = as.data.frame(data.table::fread("https://open-covid-19.github.io/data/v2/epidemiology.csv"))
  GlobalData2 = as.data.frame(data.table::fread("https://open-covid-19.github.io/data/v2/hospitalizations.csv")) 
  
  GlobalActive = GlobalData %>% full_join(GlobalData1,by=c("Key" = "key","Date"="date"))
  GlobalActive = GlobalActive %>% full_join(GlobalData2,by=c("Key" = "key","Date"="date"))  
  
  GlobalActive = inner_join(GlobalActive,CountyInfo, by = "Key")
  #colset<-c(Latitude.x,Longitude.x,Population.x,Latitude.y,Longitude.y,Population.y,Sort,'County Seat',DistanceMiles)
  #colset <- c(9,10,11,29,33,34,35,36,37)
  colset <- c(1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,32,38,39)
  GlobalActive<-GlobalActive[, names(GlobalActive)[colset]] 

 
  
  #GlobalActive = GlobalActive %>% full_join(GlobalData2,by=c("Key" = "key","Date"="date"))  
  
  # shaman.lab.json = jsonlite::fromJSON("https://api.github.com/repos/shaman-lab/COVID-19Projection/contents?per_page=100")
  # shaman.lab.path = get.shaman.lab.path()
  # 
  # R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_80contact1x10p.csv"),
  #                       filename = "www/4_load_external_data/data_files/bed_80contact1x10p.csv",
  #                       overwrite = T)
  # R.utils::downloadFile(paste0(" https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/Projection_May10/bed_80contact1x5p.csv"), 
  #                       filename = "www/4_load_external_data/data_files/bed_80contact1x5p.csv", 
  #                       overwrite = T)  
  # R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_80contact1w10p.csv"),
  #                       filename = "www/4_load_external_data/data_files/bed_80contactw10p.csv",
  #                       overwrite = T)
  # R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_80contact1w5p.csv"), 
  #                       filename = "www/4_load_external_data/data_files/bed_80contactw5p.csv", 
  #                       overwrite = T)  
    
  # YYG.json = jsonlite::fromJSON("https://api.github.com/repos/youyanggu/COVID-19Projection/contents?per_page=100")
  # YYG.path = get_YYG_path()
  # 
  # R.utils::downloadFile(paste0("https://github.com/youyanggu/covid19_projections/blob/master/projections/combined/",YYG.path,"_us.csv"), 
  #                       filename = "www/4_load_external_data/data_files/YYG_projections.csv", 
  #                       overwrite = T)  
  
  
  
  ###Global case forecasts are coming from IHME/LANL/YYG/CHIME
  ###IHME is the most difficult to translate because they don't have a separate country column
  #R.utils::downloadFile("https://covid-19.bsvgateway.org/forecast/forecast_metadata.json",
  #                      filename = "www/4_load_external_data/data_files/forecast_metadata.json",
  #                      overwrite = T)
  
  #bsv_metadata<-jsonlite::fromJSON("www/4_load_external_data/data_files/forecast_metadata.json")
  
  #Store metadata link
  URL_metadata <- "https://covid-19.bsvgateway.org/forecast/forecast_metadata.json"
  
  #Create temp file
  temp <- tempfile()
  
  #Download the json file and store in temp
  download.file(URL_metadata, temp)
  
  #pull the json attributes
  LANL_metadata <- fromJSON(temp)
  unlink(temp)
  Date <- LANL_metadata$us$most_recent_date

  Front<-'https://covid-19.bsvgateway.org/forecast/us/files/'
  Middle1<-'/confirmed/'
  End1<-'_confirmed_quantiles_us_website.csv'
  Middle2<-'/deaths/'
  End2<-'_deaths_quantiles_us_website.csv'  
  #Date<- "2020-06-17" #bsv_metadata$us$most_recent_date
  ReadIn<-paste0(Front,Date,Middle1,Date,End1)
  LANL_file_name1 = paste0("www/4_load_external_data/data_files/",Date,End1)
  R.utils::downloadFile(ReadIn,
                        filename = LANL_file_name1,
                        overwrite = T)
  ReadIn<-paste0(Front,Date,Middle2,Date,End2)
  LANL_file_name2 = paste0("www/4_load_external_data/data_files/",Date,End2)
  R.utils::downloadFile(ReadIn,
                        filename = LANL_file_name2,
                        overwrite = T)  

  Front<-'https://covid-19.bsvgateway.org/forecast/global/files/'
  Middle1<-'/confirmed/'
  End1<-'_confirmed_quantiles_global_website.csv'
  Middle2<-'/deaths/'
  End2<-'_deaths_quantiles_global_website.csv'  
  #Date<- "2020-06-17" #bsv_metadata$us$most_recent_date
  ReadIn<-paste0(Front,Date,Middle1,Date,End1)
  LANL_file_name3 = paste0("www/4_load_external_data/data_files/",Date,End1)
  R.utils::downloadFile(ReadIn,
                        filename = LANL_file_name3,
                        overwrite = T)
  ReadIn<-paste0(Front,Date,Middle2,Date,End2)
  LANL_file_name4 = paste0("www/4_load_external_data/data_files/",Date,End2)
  R.utils::downloadFile(ReadIn,
                        filename = LANL_file_name4,
                        overwrite = T)    
    
} else {
  
  #bsv_metadata<-jsonlite::fromJSON("www/4_load_external_data/data_files/forecast_metadata.json")
  
  #Store metadata link
  URL_metadata <- "https://covid-19.bsvgateway.org/forecast/forecast_metadata.json"
  #Create temp file
  temp <- tempfile()
  #Download the json file and store in temp
  download.file(URL_metadata, temp)
  #pull the json attributes
  LANL_metadata <- fromJSON(temp)
  unlink(temp)
  Date <- LANL_metadata$us$most_recent_date
  
  End1<-'_confirmed_quantiles_us_website.csv'
  End2<-'_deaths_quantiles_us_website.csv'    
  #Date<- "2020-06-17" #bsv_metadata$us$most_recent_date
  LANL_file_name1 = paste0("www/4_load_external_data/data_files/",Date,End1)
  LANL_file_name2 = paste0("www/4_load_external_data/data_files/",Date,End2)  
  End1<-'_confirmed_quantiles_global_website.csv'
  End2<-'_deaths_quantiles_global_website.csv'    
  #Date<-bsv_metadata$us$most_recent_date
  LANL_file_name3 = paste0("www/4_load_external_data/data_files/",Date,End1)
  LANL_file_name4 = paste0("www/4_load_external_data/data_files/",Date,End2)    
  
  print("data is current")
  
}
