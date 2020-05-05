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

  shaman.lab.json = jsonlite::fromJSON("https://api.github.com/repos/shaman-lab/COVID-19Projection/contents?per_page=100")
  shaman.lab.path = get.shaman.lab.path()

  R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_60contact.csv"),
                        filename = "www/4_load_external_data/data_files/bed_60contact.csv",
                        overwrite = T)
  R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_70contact.csv"),
                        filename = "www/4_load_external_data/data_files/bed_70contact.csv",
                        overwrite = T)
  R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_80contact.csv"), 
                        filename = "www/4_load_external_data/data_files/bed_80contact.csv", 
                        overwrite = T)  
  R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_nointerv.csv"), 
                        filename = "www/4_load_external_data/data_files/bed_nointerv.csv", 
                        overwrite = T)  
  
  
  # YYG.json = jsonlite::fromJSON("https://api.github.com/repos/youyanggu/COVID-19Projection/contents?per_page=100")
  # YYG.path = get_YYG_path()
  # 
  # R.utils::downloadFile(paste0("https://github.com/youyanggu/covid19_projections/blob/master/projections/combined/",YYG.path,"_us.csv"), 
  #                       filename = "www/4_load_external_data/data_files/YYG_projections.csv", 
  #                       overwrite = T)  
  
  
  R.utils::downloadFile("https://covid-19.bsvgateway.org/forecast/forecast_metadata.json",
                        filename = "www/4_load_external_data/data_files/forecast_metadata.json",
                        overwrite = T)
  
  bsv_metadata<-jsonlite::fromJSON("www/4_load_external_data/data_files/forecast_metadata.json")
  
  Front<-'https://covid-19.bsvgateway.org/forecast/us/files/'
  Middle1<-'/confirmed/'
  End1<-'_confirmed_quantiles_us_website.csv'
  Middle2<-'/deaths/'
  End2<-'_deaths_quantiles_us_website.csv'  
  Date<-bsv_metadata$us$most_recent_date
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
  
} else {
  
  bsv_metadata<-jsonlite::fromJSON("www/4_load_external_data/data_files/forecast_metadata.json")
  End1<-'_confirmed_quantiles_us_website.csv'
  End2<-'_deaths_quantiles_us_website.csv'    
  Date<-bsv_metadata$us$most_recent_date
  LANL_file_name1 = paste0("www/4_load_external_data/data_files/",Date,End1)
  LANL_file_name2 = paste0("www/4_load_external_data/data_files/",Date,End2)  
  print("data is current")
  
}
