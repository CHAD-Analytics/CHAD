try(test_date <- as.Date(file.info("www/7_other_resources/forecast_metadata.json")$ctime))

if (is.na(test_date)) { test_date <- Sys.Date()-2 }

if(test_date < Sys.Date()) {
  
  print("data is not current. downloading curent data...")
  
  R.utils::downloadFile("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                        filename = "www/3_load_external_data/data_files/time_series_covid19_confirmed_US.csv",
                        overwrite = T)
  R.utils::downloadFile("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv",
                        filename = "www/3_load_external_data/data_files/time_series_covid19_deaths_US.csv",
                        overwrite = T)
  R.utils::downloadFile("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", 
                        filename = "www/3_load_external_data/data_files/ihme-covid19.zip", 
                        overwrite = T)  
  shaman.lab.json = jsonlite::fromJSON("https://api.github.com/repos/shaman-lab/COVID-19Projection/contents?per_page=100")
  shaman.lab.path = "Projection_April23"
  for(k in 14:0) {
    
      my_shaman_lab_path = `if`(lubridate::day(as.Date(Sys.Date()-k))>9,
                                paste0("Projection_",lubridate::month(as.Date(Sys.Date()-k),label=TRUE,abbr=FALSE),lubridate::day(as.Date(Sys.Date()-k))),
                                paste0("Projection_",lubridate::month(as.Date(Sys.Date()-k),label=TRUE,abbr=FALSE),"0",lubridate::day(as.Date(Sys.Date()-k))))
    
      if(max(grepl(my_shaman_lab_path,shaman.lab.json$path,fixed=TRUE))) {
      
         shaman.lab.path = my_shaman_lab_path
         
      }
      
  }
  
  R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_60contact.csv"),
                        filename = "www/3_load_external_data/data_files/bed_60contact.csv",
                        overwrite = T)
  R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_70contact.csv"),
                        filename = "www/3_load_external_data/data_files/bed_70contact.csv",
                        overwrite = T)
  R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_80contact.csv"),
                        filename = "www/3_load_external_data/data_files/bed_80contact.csv",
                        overwrite = T)
  R.utils::downloadFile(paste0("https://raw.githubusercontent.com/shaman-lab/COVID-19Projection/master/",shaman.lab.path,"/bed_nointerv.csv"),
                        filename = "www/3_load_external_data/data_files/bed_nointerv.csv",
                        overwrite = T)
  R.utils::downloadFile("https://covid-19.bsvgateway.org/forecast/forecast_metadata.json",
                        filename = "www/3_load_external_data/data_files/forecast_metadata.json",
                        overwrite = T)
  
  bsv_metadata<-jsonlite::fromJSON("www/3_load_external_data/data_files/forecast_metadata.json")
  
  Front<-'https://covid-19.bsvgateway.org/forecast/us/files/'
  Middle<-'/confirmed/'
  End<-'_confirmed_quantiles_us.csv'
  Date<-bsv_metadata$us$most_recent_date
  ReadIn<-paste0(Front,Date,Middle,Date,End)
  LANL_file_name = paste0("www/3_load_external_data/data_files/",Date,End)
  R.utils::downloadFile(ReadIn,
                        filename = LANL_file_name,
                        overwrite = T)
  
} else {
  
  bsv_metadata<-jsonlite::fromJSON("www/3_load_external_data/data_files/forecast_metadata.json")
  End<-'_confirmed_quantiles_us.csv'
  Date<-bsv_metadata$us$most_recent_date
  LANL_file_name = paste0("www/3_load_external_data/data_files/",Date,End)
  print("data is current")
  
}