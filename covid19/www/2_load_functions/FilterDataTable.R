#' Filter a data table
#' @description This just filters the data table based on IHME or CHIME
FilterDataTable<-function(dt,ModelType,ForecastType){
  #if (MAJCOMWingType=="MAJCOM"){}
  #All selections should include first 8 columns
  if (ModelType == "IHME") {
    if (ForecastType == "Today"){
      cols<-c(1:7)
    } else if(ForecastType == "Seven"){
      cols<-c(1:11)
    } else if(ForecastType == "Fourteen"){
      cols<-c(1:7,15,16,17)  
    } else if(ForecastType == "Twenty-One"){            
      cols<-c(1:7,21,22,23)                  
    } else if(ForecastType == "Thirty"){          
      cols<-c(1:7,27,28,29)                                    
    }
    dt[, names(dt)[cols]]    
  } else {
    if (ForecastType == "Today"){
      cols<-c(1:7)
    } else if(ForecastType == "Seven"){
      cols<-c(1:7,12,13,14)
    } else if(ForecastType == "Fourteen"){
      cols<-c(1:7,18,19,20)  
    } else if(ForecastType == "Twenty-One"){            
      cols<-c(1:7,24,25,26)                  
    } else if(ForecastType == "Thirty"){          
      cols<-c(1:7,30,31,32)                                    
    }
    dt[, names(dt)[cols]]    
  }
}
