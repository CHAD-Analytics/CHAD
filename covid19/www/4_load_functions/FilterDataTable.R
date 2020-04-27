#' Filter a data table
#' @description This just filters the data table based on IHME or CHIME
FilterDataTable<-function(dt,ModelType,ForecastType){
  if (ModelType == "IHME") {
    if (ForecastType == "Today"){
      cols<-c(1:10)
    } else if(ForecastType == "Seven"){
      cols<-c(1:10)
    } else if(ForecastType == "Fourteen"){
      cols<-c(1:7,14,15,16)  
    } else if(ForecastType == "Twenty-One"){            
      cols<-c(1:7,20,21,22)                  
    } else if(ForecastType == "Thirty"){          
      cols<-c(1:7,26,27,28)                                    
    }
    dt[, names(dt)[cols]]    
  } else {
    if (ForecastType == "Today"){
      cols<-c(1:10)
    } else if(ForecastType == "Seven"){
      cols<-c(1:10)
    } else if(ForecastType == "Fourteen"){
      cols<-c(1:7,17,18,19)  
    } else if(ForecastType == "Twenty-One"){            
      cols<-c(1:7,23,24,25)                  
    } else if(ForecastType == "Thirty"){          
      cols<-c(1:7,29,30,31)                                    
    }
    dt[, names(dt)[cols]]    
  }
}
