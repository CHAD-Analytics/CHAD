GetHospitals<-function(base,radius){
  
    #Find number of hospitals in radius
    HospitalInfo$DistanceMiles = himd[,as.character(base)]
    IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= radius))
    IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
    IncludedHospitals
    
}
