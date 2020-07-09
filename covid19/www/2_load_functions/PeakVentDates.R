#' Get the total population in the selected region
PeakVentDates<-function(ChosenBase){

  BaseState<-dplyr::filter(AFBaseLocations, Base == toString(ChosenBase))
  IHME_StateSum <- dplyr::filter(IHME_Summary, State == toString(BaseState$State[1]))
  IHMEBedPeak <- IHME_StateSum$peak_vent_day_mean
  IHMEBedPeak
    
}
