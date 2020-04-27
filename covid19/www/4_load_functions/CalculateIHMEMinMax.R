#' @export
#' @importFrom dplyr filter
CalculateIHMEMinMax<-function(ChosenBase, IncludedHospitals, radius){

    #Creating the stats and dataframes determined by the base we choose to look at.
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
    TotalBedsCounty <- sum(IncludedHospitals$BEDS)

    #Get regional and state populations
    CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
    IncludedCounties <- dplyr::filter(CountyInfo, DistanceMiles <= radius)
    StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
    RegPop <- sum(IncludedCounties$Population)
    StPop <- sum(StPopList$Population)

    # Use Population ratio to scale IHME
    PopRatio <- RegPop/StPop

    # Get total hospital bed number across state
    IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
    TotalBedsState <- sum(IncludedHospitalsST$BEDS)

    # Calculate bed ratio
    BedProp <- TotalBedsCounty/TotalBedsState

    # Apply ratio's to IHME data
    IHME_Region <- IHME_State
    IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean*PopRatio, IHME_Region$allbed_lower*PopRatio, IHME_Region$allbed_upper*PopRatio)
    colnames(IHME_Data)<-c("Date", "Mean", "Lower", "Upper")

    PeakDate<-which.max(IHME_Data$Mean)
    Max<-round(IHME_Data[PeakDate,4])
    Min<-round(IHME_Data[PeakDate,3])
    paste("Min:", Min, ", Max:", Max)
}
