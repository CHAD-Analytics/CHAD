AFNAFS = vroom::vroom("www/4_load_external_data/data_files/AF_Wings2.csv")

NAFList <- sort(unique(AFNAFS$`NAF`), decreasing = FALSE)
AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFList)
WingList <- sort(unique(AFWings$Wing), decreasing = FALSE)
WingList <- c("All",WingList)