AFWings = vroom::vroom("www/4_load_external_data/data_files/AF_Wings2.csv")

WingList <- sort(unique(AFWings$`Wing`), decreasing = FALSE)
WingList<-c("All",WingList)