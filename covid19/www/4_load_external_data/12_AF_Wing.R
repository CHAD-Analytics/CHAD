AFNAFS = vroom::vroom("www/4_load_external_data/data_files/AF_Wings2.csv")

NAFList <- sort(unique(AFNAFS$`NAF`), decreasing = FALSE)
