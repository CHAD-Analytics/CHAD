LANL_Data = vroom::vroom(LANL_file_name)

LANL_Data = subset(LANL_Data, 
                   select = -c(simple_state,
                              q.01,
                              q.025,
                              q.05,
                              q.10,
                              q.15,
                              q.20,
                              q.30,
                              q.35,
                              q.40,
                              q.45,
                              q.55,
                              q.60,
                              q.65,
                              q.70,
                              q.80,
                              q.85,
                              q.90,
                              q.95,
                              q.975,
                              q.99,
                              truth_confirmed,
                              fcst_date))

LANL_Data <- merge(LANL_Data, 
                   StateList, # defined in 1_StateInfo.R
                   by.x = names(LANL_Data)[6], 
                   by.y = names(StateList)[1])

names(LANL_Data)[names(LANL_Data)=="state.abb"] <- "State"
