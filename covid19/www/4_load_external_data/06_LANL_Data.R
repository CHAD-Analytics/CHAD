LANLC_Data = vroom::vroom(LANL_file_name1)

LANLD_Data = vroom::vroom(LANL_file_name2)

LANLC_Data = subset(LANLC_Data, 
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

LANLD_Data = subset(LANLD_Data, 
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
                                truth_deaths,
                                fcst_date))

LANLC_Data <- merge(LANLC_Data, 
                   StateList, # defined in 1_StateInfo.R
                   by.x = names(LANLC_Data)[6], 
                   by.y = names(StateList)[1])

LANLD_Data <- merge(LANLD_Data, 
                   StateList, # defined in 1_StateInfo.R
                   by.x = names(LANLD_Data)[6], 
                   by.y = names(StateList)[1])

names(LANLC_Data)[names(LANLC_Data)=="state.abb"] <- "State"

names(LANLD_Data)[names(LANLD_Data)=="state.abb"] <- "State"