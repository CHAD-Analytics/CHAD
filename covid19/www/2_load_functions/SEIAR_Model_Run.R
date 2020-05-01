#' Function to establish the Army SEIAR model
#'
#' @description Uses US Army models to create projections 
#'              for the local area around a base
#'
#' @details This function allows us to pass though a simple 
#'          function to gather all statistics when we plot
#Use army models to create projections for the local area around the base
#Establish function for army SEIAR model. This allows us to pass though a simple function to gather all statistics when we plot
SEIAR_Model_Run<-function(num_init_cases, 
                          Pop.At.Risk, 
                          incub_period, 
                          latent_period, 
                          doubling, 
                          recovery_days, 
                          social_rate, 
                          hospital_rate,
                          icu_rate, 
                          ventilated_rate, 
                          hospital_dur, 
                          icu_dur, 
                          ventilated_dur, 
                          n_days, 
                          secondary_cases = 2.5, 
                          distribution_e_to_a = 0.5){
    
    ###DEFINING COMPARTMENTS OF THE MODEL
    total_infections <- num_init_cases / (hospital_rate/100) 
    I <- total_infections 
    S <- (Pop.At.Risk - I)
    E <- (total_infections*secondary_cases) * distribution_e_to_a #Assuming that each infectious person will generate 2.5 secondary cases
    A <- (total_infections*secondary_cases) * (1-distribution_e_to_a) ##Distributing the secondary cases between the E and A compartments
    R <- 0
    
    ###DEFINING PARAMETERS
    intrinsic_growth_rate <- 2 ^(1 / doubling) -1
    sigma <- 1/latent_period #Latent period (approx 2 days)
    gamma_1 <- 1/(incub_period - latent_period)
    gamma_2 <- 1/(recovery_days - latent_period - (incub_period - latent_period))
    beta <- (intrinsic_growth_rate + (1/recovery_days)) / S * (1-social_rate/100)
    r_t <- beta / (1/recovery_days) * S 
    r_0 <- r_t / (1 - social_rate/100)
    doubling_time_t <- 1 / log2(beta*S - (1/recovery_days) + 1)
    
    ###ITERATIVE LISTING OF MODEL
    myList <- list()
    myList$total_infections <- total_infections
    myList$S <- S
    myList$E <- E
    myList$A <- A
    myList$I <- I
    myList$R <- R
    myList$intrinsic_growth_rate <- intrinsic_growth_rate
    myList$sigma <- sigma
    myList$gamma_1 <- gamma_1
    myList$gamma_2 <- gamma_2
    myList$beta <- beta
    myList$r_t <- r_t
    myList$r_0 <- r_0
    myList$doubling_time_t <- doubling_time_t
    
    #initial values
    N = S + E + A + I + R
    hos_add <- ((A+I) * hospital_rate/100)
    hos_cum <- ((A+I) * hospital_rate/100)
    icu_add <- (hos_add * icu_rate/100)
    icu_cum <- (hos_cum * icu_rate/100)
    
    #create the data frame
    sir_data <- data.frame(t = 1,
                           S = S,
                           E = E,
                           A = A,
                           I = I,
                           R = R,
                           hos_add = hos_add,
                           hos_cum = hos_cum,
                           icu_add = hos_add * icu_rate/100,
                           icu_cum = hos_cum * icu_rate/100,
                           vent_add = icu_add * ventilated_rate/100,
                           vent_cum = icu_cum * ventilated_rate/100,
                           Id = 0
    )
    
    for(i in 2:n_days){
        y <- seiar(S,E,A,I,R, beta, sigma, gamma_1, gamma_2, N)
        S <- y$S
        E <- y$E
        A <- y$A
        I <- y$I
        R <- y$R
        
        #calculate new infections
        Id <- (sir_data$S[i-1] - S)
        
        #portion of the the newly infected that are in the hospital, ICU, and Vent
        hos_add <- Id * hospital_rate/100
        hos_cum <- sir_data$hos_cum[i-1] + hos_add
        
        icu_add <- hos_add * icu_rate/100
        icu_cum <- sir_data$icu_cum[i-1] + icu_add
        
        vent_add <- icu_add * ventilated_rate/100 
        vent_cum <- sir_data$vent_cum[i-1] + vent_add
        
        temp <- data.frame(t = i,
                           S = S,
                           E = E,
                           A = A,
                           I = I,
                           R = R,
                           hos_add = hos_add,
                           hos_cum = hos_cum,
                           icu_add = icu_add,
                           icu_cum = icu_cum,
                           vent_add = vent_add,
                           vent_cum = vent_cum,
                           Id = Id
        )
        
        sir_data <- rbind(sir_data,temp)
    }
    
    #doing some weird stuff to get a rolling sum of hospital impacts based on length of stay (los)
    if(n_days > hospital_dur){
        h_c <- zoo::rollsum(sir_data$hos_add,hospital_dur)
        sir_data$hos_cum <- c(sir_data$hos_cum[1:(n_days - length(h_c))],h_c)
    } 
    if(n_days > icu_dur){
        i_c <- zoo::rollsum(sir_data$icu_add,icu_dur)
        sir_data$icu_cum <- c(sir_data$icu_cum[1:(n_days - length(i_c))],i_c)
    } 
    if(n_days > ventilated_dur){
        v_c <- zoo::rollsum(sir_data$vent_add,ventilated_dur)
        sir_data$vent_cum <- c(sir_data$vent_cum[1:(n_days - length(v_c))],v_c)
    } 
    
    #write.csv(sir_data, file = 'test.csv') # for testing
    h_m <- round(max(sir_data$hos_cum), 0)
    i_m <- round(max(sir_data$icu_cum), 0)
    v_m <- round(max(sir_data$vent_cum), 0)
    myList$sir <- sir_data
    myList$hos_max <- h_m
    myList$icu_max <- i_m
    myList$vent_max <- v_m 
    
    h_m <- sir_data$t[which.max(sir_data$hos_cum)][1]
    i_m <- sir_data$t[which.max(sir_data$icu_cum)][1]
    v_m <- sir_data$t[which.max(sir_data$vent_cum)][1]
    myList$hos_t_max <- h_m
    myList$icu_t_max <- i_m
    myList$vent_t_max <- v_m 
    
    h_m <- round(max(sir_data$hos_add), 0)
    i_m <- round(max(sir_data$icu_add), 0)
    v_m <- round(max(sir_data$vent_add), 0)
    
    myList$hos_add <- h_m
    myList$icu_add <- i_m
    myList$vent_add <- v_m 
    return(myList)
}
