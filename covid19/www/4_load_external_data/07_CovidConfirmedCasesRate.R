v <- rep(0, as.numeric(ncol(CovidConfirmedCases)))

for (i in 1:nrow(CovidConfirmedCases)){
    
    j = 0
    cases = CovidConfirmedCases[i,ncol(CovidConfirmedCases)]
    
    if (cases != 0){
        
        while (cases/2 < CovidConfirmedCases[i,ncol(CovidConfirmedCases)-j])
        {
            j = j + 1
            if (j>100){
              j = 100
              break
            }
        }
        
        days = j
        
    } else {
      
        days = 0
        
    }
    
    v[i] <- days
    
}

CovidConfirmedCasesRate <- cbind(CovidConfirmedCases,v)
