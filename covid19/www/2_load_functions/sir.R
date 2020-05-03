sir<-function(S,I,R, beta, gamma, N){
    Sn <- (-beta * S * I) + S
    In = (beta * S * I - gamma * I) + I
    Rn = gamma * I + R
    if(Sn < 0) Sn = 0
    if(In < 0) In = 0
    if(Rn < 0) Rn = 0

    scale = N / (Sn + In + Rn )
    myListSIR <- list()
    myListSIR$S <- (Sn * scale)
    myListSIR$I <- (In * scale)
    myListSIR$R <- (Rn * scale)
    return(myListSIR)
}
