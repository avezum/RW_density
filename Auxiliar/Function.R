##-----------------------------------------------------------------------------##
## Project: PD distribution and capital savings                                ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file loads functions                                      ##
##-----------------------------------------------------------------------------##

##=============================================================================##
## Basel II/III capital requirements formulas                                  ##
##=============================================================================##

mapping_wholesale <- function(PD, LGD = 0.45, M = 2.5, alpha = 0.999){
  if(PD==0){
    R  <- NA
    K  <- 0
    RW <- 0
  } else{
  LGD <- ifelse(is.na(LGD), 0.45, LGD)
  R   <- 0.12*(1-exp(-50*PD))/(1-exp(-50))+0.24*(1-(1-exp(-50*PD))/(1-exp(-50)))
  b   <- (0.11852-0.05478*log(PD))^2
  K   <- 1.06*LGD*(pnorm(((1-R)^(-0.5))*qnorm(PD)+((R/(1-R))^(0.5))*qnorm(alpha))-PD)*((1+(M-2.5)*b)/(1-1.5*b))
  RW  <- K*12.5
  data <- data.frame(K,RW,PD,LGD,M,R)
  return(data)
}
}

mapping_retail <- function(PD, LGD, R, alpha = 0.999){
  if(PD==0){
    K  <- 0
    RW <- 0
  } else{
  K  <- 1.06*LGD*(pnorm(((1-R)^(-0.5))*qnorm(PD)+((R/(1-R))^(0.5))*qnorm(alpha))-PD)
  RW <- K*12.5
  data <- data.frame(K,RW,PD,LGD,R)
  return(data)
  }
}

mapping_other_retail <- function(PD, LGD, alpha = 0.999){
  if(PD==0){
    R  <- NA
    K  <- 0
    RW <- 0
  } else{
  R  <- 0.03*(1-exp(-35*PD))/(1-exp(-35))+0.16*(1-(1-exp(-35*PD))/(1-exp(-35)))
  K  <- 1.06*LGD*(pnorm(((1-R)^(-0.5))*qnorm(PD)+((R/(1-R))^(0.5))*qnorm(alpha))-PD)
  RW <- K*12.5
  data <- data.frame(K,RW,PD,LGD,R)
  return(data)
  }  
}
