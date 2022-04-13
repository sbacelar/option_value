library(tidyverse)

library(readr)
ex <- read_delim("data/stockwise_ex.csv", 
                 delim = ";", escape_double = FALSE,
                 trim_ws = TRUE)
rate = 0.05

Y <- ex$`Earnings Forecast`
B <- ex$`Incl_SS_Offset`
t <- ex$Age[1]

S <- ex$Age[19]

ov1 <- 0
calc_opvalue1 <- function(t, r, S, Y, B, rate) {
  for (i in t:(r-1)){
    ov1 <- ov1 + Y[i-t+1]/(1+rate)^(i-t)
  }
  return(ov1)
}

ov2 <- 0
calc_opvalue2 <- function(t, r, S, Y, B, rate) {
  for (i in r:S){
    ov2 <- ov2 + B[i-r+1]/(1+rate)^(i-t)
  }
  return(ov2)
}

r <- ex$Age[7]
r <- ex$Age[10]
r <- ex$Age[16]
r <- ex$Age[17]
r <- ex$Age[19]

sumY <- calc_opvalue1(t, r, S, Y, B, rate)
sumB <- calc_opvalue2(t, r, S, Y, B, rate)
sumTot <- sumY + sumB
sumTot
