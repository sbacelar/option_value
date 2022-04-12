library(tidyverse)

library(readr)
ex <- read_delim("data/stockwise_ex.csv", 
                           delim = ";", escape_double = FALSE,
                           trim_ws = TRUE)
rate = 0.05

Y <- ex$`Earnings Forecast`
B <- ex$`Incl_SS_Offset`
t <- ex$Age[1]
r <- ex$Age[16]
S <- ex$Age[19]

ov1 <- vector("numeric", 15)
calc_opvalue1 <- function(t, r, S, Y, B, rate) {
  for (i in t:(r-1)){
    ov1[i-t+1] <- ov1[i-t+1] + Y[i-t+1]/(1+rate)^(i-t)
  }
  return(sum(ov))
}

ov2 <- vector("numeric", 4)
calc_opvalue2 <- function(t, r, S, Y, B, rate) {
  for (i in r:S){
    ov2[i-r+1] <- ov[i-r+1] + B[i-r+1]/(1+rate)^(i-t)
  }
  return(sum(ov2))
}

calc_opvalue1(t, r, S, Y, B, rate)
calc_opvalue2(t, r, S, Y, B, rate)

for (i in t:(r-1)){
  print(i)
  print(Y[i-t+1,]/(1+rate)^(i-t))
}

for (i in r:S){
  print(B[i-r+1]/(1+rate)^(i-t))
}

ov2 <- 0
calc_opvalue2 <- function(t, r, S, Y, B, rate) {
  for (i in r:S){
    ov2 <- ov2 + B[i-r+1]/(1+rate)^(i-t)
  }
  return(ov2)
}
