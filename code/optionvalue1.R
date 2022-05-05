library(tidyverse)

library(readr)
ex <- read_delim("data/stockwise_ex.csv", 
                           delim = ";", escape_double = FALSE,
                           trim_ws = TRUE)
rate = 0.05 # discount rate

# indexes
age_t <- 1
age_r <- 16
age_s <- 19

Y <- ex$`Earnings Forecast`
B <- ex$`Incl_SS_Offset`
t <- ex$Age[age_t]
r <- ex$Age[age_r]
S <- ex$Age[age_s]

# 50:64
ov1 <- vector("numeric", (age_s - age_t + 1))
#ov1 <- vector("numeric", (age_r - age_t))
calc_opvalue1 <- function(t, r, S, Y, B, rate) {
  for (i in t:(r-1)){
    ov1[i-t+1] <- ov1[i-t+1] + Y[i-t+1]/(1+rate)^(i-t)
  }
  return(sum(ov1))
}

# 65:68
ov2 <- vector("numeric", (age_s - age_t + 1))
#ov2 <- vector("numeric", (age_s - age_r + 1))
calc_opvalue2 <- function(t, r, S, Y, B, rate) {
  for (i in r:S){
    ov2[i-r+1] <- ov2[i-r+1] + B[i-r+1]/(1+rate)^(i-t)
  }
  return(sum(ov2))
}

p1 <- calc_opvalue1(t, r, S, Y, B, rate)
p2 <- calc_opvalue2(t, r, S, Y, B, rate)
p <- p1 + p2

# TODO
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
