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



calc_opvalue <- function(t, r, S, Y, B, rate) {
  ov1 <- vector("numeric", (age_s - age_t + 1))
  ov2 <- vector("numeric", (age_s - age_t + 1))
  
  for (i in t:(r-1)){
    ov1[i-t+1] <- ov1[i-t+1] + Y[i-t+1]/(1+rate)^(i-t)
  }
  p1 <- sum(ov1)
  for (i in r:S){
    ov2[i-r+1] <- ov2[i-r+1] + B[i-r+1]/(1+rate)^(i-t)
  }
  p2 <- sum(ov2)
  p <- p1 + p2 
  return(p)
}

TODO
# maxi <- tibble(51:58,2)
# for (r in 51:68){
#   maxi(r,1) <- r
#   maxi(r,2) <- calc_opvalue(t, r, S, Y, B, rate)
# }

for (r in 51:68){
  print(paste0(r, " -- ", calc_opvalue(t, r, S, Y, B, rate)))
}
