library(tidyverse)

library(readr)
ex <- read_delim("data/stockwise_ex.csv", 
                           delim = ";", escape_double = FALSE,
                           trim_ws = TRUE)
rate = 0.05
Y <- ex[2]
B <- ex[4]
t <- ex$Age[1]
r <- ex$Age[16]
S <- ex$Age[19]

ov <- tibble()
calc_opvalue <- function(t, r, S, Y, B, rate) {
  for (i in t:(r-1)){
    ov <- ov + Y[i-t+1,]/(1+rate)^(i-t)
  }
}

ov <- calc_opvalue(t, r, S, Y, B, rate)

for (i in t:(r-1)){
  print(Y[i-t+1,]/(1+rate)^(i-t))
}
