rm(list=ls())
### Libraries
library(qs)
library(xtable)

################################################################################
######################### Descriptive statistics ###############################
################################################################################
Data <- qread("Data/coinReturn.qs")

### Descriptive statistics
options("scipen"=100, "digits"=4)
ST <- data.frame("Mean" = round(apply(Data[,-1], 2, mean, na.rm = TRUE),4),
                 "S.D." = round(apply(Data[,-1], 2, sd, na.rm = TRUE),4),
                 "Min" = round(apply(Data[,-1], 2, min, na.rm = TRUE),4),
                 #"Q 0.25" = round(apply(Data[,-1], 2, quantile, 0.25),4),
                 "Median" = round(apply(Data[,-1], 2, median, na.rm = TRUE),4),
                 #"Q 0.75" = round(apply(Data[,-1], 2, quantile, 0.75),4),
                 "Max" = round(apply(Data[,-1], 2, max, na.rm = TRUE),4),
                 "rho_1" = round(apply(Data[,-1],2, FUN = function (x){ c(acf(x, lag.max = 5, 
                                                                              plot = F, na.action = na.pass)$acf)[2]}),4),
                 "rho_7" = round(apply(Data[,-1],2, FUN = function (x){ c(acf(x, lag.max = 7, 
                                                                              plot = F, na.action = na.pass)$acf)[8]}),4)
)

### Write CSV
write.csv(ST, "Output/Desc_stat.csv")
options("scipen"=0, "digits"=7)

### Write LaTeX file
print(x = xtable(ST, digits = 4), file = "Output/descriptive.tex",
      include.rownames = T, booktabs = T, floating = F)
