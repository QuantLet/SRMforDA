rm(list=ls())
library(qs)
library(data.table)
library(quadprog)
library(nloptr)
library(parallel)
library(foreach)
library(doParallel)
source('R/portfolio.R')

################################################################################
############################# Read data ########################################
################################################################################
ret <- qread("data/coin_return.qs")
colnames(ret)[1] <- "Date"
ret <- ret[-c(1:32),-1]

### Portfolio parameters
t_window <- 365
a_window <- 30
n_per <- floor((nrow(ret)-t_window)/a_window)



################################################################################
################################# LOOP #########################################
################################################################################
### LOOP for weights
cl=makeCluster(detectCores(all.tests = FALSE, logical = FALSE),outfile="")
registerDoParallel(cores=cl)

(start_time <- Sys.time())

results <- foreach (i = c(1:n_per),
                    .packages='tidyverse') %dopar% {

  t_start = 1 + (i-1)*a_window
  t_end = t_window + (i-1)*a_window
  
  # NAIVE
  out <- data.frame(t(c(rep(1/ncol(ret), ncol(ret)))))
  colnames(out) <- colnames(ret)
  rownames(out)[1] <- "Naive"
  
  # PWR 01
  out <- rbind(out, as.list(t(getSRM_PWR_l_w(ret[t_start:t_end,], 0.1))))
  rownames(out)[2] <- "SRM_PWR_01"
  
  # PWR 03
  out <- rbind(out, as.list(t(getSRM_PWR_l_w(ret[t_start:t_end,], 0.3))))
  rownames(out)[3] <- "SRM_PWR_03"

  # PWR 05
  out <- rbind(out, as.list(t(getSRM_PWR_l_w(ret[t_start:t_end,], 0.5))))
  rownames(out)[4] <- "SRM_PWR_05"

  # PWR 07
  out <- rbind(out, as.list(t(getSRM_PWR_l_w(ret[t_start:t_end,], 0.7))))
  rownames(out)[5] <- "SRM_PWR_07"

  # PWR 09
  out <- rbind(out, as.list(t(getSRM_PWR_l_w(ret[t_start:t_end,], 0.9))))
  rownames(out)[6] <- "SRM_PWR_09"
    
  out <- out[-1,]
  return(out)
}

Sys.time()-start_time

qsave(results, "data/Weights/weights_30d_PWR_l.qs")

