rm(list=ls())
library(qs)
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
a_window <- 90
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
  
  # PWR 1.5
  out <- rbind(out, as.list(t(getSRM_PWR_h_w(ret[t_start:t_end,], 1.5))))
  rownames(out)[2] <- "SRM_PWR_1_5"
  
  # PWR 3
  out <- rbind(out, as.list(t(getSRM_PWR_h_w(ret[t_start:t_end,], 3))))
  rownames(out)[3] <- "SRM_PWR_3"

  # PWR 5
  out <- rbind(out, as.list(t(getSRM_PWR_h_w(ret[t_start:t_end,], 5))))
  rownames(out)[4] <- "SRM_PWR_5"

  # PWR 10
  out <- rbind(out, as.list(t(getSRM_PWR_h_w(ret[t_start:t_end,], 10))))
  rownames(out)[5] <- "SRM_PWR_10"

  # PWR 15
  out <- rbind(out, as.list(t(getSRM_PWR_h_w(ret[t_start:t_end,], 15))))
  rownames(out)[6] <- "SRM_PWR_15"
    
  out <- out[-1,]
  return(out)
}

Sys.time()-start_time

qsave(results, "data/Weights/weights_90d_PWR_h.qs")

