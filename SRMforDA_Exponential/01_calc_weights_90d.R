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
print("Loop start")
results <- foreach (i = c(1:n_per),
                    .packages='tidyverse') %dopar% {

  t_start = 1 + (i-1)*a_window
  t_end = t_window + (i-1)*a_window
  
  # NAIVE
  out <- data.frame(t(c(rep(1/ncol(ret), ncol(ret)))))
  colnames(out) <- colnames(ret)
  rownames(out)[1] <- "Naive"
  
  # EXP 1
  out <- rbind(out, as.list(t(getSRM_EXP_w(ret[t_start:t_end,], 1))))
  rownames(out)[2] <- "SRM_EXP_1"
  
  # EXP 5
  out <- rbind(out, as.list(t(getSRM_EXP_w(ret[t_start:t_end,], 5))))
  rownames(out)[3] <- "SRM_EXP_5"

  # EXP 10
  out <- rbind(out, as.list(t(getSRM_EXP_w(ret[t_start:t_end,], 10))))
  rownames(out)[4] <- "SRM_EXP_10"

  # EXP 15
  out <- rbind(out, as.list(t(getSRM_EXP_w(ret[t_start:t_end,], 15))))
  rownames(out)[5] <- "SRM_EXP_15"
  
  # EXP 20
  out <- rbind(out, as.list(t(getSRM_EXP_w(ret[t_start:t_end,], 20))))
  rownames(out)[6] <- "SRM_EXP_20"
  
  # EXP 25
  out <- rbind(out, as.list(t(getSRM_EXP_w(ret[t_start:t_end,], 25))))
  rownames(out)[7] <- "SRM_EXP_25"
    
  out <- out[-1,]
  return(out)
}

Sys.time()-start_time

qsave(results, "data/Weights/weights_90d_EXP.qs")

