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
  
  # VaR 0.1
  out <- rbind(out, as.list(t(getSRM_VAR_w(ret[t_start:t_end,], 0.10))))
  rownames(out)[2] <- "VaR_10"
  
  # VaR 0.05
  out <- rbind(out, as.list(t(getSRM_VAR_w(ret[t_start:t_end,], 0.05))))
  rownames(out)[3] <- "VaR_05"
  
  # VaR 0.01
  out <- rbind(out, as.list(t(getSRM_VAR_w(ret[t_start:t_end,], 0.01))))
  rownames(out)[4] <- "VaR_01"
  
  # ES 0.90
  out <- rbind(out, as.list(t(getSRM_ES_w(ret[t_start:t_end,], 0.10))))
  rownames(out)[5] <- "ES_10"
  
  # ES 0.95
  out <- rbind(out, as.list(t(getSRM_ES_w(ret[t_start:t_end,], 0.05))))
  rownames(out)[6] <- "ES_05"
  
  # ES 0.99
  out <- rbind(out, as.list(t(getSRM_ES_w(ret[t_start:t_end,], 0.01))))
  rownames(out)[7] <- "ES_01"
    
  out <- out[-1,]
  return(out)
}

Sys.time()-start_time

qsave(results, "data/Weights/weights_30d_VaRES.qs")

