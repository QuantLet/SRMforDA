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

  
  # Minimum Variance
  out <- rbind(out, as.list(t(quadprog::solve.QP(Dmat = 2*cov(ret[t_start:t_end,]),
                                                 dvec = rep(0, ncol(ret)),
                                                 Amat = cbind(rep(1,ncol(ret)),diag(ncol(ret))),
                                                 bvec = c(1, rep(0, ncol(ret))))$solution
                              )))
  out[2,out[2,]<0] <- 0
  rownames(out)[2] <- "MinVar"
  
  return(out)
}

Sys.time()-start_time

qsave(results, "data/Weights/weights_30d.qs")

