################################################################################
########################### Functions ##########################################
################################################################################

########################## Value at Risk #######################################
### Find portfolio with minimum Value at Risk
# ret is return of assets which are involved in portfolio
# q is quantile

getSRM_VAR_w = function(ret, q) {
  eval_f = function(w, ret){
    SRM <- quantile(c(w %*% t(ret)), q)
  }
  eval_grad_f = function(w, ret) numDeriv::grad(eval_f,w,ret=ret)
  nloptr::nloptr(x0 = rep(1/ncol(ret),ncol(ret)), #Initial weight set to equal 
                 eval_f=eval_f, 
                 eval_grad_f=eval_grad_f,
                 lb=rep(0,ncol(ret)),
                 ub=rep(1,ncol(ret)),
                 eval_g_ineq = function(w, ret) {0},
                 eval_jac_g_ineq = function(w, ret) {rep(0, ncol(ret))},
                 eval_g_eq = function(w, ret) {sum(w)-1},
                 eval_jac_g_eq = function(w, ret) {rep(1, ncol(ret))},
                 opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_rel' = 1.0e-7, 'maxeval' = 1000),
                 ret = as.matrix(ret))$solution
}




########################## Expected Shortfall ##################################
### Find portfolio with minimum Expected Shortfall
# ret is return of assets which are involved in portfolio
# q is quantile

getSRM_ES_w = function(ret, q) {
  fun_ES <- function(p, data) (1/q)*quantile(-data, p)
  eval_f = function(w, ret){
    SRM <- integrate(f=fun_ES, lower=(1-q), upper=1, data=c(w %*% t(ret)), subdivisions = 10^5, stop.on.error = F)$value
  }
  eval_grad_f = function(w, ret) numDeriv::grad(eval_f,w,ret=ret)
  nloptr::nloptr(x0 = rep(1/ncol(ret),ncol(ret)), #Initial weight set to equal 
                 eval_f=eval_f, 
                 eval_grad_f=eval_grad_f,
                 lb=rep(0,ncol(ret)),
                 ub=rep(1,ncol(ret)),
                 eval_g_ineq = function(w, ret) {0},
                 eval_jac_g_ineq = function(w, ret) {rep(0, ncol(ret))},
                 eval_g_eq = function(w, ret) {sum(w)-1},
                 eval_jac_g_eq = function(w, ret) {rep(1, ncol(ret))},
                 opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_rel' = 1.0e-7, 'maxeval' = 1000),
                 ret = as.matrix(ret))$solution
}

################### Exponentional Spectral Risk Measure ########################
### Find portfolio with minimum Exponentional Spectral Risk Measure
# ret is return of assets which are involved in portfolio
# k is Exponentional Spectral Risk Measure parameter
getSRM_EXP_w = function(ret, k) {
  fun_EXP <- function(p, data) (k/(1-exp(-k)))*exp(-k*(1-p))*quantile(-data,p)
  eval_f = function(w, ret){
    SRM <- integrate(f=fun_EXP, lower=0, upper=1, data=c(w %*% t(ret)), subdivisions = 10^5, stop.on.error = F)$value
  }
  eval_grad_f = function(w, ret) numDeriv::grad(eval_f,w,ret=ret)
  nloptr::nloptr(x0 = rep(1/ncol(ret),ncol(ret)), #Initial weight set to equal 
                 eval_f=eval_f, 
                 eval_grad_f=eval_grad_f,
                 lb=rep(0,ncol(ret)),
                 ub=rep(1,ncol(ret)),
                 eval_g_ineq = function(w, ret) {0},
                 eval_jac_g_ineq = function(w, ret) {rep(0, ncol(ret))},
                 eval_g_eq = function(w, ret) {sum(w)-1},
                 eval_jac_g_eq = function(w, ret) {rep(1, ncol(ret))},
                 opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_rel' = 1.0e-7, 'maxeval' = 1000),
                 ret = as.matrix(ret))$solution
}


####################### Power Spectral Risk Measure ############################
### Find portfolio with minimum Power Spectral Risk Measure for gamma < 1
# ret is return of assets which are involved in portfolio
# gamma is Power Spectral Risk Measure parameter

getSRM_PWR_l_w = function(ret, gamma) {
  fun_PWR_l <- function(p, data) gamma*(1-p)^(gamma-1)*quantile(-data,p)
  eval_f = function(w, ret){
    SRM <- integrate(f=fun_PWR_l, lower=0, upper=1, data=c(w %*% t(ret)), subdivisions = 10^5, stop.on.error = F)$value
  }
  eval_grad_f = function(w, ret) numDeriv::grad(eval_f,w,ret=ret)
  nloptr::nloptr(x0 = rep(1/ncol(ret),ncol(ret)), #Initial weight set to equal 
                 eval_f=eval_f, 
                 eval_grad_f=eval_grad_f,
                 lb=rep(0,ncol(ret)),
                 ub=rep(1,ncol(ret)),
                 eval_g_ineq = function(w, ret) {0},
                 eval_jac_g_ineq = function(w, ret) {rep(0, ncol(ret))},
                 eval_g_eq = function(w, ret) {sum(w)-1},
                 eval_jac_g_eq = function(w, ret) {rep(1, ncol(ret))},
                 opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_rel' = 1.0e-7, 'maxeval' = 1000),
                 ret = as.matrix(ret))$solution
}

### Find portfolio with minimum Power Spectral Risk Measure for gamma > 1
# ret is return of assets which are involved in portfolio
# gamma is Power Spectral Risk Measure parameter

getSRM_PWR_h_w = function(ret, gamma) {
  fun_PWR_h <- function(p, data) gamma*p^(gamma-1)*quantile(-data,p)
  eval_f <- function(w, ret){
    SRM <- integrate(f=fun_PWR_h, lower=0, upper=1, data=c(w %*% t(ret)), subdivisions = 10^5, stop.on.error = F)$value
  }
  eval_grad_f = function(w, ret) numDeriv::grad(eval_f,w,ret=ret)
  nloptr::nloptr(x0 = rep(1/ncol(ret),ncol(ret)), #Initial weight set to equal 
                 eval_f=eval_f, 
                 eval_grad_f=eval_grad_f,
                 lb=rep(0,ncol(ret)),
                 ub=rep(1,ncol(ret)),
                 eval_g_ineq = function(w, ret) {0},
                 eval_jac_g_ineq = function(w, ret) {rep(0, ncol(ret))},
                 eval_g_eq = function(w, ret) {sum(w)-1},
                 eval_jac_g_eq = function(w, ret) {rep(1, ncol(ret))},
                 opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_rel' = 1.0e-7, 'maxeval' = 1000),
                 ret = as.matrix(ret))$solution
}

