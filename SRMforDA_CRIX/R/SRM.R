########################## Expected Shortfall ##################################
### Calculate Expected Shortfall
# ret is return of assets which are involved in portfolio
# q is quantile

getSRM_ES = function(ret, q) {
  fun_ES <- function(p, data) (1/q)*quantile(-data, p)
  SRM <- integrate(f=fun_ES, lower=(1-q), upper=1, data=ret, subdivisions = 10^5, stop.on.error = F)$value
  return(SRM)
}

################### Exponentional Spectral Risk Measure ########################
### Calculate Exponentional Spectral Risk Measure
# ret is return of assets which are involved in portfolio
# k is Exponentional Spectral Risk Measure parameter
getSRM_EXP = function(ret, k) {
  fun_EXP <- function(p, data) (k/(1-exp(-k)))*exp(-k*(1-p))*quantile(-data,p)
  SRM <- integrate(f=fun_EXP, lower=0, upper=1, data=ret, subdivisions = 10^5, stop.on.error = F)$value
  return(SRM)
}

####################### Power Spectral Risk Measure ############################
### Calculate Power Spectral Risk Measure for 0 < gamma < 1
# ret is return of assets which are involved in portfolio
# gamma is Power Spectral Risk Measure parameter

getSRM_PWR_l = function(ret, gamma) {
  fun_PWR_l <- function(p, data) gamma*(1-p)^(gamma-1)*quantile(-data,p)
  SRM <- integrate(f=fun_PWR_l, lower=0, upper=1, data=ret, subdivisions = 10^5, stop.on.error = F)$value
  return(SRM)
}

### Calculate Power Spectral Risk Measure for gamma > 1
# ret is return of assets which are involved in portfolio
# gamma is Power Spectral Risk Measure parameter

getSRM_PWR_h = function(ret, gamma) {
  fun_PWR_h <- function(p, data) gamma*p^(gamma-1)*quantile(-data,p)
  SRM <- integrate(f=fun_PWR_h, lower=0, upper=1, data=ret, subdivisions = 10^5, stop.on.error = F)$value
  return(SRM)
}
  
  