################################################################################
########################### Functions ##########################################
################################################################################

################################ TO ############################################
getTO = function(ret,w, a_window, t_window) {
  out_TR <- c()
  ### inputs w, a_window, t_window, rets
  for (n_row in 1:nrow(w[[1]])){
    for (i in 1:(length(w)-1)){
      for (j in 1:ncol(w[[1]])){
        t_start = t_window + 1 + (i-1)*a_window
        t_end = t_start + a_window - 1
        CR <- PerformanceAnalytics::Return.cumulative(as.data.frame(ret)[(t_start:t_end),(j+1)])
        if (j==1){
          TR <- w[[i]][n_row,j]*(1+CR)
        }else{
          TR <- c(TR, w[[i]][n_row,j]*(1+CR))
        }
      }
      if (i==1){
        TO <- sum(abs(TR/sum(TR) - w[[i+1]][n_row,]))
      }else{
        TO <- TO + sum(abs(TR/sum(TR) - w[[i+1]][n_row,]))
      }
    }
    out_TR <- c(out_TR, (a_window/30)^(-1)*TO)
  }
  
  return(out_TR)
}


################################ TTO ###########################################
getTTO = function(w, a_window) {
  out_TTR <- c()
  for (n_row in 1:nrow(w[[i]])){
    for (i in 1:(length(w)-1)){
      for (j in 1:ncol(w[[1]]))
        if (j==1 & i==1){
          TR <- abs(w[[i]][n_row,j] - w[[i+1]][n_row,j])
        }else{
          TR <- TR + abs(w[[i]][n_row,j] - w[[i+1]][n_row,j])
        }
    }
    out_TTR <- c(out_TTR, (a_window/30)^(-1)*TR)
  }
  return(out_TTR)
}