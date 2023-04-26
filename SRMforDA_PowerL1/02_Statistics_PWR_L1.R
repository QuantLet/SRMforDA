rm(list=ls())
library(qs)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(xtable)
source('R/turnover.R')
### Color pallet
colpal <- c("#000000", "#FF0000", "#0000FF", "#00FF00", "#FFA500", "#FF00FF", "#784421")

### Read Crypto returns
ret <- qread("data/coin_return.qs")
ret <- na.omit(ret)
colnames(ret) <- c("Date", "BTC", "LTC", "XRP", "DOGE", "DASH", "ETH", "ETC", "ZEC", "BCH", "BSV")

### Portfolio parameters
t_window <- 365
a_window <- 30 #Options 14,30,90

### Read weights
w <- qread(paste0("data/Weights/weights_", a_window, "d_PWR_l.qs"))

### LOOP for calculating portfolio return
n_per <- floor((nrow(ret)-t_window)/a_window)
for (i in 1:n_per){
  t_start = t_window + 1 + (i-1)*a_window
  t_end = t_start + a_window - 1
  
  if (!exists("p_ret")){
    p_ret <- data.frame("Date" = ret[t_start:t_end, 1],
                        "PWR_01" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_01",])),
                        "PWR_03" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_03",])),
                        "PWR_05" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_05",])),
                        "PWR_07" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_07",])),
                        "PWR_09" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_09",]))
                        )
   }else{
    p_ret <- rbind(p_ret, 
                   data.frame("Date" = ret[t_start:t_end, 1],
                              "PWR_01" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_01",])),
                              "PWR_03" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_03",])),
                              "PWR_05" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_05",])),
                              "PWR_07" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_07",])),
                              "PWR_09" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["SRM_PWR_09",]))
                   ))
  }
  
}
colnames(p_ret) <- c("Date", "PWR_01", "PWR_03", "PWR_05", "PWR_07", "PWR_09")

### Cumulative wealth for graph
CW_p <- data.frame("Date" = p_ret$Date,
                   "PWR_01"  = cumprod(1+p_ret$PWR_01),
                   "PWR_03"  = cumprod(1+p_ret$PWR_03),
                   "PWR_05"  = cumprod(1+p_ret$PWR_05),
                   "PWR_07"  = cumprod(1+p_ret$PWR_07),
                   "PWR_09"  = cumprod(1+p_ret$PWR_09)
                   )

### Graph
PWR <- ggplot(data = reshape2::melt(CW_p, id.vars = "Date"),
       aes(x = Date, y = value, group = variable)) +
  geom_line(aes(colour=variable), linewidth = 0.5) +
  theme_minimal()+theme_classic()+ theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x ="Time", y = "Cumulative Wealth", color = "\u03b3")+
  scale_color_manual(values = colpal)
ggsave(paste0("Output/PWR_L1_CR_", a_window, "d.jpg"), PWR, width = 6000, height = 3600, 
       dpi = 800, units = "px", limitsize = F)
ggsave(paste0("Output/PWR_L1_CR_", a_window, "d.png"), PWR, width = 6000, height = 3600, 
              dpi = 800, units = "px", limitsize = F)

### Statistics
rownames(p_ret) <- p_ret[,1]
stat <- data.frame("TO" = getTO(ret, w, a_window, t_window),
                   "TTO" = getTTO(w, a_window),
                   "CW" = apply(p_ret[,-1], 2, function (x) Return.cumulative(x)+1),
                   "SD" = apply(p_ret[,-1], 2, function (x) sd(x)),
                   "SR" = apply(p_ret[,-1], 2, function (x) mean(x)/sd(x)),
                   "CR" = apply(p_ret[,-1], 2, function (x) 365*mean(x)/maxDrawdown(x)))
write.csv(stat, paste0("Output/PWR_L1_STAT_", a_window, "d.csv"))

### Write LaTeX file
print(x = xtable(stat, digits = 4), file = paste0("Output/PWR_L1_STAT_", a_window, "d.tex"),
      include.rownames = T, booktabs = T, floating = F)

### Choose and save portfolio 
if (a_window==30){
  qsave(CW_p[, c("Date", "PWR_01")], "Output/Final_data/PWR_l.qs")
  qsave(stat["PWR_01",], "Output/Final_data/PWR_l_stat.qs")
}

