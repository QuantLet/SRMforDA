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
w <- qread(paste0("data/Weights/weights_", a_window, "d_VaRES.qs"))

### LOOP for calculating portfolio return
n_per <- floor((nrow(ret)-t_window)/a_window)
for (i in 1:n_per){
  t_start = t_window + 1 + (i-1)*a_window
  t_end = t_start + a_window - 1
  
  if (!exists("p_ret")){
    p_ret <- data.frame("Date" = ret[t_start:t_end, 1],
                        "VaR_10" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["VaR_10",])),
                        "VaR_05" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["VaR_05",])),
                        "VaR_01" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["VaR_01",])),
                        "ES_10" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["ES_10",])),
                        "ES_05" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["ES_05",])), 
                        "ES_01" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["ES_01",]))
                        )
   }else{
    p_ret <- rbind(p_ret, 
                   data.frame("Date" = ret[t_start:t_end, 1],
                              "VaR_10" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["VaR_10",])),
                              "VaR_05" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["VaR_05",])),
                              "VaR_01" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["VaR_01",])),
                              "ES_10" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["ES_10",])),
                              "ES_05" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["ES_05",])), 
                              "ES_01" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["ES_01",]))
                   ))
  }
  
}
colnames(p_ret) <- c("Date", "VaR_10", "VaR_05", "VaR_01", "ES_10", "ES_05", "ES_01")

### Cumulative wealth for graph
CW_p <- data.frame("Date" = p_ret$Date,
                   "VaR_10" = cumprod(1+p_ret$VaR_10),
                   "VaR_05" = cumprod(1+p_ret$VaR_05),
                   "VaR_01" = cumprod(1+p_ret$VaR_01),
                   "ES_10"  = cumprod(1+p_ret$ES_10),
                   "ES_05"  = cumprod(1+p_ret$ES_05),
                   "ES_01"  = cumprod(1+p_ret$ES_01)
                   )

### Graph
VaR <- ggplot(data = reshape2::melt(CW_p %>% select("Date", "VaR_10", "VaR_05", "VaR_01"), id.vars = "Date"),
                aes(x = Date, y = value, group = variable)) +
  geom_line(aes(colour=variable), linewidth = 0.5) +
  theme_minimal()+theme_classic()+ theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x ="Time", y = "Cumulative Wealth", color = "")+
  scale_color_manual( values = colpal)
ggsave(paste0("Output/VaR_CR_", a_window, "d.jpg"), VaR, width = 6000, height = 3600, 
       dpi = 800, units = "px", limitsize = F)
ggsave(paste0("Output/VaR_CR_", a_window, "d.png"), VaR, width = 6000, height = 3600, 
       dpi = 800, units = "px", limitsize = F)


ES <- ggplot(data = reshape2::melt(CW_p %>% select("Date", "ES_10", "ES_05", "ES_01"), id.vars = "Date"),
              aes(x = Date, y = value, group = variable)) +
  geom_line(aes(colour=variable), linewidth = 0.5) +
  theme_minimal()+theme_classic()+ theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x ="Time", y = "Cumulative Wealth", color = "")+
  scale_color_manual( values = colpal)
ggsave(paste0("Output/ES_CR_", a_window, "d.jpg"), ES, width = 6000, height = 3600, 
       dpi = 800, units = "px", limitsize = F)
ggsave(paste0("Output/ES_CR_", a_window, "d.png"), ES, width = 6000, height = 3600, 
       dpi = 800, units = "px", limitsize = F)

### Statistics
rownames(p_ret) <- p_ret[,1]
stat <- data.frame("TO" = getTO(ret, w, a_window, t_window),
                   "TTO" = getTTO(w, a_window),
                   "CW" = apply(p_ret[,-1], 2, function (x) Return.cumulative(x)+1),
                   "SD" = apply(p_ret[,-1], 2, function (x) sd(x)),
                   "SR" = apply(p_ret[,-1], 2, function (x) mean(x)/sd(x)),
                   "CR" = apply(p_ret[,-1], 2, function (x) 365*mean(x)/maxDrawdown(x)))
write.csv(stat, paste0("Output/VaRES_STAT_", a_window, "d.csv"))

### Write LaTeX file
print(x = xtable(stat, digits = 4), file = paste0("Output/VaRES_STAT_", a_window, "d.tex"),
      include.rownames = T, booktabs = T, floating = F)

### Choose and save portfolio for final comparison 
if (a_window==30){
  qsave(CW_p[, c("Date", "VaR_05", "ES_05")], "Output/Final_data/VaRES.qs")
  qsave(stat[c("VaR_05", "ES_05"),], "Output/Final_data/VaRES_stat.qs")
}
