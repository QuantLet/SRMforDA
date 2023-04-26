rm(list=ls())
library(qs)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(xtable)
source('R/turnover.R')
### Color pallet
colpal <- c("#000000", "#FF0000", "#00FF00", "#0000FF", "#FFA500", 
            "#FF00FF", "#784421", "gray")

### Read Crypto returns
ret <- qread("data/coin_return.qs")
ret <- na.omit(ret)
colnames(ret) <- c("Date", "BTC", "LTC", "XRP", "DOGE", "DASH", "ETH", "ETC", "ZEC", "BCH", "BSV")

### Portfolio parameters
t_window <- 365  # Training window
a_window <- 30   # Application window

### Read weights
w <- qread(paste0("data/Weights/weights_", a_window, "d.qs"))

### LOOP for calculating portfolio return
n_per <- floor((nrow(ret)-t_window)/a_window)
for (i in 1:n_per){
  t_start = t_window + 1 + (i-1)*a_window
  t_end = t_start + a_window - 1

  if (!exists("p_ret")){
    p_ret <- data.frame("Date" = ret[t_start:t_end, 1],
                        "Naive" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["Naive",])),
                        "MinVar" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["MinVar",]))
                        )
   }else{
    p_ret <- rbind(p_ret, 
                   data.frame("Date" = ret[t_start:t_end, 1],
                              "Naive" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["Naive",])),
                              "MinVar" = (as.matrix(ret[t_start:t_end, -1]) %*% t(w[[i]]["MinVar",]))
                   ))
  }
  
}
colnames(p_ret) <- c("Date", "Naive", "MinVar")

### Portfolio cumulative wealth for graph
CW_p <- data.frame("Date" = p_ret$Date,
                   "Naive" = cumprod(1+p_ret$Naive),
                   "MinVar" = cumprod(1+p_ret$MinVar)
                   )

CW_p <- data.frame(cbind(CW_p, qread("data/Final_data/VaRES.qs")[,-1],
                    qread("data/Final_data/EXP.qs")[,-1],
                    qread("data/Final_data/PWR_l.qs")[,-1],
                    qread("data/Final_data/PWR_h.qs")[,-1]))
CW_p$Date <- ymd(CW_p$Date) 
colnames(CW_p) <- c("Date", "Naive", "MinVar", "VaR_05", "ES_05", "EXP_25", "PWR_01", "PWR_15")

### Add CRIX
CRIX <- read.csv2("data/crix.csv", sep = ";", dec = ",")
CRIX$date <- dmy(CRIX$date)
colnames(CRIX) <- c("Date", "CRIX")

CW_p <- inner_join(CW_p, CRIX, by = "Date")
CW_p$CRIX <- CW_p$CRIX/CW_p$CRIX[1] 

### Graph
Final <- ggplot(data = reshape2::melt(CW_p, id.vars = "Date"),
              aes(x = Date, y = value, group = variable)) +
  geom_line(aes(colour=variable), linewidth = 0.5) +
  theme_minimal()+theme_classic()+ theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x ="Time", y = "Cumulative Wealth", color = "")+
  scale_color_manual(values = colpal)
Final
ggsave(paste0("Output/Final_CR_", a_window, "d.jpg"), Final, width = 6000, height = 3600, 
       dpi = 800, units = "px", limitsize = F)
ggsave(paste0("Output/Final_CR_", a_window, "d.png"), Final, width = 6000, height = 3600, 
       dpi = 800, units = "px", limitsize = F)

### Statistics 
# Calculate statistics for Minimum Variance and Naive portfolio
rownames(p_ret) <- p_ret[,1]
stat <- data.frame("TO" = getTO(ret, w, a_window, t_window),
                   "TTO" = getTTO(w, a_window),
                   "CW" = apply(p_ret[,-1], 2, function (x) Return.cumulative(x)+1),
                   "SD" = apply(p_ret[,-1], 2, function (x) sd(x)),
                   "SR" = apply(p_ret[,-1], 2, function (x) mean(x)/sd(x)),
                   "CR" = apply(p_ret[,-1], 2, function (x) 365*mean(x)/maxDrawdown(x)))

# Calculate statistics for CRIX
r_CRIX <- CRIX %>% filter(Date >= ymd("2019-11-11") & Date <=ymd("2023-02-21"))
rownames(r_CRIX) <- r_CRIX[,1]
r_CRIX <- cbind(r_CRIX, c(NA, r_CRIX[2:(nrow(r_CRIX)),2]/r_CRIX[1:(nrow(r_CRIX)-1),2]-1))
r_CRIX <- r_CRIX[-1,-2]
MD_CRIX <- maxDrawdown(r_CRIX[,2])
stat_CRIX <- data.frame("TO" = 0,
                        "TTO" = 0,
                        "CW" = (Return.cumulative(r_CRIX[,2])+1),
                        "SD" = sd(r_CRIX[,2]),
                        "SR" = mean(r_CRIX[,2])/sd(r_CRIX[,2]),
                        "CR" = 365*mean(r_CRIX[,2])/MD_CRIX)

# Join to one table
stat <- rbind(stat, "CRIX" = stat_CRIX,
              qread("data/Final_data/VaRES_stat.qs"),
              qread("data/Final_data/EXP_stat.qs"),
              qread("data/Final_data/PWR_l_stat.qs"),
              qread("data/Final_data/PWR_h_stat.qs"))

write.csv(stat, paste0("Output/Final_STAT_", a_window, "d.csv"))

print(x = xtable(stat, digits = 4), file = paste0("Output/Final_STAT_", a_window, "d.tex"),
      include.rownames = T, booktabs = T, floating = F)
