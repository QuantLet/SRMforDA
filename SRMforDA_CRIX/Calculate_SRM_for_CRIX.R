rm(list=ls())
### Libraries
library(tidyverse)
library(latex2exp)
library(grid)
library(gridExtra)
library(lubridate)
library(qs)
library(PerformanceAnalytics)
source('R/SRM.R')

### Color palette
colpal <- c("#000000", "#FF0000", "#0000FF", "#00FF00", "#FFA500", "#FF00FF")

################################################################################
############################# Read data ########################################
################################################################################
data <- read.csv2("Data/crix.csv", sep = ";", dec = ",")
data$date <- dmy(data$date)
colnames(data) <- c("Date", "CRIX")

### Plot price
(CRIX_price <- ggplot(data=data, aes(x=Date, y=CRIX))+
  geom_line()+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  xlab("Date") + ylab("CRIX Value"))
ggsave("Output/CRIX_price.jpg", CRIX_price, width = 6000, height = 3600, 
       dpi = 500, units = "px", limitsize = F)

### Calculate and plot return
CRIX_ret <- data.frame(Date = data$Date[-1],
                     ret = diff(log(data$CRIX)))

(CRIX_return <- ggplot(CRIX_ret, aes(x=Date, y=ret))+
  geom_line()+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  xlab("Date") + ylab("Return"))
ggsave("Output/CRIX_return.jpg", CRIX_return, width = 6000, height = 3600, 
       dpi = 500, units = "px", limitsize = F)

################################################################################
############################# Diagnostic #######################################
################################################################################
### Normal distribution
ggplot(CRIX_ret, aes(ret)) + 
  geom_histogram(aes(y = after_stat(!!str2lang("density"))), bins=200)+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  stat_function(fun=dnorm, args=list(mean=mean(CRIX_ret$ret), sd=sd(CRIX_ret$ret)), 
                col = "red")+ 
  xlab("") + ylab("Density")

### QQ plot
ggplot(CRIX_ret, aes(sample=ret))+
  stat_qq()+
  stat_qq_line(col = "red")+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles")

################################################################################
######################## Calculate SRM for CRIX ################################
################################################################################
if (file.exists("Data/crix_SRM.qs")){
  out <- qread("Data/crix_SRM.qs")
}else{
  for (i in 1:(nrow(CRIX_ret)-364)){
    print(paste0(i, "/", (nrow(CRIX_ret)-364)))
    if (i == 1){
      out <- data.frame(Date = CRIX_ret$Date[365:length(CRIX_ret$Date)],
                        # VaR
                        VaR_10 = rep(NA, nrow(CRIX_ret)-364),
                        VaR_05 = rep(NA, nrow(CRIX_ret)-364),
                        VaR_01 = rep(NA, nrow(CRIX_ret)-364),
                        # ES
                        ES_10 = rep(NA, nrow(CRIX_ret)-364),
                        ES_05 = rep(NA, nrow(CRIX_ret)-364),
                        ES_01 = rep(NA, nrow(CRIX_ret)-364),
                        # EXP
                        EXP_1 = rep(NA, nrow(CRIX_ret)-364),
                        EXP_5 = rep(NA, nrow(CRIX_ret)-364),
                        EXP_10 = rep(NA, nrow(CRIX_ret)-364),
                        EXP_15 = rep(NA, nrow(CRIX_ret)-364),
                        EXP_20 = rep(NA, nrow(CRIX_ret)-364),
                        EXP_25 = rep(NA, nrow(CRIX_ret)-364),
                        # PWR less 1
                        GAM_01 = rep(NA, nrow(CRIX_ret)-364),
                        GAM_03 = rep(NA, nrow(CRIX_ret)-364),
                        GAM_05 = rep(NA, nrow(CRIX_ret)-364),
                        GAM_07 = rep(NA, nrow(CRIX_ret)-364),
                        GAM_09 = rep(NA, nrow(CRIX_ret)-364),
                        # PWR more 1
                        GAM_1_5 = rep(NA, nrow(CRIX_ret)-364),
                        GAM_3 = rep(NA, nrow(CRIX_ret)-364),
                        GAM_5 = rep(NA, nrow(CRIX_ret)-364),
                        GAM_10 = rep(NA, nrow(CRIX_ret)-364),
                        GAM_15 = rep(NA, nrow(CRIX_ret)-364),
                        #MaxDrawdown
                        MD = rep(NA, nrow(CRIX_ret)-364))
    }
    
    ### VaR
    out$VaR_10[i] <- -quantile(CRIX_ret$ret[(i:(364+i))], 0.1)
    out$VaR_05[i] <- -quantile(CRIX_ret$ret[(i:(364+i))], 0.05)
    out$VaR_01[i] <- -quantile(CRIX_ret$ret[(i:(364+i))], 0.01)
    
    ### ES
    out$ES_10[i] <- getSRM_ES(CRIX_ret$ret[(i:(364+i))], 0.1)
    out$ES_05[i] <- getSRM_ES(CRIX_ret$ret[(i:(364+i))], 0.05)
    out$ES_01[i] <- getSRM_ES(CRIX_ret$ret[(i:(364+i))], 0.01)
    
    
    ### Exponential SRM
    out$EXP_1[i]  <- getSRM_EXP(CRIX_ret$ret[(i:(364+i))], 1)
    out$EXP_5[i]  <- getSRM_EXP(CRIX_ret$ret[(i:(364+i))], 5)
    out$EXP_10[i] <- getSRM_EXP(CRIX_ret$ret[(i:(364+i))], 10)
    out$EXP_15[i] <- getSRM_EXP(CRIX_ret$ret[(i:(364+i))], 15)
    out$EXP_20[i] <- getSRM_EXP(CRIX_ret$ret[(i:(364+i))], 20)
    out$EXP_25[i] <- getSRM_EXP(CRIX_ret$ret[(i:(364+i))], 25)
    
    ### Power spectral risk measures 0 < gamma < 1
    out$GAM_01[i] <- getSRM_PWR_l(CRIX_ret$ret[(i:(364+i))], 0.1)
    out$GAM_03[i] <- getSRM_PWR_l(CRIX_ret$ret[(i:(364+i))], 0.3)
    out$GAM_05[i] <- getSRM_PWR_l(CRIX_ret$ret[(i:(364+i))], 0.5)
    out$GAM_07[i] <- getSRM_PWR_l(CRIX_ret$ret[(i:(364+i))], 0.7)
    out$GAM_09[i] <- getSRM_PWR_l(CRIX_ret$ret[(i:(364+i))], 0.9)
    
    ### Power spectral risk measures gamma > 1
    out$GAM_1_5[i] <- getSRM_PWR_h(CRIX_ret$ret[(i:(364+i))], 1.5)
    out$GAM_3[i]   <- getSRM_PWR_h(CRIX_ret$ret[(i:(364+i))], 3)
    out$GAM_5[i]   <- getSRM_PWR_h(CRIX_ret$ret[(i:(364+i))], 5)
    out$GAM_10[i]  <- getSRM_PWR_h(CRIX_ret$ret[(i:(364+i))], 10)
    out$GAM_15[i]  <- getSRM_PWR_h(CRIX_ret$ret[(i:(364+i))], 15)
    
    ### Maximum drawdown
    MD_dat <- CRIX_ret[(i:(364+i)),]; rownames(MD_dat) <- MD_dat$Date; MD_dat <- MD_dat[,-1]
    out$MD[i] <- maxDrawdown(MD_dat)
  }
  qsave(out, "Data/crix_SRM.qs")
}

################################################################################
################################# Graph ########################################
################################################################################
### VaR
CRIX_VaR <- ggplot(reshape2::melt(out, "Date") %>% 
                     filter(variable %in% c("VaR_10", "VaR_05", "VaR_01")), aes(x=Date, y=value))+
  geom_line(aes(color=variable))+
  theme_minimal()+theme_bw()+ theme(legend.position = "none", panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), axis.title.x=element_blank(),
                                    axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  labs(x ="Date", y = "Value-at-Risk", color = "SRM")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  scale_color_manual(values = colpal)
CRIX_VaR

### ES
CRIX_ES <- ggplot(reshape2::melt(out, "Date") %>% 
                     filter(variable %in% c("ES_10", "ES_05", "ES_01")), aes(x=Date, y=value))+
  geom_line(aes(color=variable))+
  theme_minimal()+theme_bw()+ theme(legend.position = "none", panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), axis.title.x=element_blank(),
                                    axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  labs(x ="Date", y = "Expected Shortfall", color = "SRM")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  scale_color_manual(values = colpal)
CRIX_ES

### Exponential SRM
CRIX_EXP <- ggplot(reshape2::melt(out, "Date") %>% 
                     filter(variable %in% c("EXP_1", "EXP_5", "EXP_10", "EXP_15", "EXP_20", "EXP_25")),
                   aes(x=Date, y=value))+
  geom_line(aes(color=variable))+
  theme_minimal()+theme_bw()+ theme(legend.position = "none", panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), axis.title.x=element_blank(),
                                    axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  labs(x ="Date", y = "Exponential SRM", color = "SRM")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  scale_color_manual(values = colpal)
CRIX_EXP

### Power SRM for 0 < gamma < 1
CRIX_PWR_l <- ggplot(reshape2::melt(out, "Date") %>% 
                     filter(variable %in% c("GAM_01", "GAM_03", "GAM_05", "GAM_07", "GAM_09")),
                   aes(x=Date, y=value))+
  geom_line(aes(color=variable))+
  theme_minimal()+theme_bw()+ theme(legend.position = "none", panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), axis.title.x=element_blank(),
                                    axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  labs(x ="Date", y = expression("Power SRM for 0 < "*gamma*" < 1"), color = "SRM")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  scale_color_manual(values = colpal)
CRIX_PWR_l

### Power SRM for gamma > 1
CRIX_PWR_h <- ggplot(reshape2::melt(out, "Date") %>% 
                       filter(variable %in% c("GAM_1_5", "GAM_3", "GAM_5", "GAM_10", "GAM_15")),
                     aes(x=Date, y=value))+
  geom_line(aes(color=variable))+
  theme_minimal()+theme_bw()+ theme(legend.position = "none", panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), axis.title.x=element_blank(),
                                    axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  labs(x ="Date", y = expression("Power SRM for "*gamma*" > 1"), color = "SRM")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  scale_color_manual(values = colpal)
CRIX_PWR_h

### MaxDrawdown
CRIX_MD <- ggplot(reshape2::melt(out, "Date") %>% filter(variable %in% c("MD")), 
                  aes(x=Date, y=-value))+
  geom_line(aes(color=variable))+
  theme_minimal()+theme_bw()+ theme(legend.position = "none", panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), axis.title.x=element_blank(),
                                    axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  labs(x ="Date", y = "MaxDrawdown", color = "SRM")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  scale_color_manual(values = "black")
CRIX_MD

### CRIX price
CRIX_price <- ggplot(data=data %>% filter(Date >= as.Date("2019-08-09"), Date <= as.Date("2023-03-13")), aes(x=Date, y=CRIX))+
  geom_line()+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y")+
  xlab("Date") + ylab("CRIX Value")

CRIX_SRM <- grid.arrange(CRIX_VaR, CRIX_ES, CRIX_EXP, CRIX_PWR_l, CRIX_PWR_h, CRIX_MD, CRIX_price, nrow=7)
ggsave("Output/CRIX_SRM.jpg", CRIX_SRM, width = 7000, height = 12000, 
       dpi = 1000, units = "px", limitsize = F)

