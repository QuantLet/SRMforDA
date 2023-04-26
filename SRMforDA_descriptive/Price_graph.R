rm(list=ls())
### Libraries
library(qs)
library(lubridate)
library(tidyverse)
library(cowplot)

################################################################################
############################### Price Graphs ###################################
################################################################################
Data <- qread("Data/coinPrice.qs")

PG <- plot_grid(ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=BTC))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+ 
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "BTC", vjust=2, hjust=0, fontface = "bold", size=10), 
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=LTC))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "LTC", vjust=2, hjust=0, fontface = "bold", size=10),
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=XRP))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "XRP", vjust=2, hjust=0, fontface = "bold", size=10),
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=DOGE))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+ 
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "DOGE", vjust=2, hjust=0, fontface = "bold", size=10), 
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=DASH))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "DASH", vjust=2, hjust=0, fontface = "bold", size=10),
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=ETH))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "ETH", vjust=2, hjust=0, fontface = "bold", size=10),
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=ETC))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+ 
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "ETC", vjust=2, hjust=0, fontface = "bold", size=10), 
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=ZEC))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "ZEC", vjust=2, hjust=0, fontface = "bold", size=10),
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=BCH))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "BCH", vjust=2, hjust=0, fontface = "bold", size=10),
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=BSV))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "BSV", vjust=2, hjust=0, fontface = "bold", size=10),
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=CRIX))+ geom_line()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "CRIX", vjust=2, hjust=0, fontface = "bold", size=10),
                ggplot(data=na.omit(Data %>%filter(Date >=ymd("2018-11-09"))), aes(x=Date, y=CRIX))+geom_blank()+
                  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                    panel.border = element_blank(), axis.line.x = element_line(linewidth = 0.2, linetype = "solid", colour = "black"))+
                  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
                  xlab("") + ylab("")+
                  scale_color_manual(values = "white")+
                  annotate("text",  x=ymd("2018-11-09"), y = Inf, label = "", vjust=2, hjust=0, fontface = "bold", size=10),
                align = "v", ncol=3)
PG
ggsave("Output/Crypto_price.jpg", PG, width = 6000, height = 3600, 
       dpi = 500, units = "px", limitsize = F)
