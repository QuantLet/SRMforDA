rm(list=ls())
### Libraries
library(tidyverse)
library(reshape2)
### Color palette
colpal <- c("#000000", "#FF0000", "#0000FF", "#00FF00", "#FFA500", "#FF00FF")
################################################################################
############################## Risk Spectrum ###################################
################################################################################

### ES
x <- seq(0,1, 1/10000)
ES <- data.frame(p = x,
                 ES_10 = c(rep(1/0.1, 1001), rep(NA, 9000) ),
                 ES_05 = c(rep(1/0.05, 501), rep(NA, 9500) ),
                 ES_01 = c(rep(1/0.01, 101), rep(NA, 9900) ))

ES <- melt(ES, id.vars = "p")
RS_ES <- ggplot(ES, aes(x = p, y=value, color= variable))+
  geom_line(linewidth = 2)+
  theme_minimal()+theme_classic()+ theme(legend.position = "none", panel.grid.major = element_blank(), 
                                         panel.grid.minor = element_blank())+
  labs(x ="", y = "Risk Spectrum", color = "ES")+
  scale_color_manual(values = colpal)+
  scale_x_continuous(n.breaks = 10, limits = c(0,1), expand = c(0, 0))+
  scale_y_continuous(n.breaks = 10, limits = c(0,110), expand = c(0, 0))
RS_ES
ggsave("Output/RS_ES.jpg", RS_ES, width = 6000, height = 3600, 
       dpi = 1300, units = "px", limitsize = F)

### Exponential SRM
EXP <- data.frame(p = x,
                 EXP_1  =    1*exp(-1*x)/(1-exp(-1)),
                 EXP_5  =    5*exp(-5*x)/(1-exp(-5)),
                 EXP_10  =  10*exp(-10*x)/(1-exp(-10)),
                 EXP_15  =  15*exp(-15*x)/(1-exp(-15)),
                 EXP_20  =  20*exp(-20*x)/(1-exp(-20)),
                 EXP_25 =   25*exp(-25*x)/(1-exp(-25)))

EXP <- melt(EXP, id.vars = "p")
RS_EXP <- ggplot(EXP, aes(x = p, y=value, color= variable))+
  geom_line(linewidth = 2)+
  theme_minimal()+theme_classic()+ theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x ="", y = "Risk Spectrum", color = "k")+
  scale_color_manual(values = colpal)+
  scale_x_continuous(n.breaks = 10, limits = c(0,1), expand = c(0, 0))+
  scale_y_continuous(n.breaks = 5, limits = c(0,25), expand = c(0, 0))
RS_EXP
ggsave("Output/RS_EXP.jpg", RS_EXP, width = 6000, height = 3600, 
       dpi = 1200, units = "px", limitsize = F)


### Power SRM gamma < 1
PWR_l <- data.frame(p = x,
                  # PWR less 1
                  GAM_01 = 0.1*x^(0.1-1),
                  GAM_03 = 0.3*x^(0.3-1),
                  GAM_05 = 0.5*x^(0.5-1),
                  GAM_07 = 0.7*x^(0.7-1),
                  GAM_09 = 0.9*x^(0.9-1)
                  )

PWR_l <- melt(PWR_l, id.vars = "p")
RS_PWR_l <- ggplot(PWR_l, aes(x = p, y=value, color= variable))+
  geom_line(linewidth = 2)+
  theme_minimal()+theme_classic()+ theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x ="", y = "", color = "\u03b3")+
  scale_color_manual(values = colpal)+
  scale_x_continuous(n.breaks = 10, limits = c(0,1), expand = c(0, 0))+
  scale_y_continuous(n.breaks = 10, limits = c(0,10), expand = c(0, 0))
RS_PWR_l
ggsave("Output/RS_PWR_l.jpg", RS_PWR_l, width = 6000, height = 3600, 
       dpi = 1200, units = "px", limitsize = F)

### Power SRM
PWR_h <- data.frame(p = x,
                  # PWR more 1
                  GAM_1_5 = 1.5*(1-x)^(1.5-1),
                  GAM_3   = 3.0*(1-x)^(3.0-1),
                  GAM_5   = 5.0*(1-x)^(5.0-1),
                  GAM_10  = 10*(1-x)^(10-1),
                  GAM_15  = 15*(1-x)^(15-1)
)

PWR_h <- melt(PWR_h, id.vars = "p")
RS_PWR_h <- ggplot(PWR_h, aes(x = p, y=value, color= variable))+
  geom_line(linewidth = 2)+
  theme_minimal()+theme_classic()+ theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x ="", y = "", color = "\u03b3")+
  scale_color_manual(values = colpal)+
  scale_x_continuous(n.breaks = 10, limits = c(0,1), expand = c(0, 0))+
  scale_y_continuous(n.breaks = 15, limits = c(0,15), expand = c(0, 0))
RS_PWR_h
ggsave("Output/RS_PWR_h.jpg", RS_PWR_h, width = 6000, height = 3600, 
       dpi = 1200, units = "px", limitsize = F)
