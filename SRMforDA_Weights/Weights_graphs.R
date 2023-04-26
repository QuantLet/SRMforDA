rm(list=ls())
###Libraries
library(qs)
library(data.table)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
###Color palette
mycol <- c("white", "#5D6795", "#E41A1C", "#658E67", "#A35390", 
           "#F37912", "#000000", "#D7B32E", "#B85F49", "#F781BF")

###Choose rebalancing period
a_window <- 30 #Options 14, 30, 90

###Make graphs
for (p_type in c("VaRES", "EXP", "PWR_l", "PWR_h", "Benchmark")){
  # Read data
  w <- qread(paste0("data/weights_", a_window, "d_", p_type, ".qs"))
  
  # Transform data
  weight_list <- list()
  for (i in 1:length(w)){
    for (j in 1:nrow(w[[i]])){
      if (i == 1){
        weight_list[[j]] <- w[[i]][j, ]
      }else{
        weight_list[[j]] <- rbind(weight_list[[j]], w[[i]][j, ])
      }
    }
  }
  
  # Make plots
  for (i in 1:length(rownames(w[[1]]))){
    # Create X-axis
    Date <- seq(from = ymd("2019-11-10"), to=(ymd("2019-11-10")+a_window*(nrow(weight_list[[i]])-1)), by=a_window)
    colnames(weight_list[[i]]) <- c("BTC", "LTC", "XRP", "DOGE", "DASH", "ETH", "ETC", "ZEC", "BCH", "BSV")
    gg <- ggplot(data = reshape2::melt(cbind(Date, weight_list[[i]]), id.vars = "Date"),
                 aes(fill=variable, y=value, x=Date)) +
      geom_bar(position="fill", stat="identity") +
      theme_minimal()+ theme(legend.position = "none")+
      scale_fill_manual( values = mycol)+
      labs(title ="", x = "Time", y = "Weights", fill = "Assets")+
      ylim(0,1)
    gg
    ggsave(paste0("Output/",rownames(w[[1]])[i], "_", a_window, "d.jpg"), gg, width = 6000, height = 3600,
           dpi = 800, units = "px", limitsize = F, bg="white")
    ggsave(paste0("Output/",rownames(w[[1]])[i], "_", a_window, "d.png"), gg, width = 6000, height = 3600,
           dpi = 800, units = "px", limitsize = F, bg="white")
  }
}

