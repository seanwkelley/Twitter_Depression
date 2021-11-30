#--------------------------------------------------
#Figure S1. Split half reliability of primary 9-node network. 
#S1B: Histogram of global network strength from personalised networks of all participants (n = 946). 
#S1C: Variability of node strength centrality in the 9-node network (n = 946).  

#--------------------------------------------------

#Load necessary packages 
library(lmerTest)
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(patchwork)
library(qgraph)
library(bootnet)
library(DescTools)
library(extrafont)
loadfonts(device = "win")


setwd('/Users/seankelley/Twitter_Depression_Kelley/')

#remove outliers 
remove_outliers <- function(x, na.rm = TRUE, ...) {
  s <- sd(x)
  m <- mean(x)
  y <- x
  y[x > (m + 3*s)] <- NA
  y[x < (m - 3*s)] <- NA
  y
}
# function to apply to all rows
remove_all_outliers <- function(d){
  d[] <- lapply(d, function(x) if (is.numeric(x))
    remove_outliers(x) else x)
  d
}

raincloud_theme2 = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  plot.tag = element_text(size = 18,face = "bold"),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


episode_depression = theme(
  panel.background = element_blank(),
  legend.title =  element_blank(),
  legend.text = element_text(size=46),
  legend.key=element_blank(),
  legend.justification = "right",
  plot.margin = unit(c(1,1,1,1), "cm"),
  text = element_text(size = 46, family = "Arial"),
  axis.title.x = element_text(size = 46,vjust=-2),
  axis.title.y = element_text(size = 46),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  plot.tag = element_text(size = 18,face = "bold"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



cbpalette <- c("#DD8D29","#E2D200","#46ACC8","#0B775E","#C51B7D","#B40F20","#8C510A",
               "#08306B","#7FBC41")
##################################################################################################

#load in strength centralities of participants with >= 30 days of TWeets
dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
dc_net$Id <- dc_net[,1]
dc_net <- dc_net[,-1]
dc_net[,1:10] <- remove_all_outliers(dc_net[1:10])




png("Figures/FigureS1b.png", units="cm", width=40, height=40, res=600)

dc_net %>%
  ggplot( aes(x=Mean_Centrality)) +
  geom_histogram(fill = "black", color="black", alpha=0.8, position = 'identity',bins = 15) +
  labs(fill="") + xlab("Global Network Strength") +  
  theme(text = element_text(size=46),
        axis.title.x = element_text(size = 46),
        axis.title.y = element_text(size = 46),
        axis.text = element_text(size = 46),
        legend.title=element_text(size=46),
        legend.text=element_text(size=46),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm"))

dev.off()


dc_net_bt <- dc_net[,1:9]
colnames(dc_net_bt) <- c("Neg. Emo.","Pos. Emo.","1st Pers.\nSing.","1st Pers.\nPl.","3rd Pers.","2nd Pers.",
                         "Swear","Articles","Negate")
dc_net_bt <- melt(dc_net_bt)

dc_net_bt$variable <- factor(dc_net_bt$variable, levels = c("1st Pers.\nPl.","1st Pers.\nSing.","2nd Pers.","3rd Pers.",
                                                            "Articles","Neg. Emo.","Negate","Pos. Emo.","Swear")) 

cbpalette <- c("#DD8D29","#E2D200","#46ACC8","#0B775E","#C51B7D","#B40F20","#8C510A",
               "#08306B","#7FBC41")


bx_palette <- c("#0B775E","#46ACC8","#B40F20","#C51B7D","#08306B","#DD8D29","#7FBC41","#E2D200","#8C510A")


boxplot2 <- ggplot(data = dc_net_bt, aes(y = value, x = variable,fill = variable)) +
  geom_flat_violin(position = position_nudge(x = .3, y = 0), alpha = .8,adjust= 3,trim = F) +
  geom_point(aes(y = value),position = position_jitter(width = .15), size = 2, alpha = 1) +
  geom_boxplot(width = .4, size = 1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values = bx_palette)+
  scale_fill_manual(values = bx_palette)  + ylab("Strength Centrality") + xlab("") +  
  theme(text = element_text(size=46),
        axis.title.x = element_text(size = 46),
        axis.title.y = element_text(size = 46),
        axis.text = element_text(size = 46),
        legend.title=element_text(size=46),
        legend.text=element_text(size=46),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(1,1,1,1),"cm"))




png("Figures/FigureS1C.png", units="cm", width=80, height=30, res=600)
boxplot2
dev.off()


#---------------------------------------------------------




