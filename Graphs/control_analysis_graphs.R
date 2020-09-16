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
library(extrafont)
loadfonts(device = "win")



setwd('D:/Twitter_Depression_Kelley/')

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
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


sensitivity_theme = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 36, family = "Arial"),
  plot.margin=unit(c(1,0,0,0),"cm"),
  axis.title.x = element_text(size = 36),
  axis.title.y = element_text(size = 36),
  axis.text = element_text(size = 36),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

##################################################################################################
#within episode network connectivty 

dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode_15d.csv')
dc_net <- dc_net[,-1]
dc_net[,1:11] <- remove_all_outliers(dc_net[1:11])


g1 <- ggplot(data = dc_net, aes(y = Days, x = as.factor(Depressed_Today), fill = as.factor(Depressed_Today))) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) + scale_fill_manual(values=c("darkblue", "red"))+
  sensitivity_theme + xlab("") + ylab("Days\n") +
  scale_x_discrete(labels=c("0" = "Outside Episode", "1" = "Within Episode"))


g2 <- ggplot(data = dc_net, aes(x = Days, y = Mean_Centrality,color = as.factor(Depressed_Today))) + geom_point(alpha= 0.5,size = 3) + 
  sensitivity_theme + ylab("Global Network Connectivity\n") + 
  scale_color_manual(values=c("darkblue", "red")) +
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black")


##################################################################################################
#Association between network connectivity and current depression severity 

dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
dc_net <- dc_net[,-1]
dc_net[,1:10] <- remove_all_outliers(dc_net[1:10])



g3 <- ggplot(data = dc_net, aes(x = Days, y = Mean_Centrality)) + geom_point(size = 3) + 
  sensitivity_theme + ylab("Global Network Connectivity\n")  +
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black")

g4 <-  ggplot(data = dc_net, aes(x = Days, y = Depression_zscore)) + geom_point(size = 3) + 
  sensitivity_theme + ylab("Current Depression Severity\n") +
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") 

summary(glm(Depression_zscore ~ Days, data = dc_net))


##################################################################################################

combined <- (g3 + g4) / (g1 + g2) + plot_annotation(tag_levels = 'A')


tiff("Figures/control_analysis_days.tiff", units="cm", width=70, height=50, res=600)
combined
dev.off()
