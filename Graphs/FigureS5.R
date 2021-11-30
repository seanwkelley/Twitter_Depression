#--------------------------------------------------------
#Figure S5: Bootstrapped regression coefficient of change in network connectivity within a depressive episode 
#from 80% random sub-samples of the data repeated 1,000 times
#--------------------------------------------------------


#load 
library(dplyr)
library(lmerTest)

setwd('/Users/seankelley/Twitter_Depression_Kelley/')

within_network <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode_15d.csv')
within_network <- within_network[,-1]
within_network[,1:10] <- remove_all_outliers(within_network[1:10])
summary(lmer(Mean_Centrality ~ Depressed_Today + (1|Id),data = within_network))


coef_list <- list()

for(i in 1:1000) {
  print(i)
within_network2 <- within_network %>% filter(Id %in% sample(unique(within_network$Id), 286*.8))

within_network2[,1:10] <- remove_all_outliers(within_network2[1:10])
coef_lmr <- summary(lmer(Mean_Centrality ~ Depressed_Today + (1|Id),data = within_network2))

coef_list[[i]] <- coef_lmr$coefficients[2]

}

model1 <- as.data.frame(melt(coef_list)$value)
colnames(model1) <- "value"


m1_graph <- model1%>%
  ggplot( aes(x=value)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","grey20"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + xlab("Beta") +  
  geom_vline(aes(xintercept = 0.02554),color="darkblue", linetype="dashed", size=4) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) +
  scale_x_continuous(breaks=c(0.01,0.015,0.02,0.025,0.03,0.035,0.04))


png("Figures/FigureS5.png", units="cm", width=60, height=30, res=600)
m1_graph
dev.off()

