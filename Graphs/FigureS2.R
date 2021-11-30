#------------------------------------------------
#Figure S2: Random networks of 9 text features either significantly ('Depression Relevant') or not significantly ('Depression Irrelevant') 
#associated with current depression excluding LIWC supra-categories. 

#------------------------------------------------


#Load necessary packages 
library(ggplot2)
library(patchwork)
library(here)
library(dplyr)
library(tidyr)

setwd('/Users/seankelley/Twitter_Depression_Kelley/')


model2_coeff_dep <- read.csv('Data/Results/all_tweets/model2_dep_hier.csv',header = F)
model2_coeff_nodep <- read.csv('Data/Results/all_tweets/model2_nodep_hier.csv',header = F)


colnames(model2_coeff_dep) = colnames(model2_coeff_nodep) <- c("variables","beta","se","pval")

mean(model2_coeff_dep$beta)
mean(model2_coeff_nodep$beta)


length(which(abs(model2_coeff_dep$beta) > 0.02435613 ))/length(model2_coeff_dep$beta)
length(which(abs(model2_coeff_nodep$beta) > 0.01536438))/length(model2_coeff_nodep$beta)


model2_coeff_dep$network_type <- "depression";model2_coeff_nodep$network_type <- "no_depression"
model2_coeff <- as.data.frame(rbind(model2_coeff_dep,model2_coeff_nodep))


summary(glm(beta ~ network_type,data = model2_coeff))


p2 <- model2_coeff %>%
  ggplot( aes(x=beta, fill=network_type)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","grey20"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + xlab("Beta") +  
  geom_vline(aes(xintercept = mean(model2_coeff_dep$beta)),color="grey54", linetype="dashed", size=1.5) +
  geom_vline(aes(xintercept = mean(model2_coeff_nodep$beta)),color="grey20", linetype="dashed", size=1.5) +
  #geom_vline(aes(xintercept = 0.02435613),color="darkblue", linetype="dashed", size=1.5) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) + xlim(c(-0.3,0.15)) + 
  scale_x_continuous(breaks=c(-0.3, -0.2, -0.1, 0, 0.1,0.2))


png("Figures/FigureS2.png", units="cm", width=75, height=30, res=600)
p1 + p2 
dev.off()

