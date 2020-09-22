#------------------------------------------------
#Figure 4: Random networks of 9 text features either significantly or not significantly associated with current depression 

#------------------------------------------------


#Load necessary packages 
library(ggplot2)
library(patchwork)
library(here)
library(dplyr)


setwd('/Users/seankelley/Twitter_Depression_Kelley/')


#100 random networks based on text features with/without a significant association with depression 
#for within-episode effect on global network connectivity 

model2_coeff_dep <- read.csv('Data/Results/all_tweets/model2_coeff_rand_dep_pro3.csv',header = F)
model2_coeff_nodep <- read.csv('Data/Results/all_tweets/model2_coeff_rand_nodep_pro3.csv',header = F)

colnames(model2_coeff_dep) = colnames(model2_coeff_nodep) <- c("beta","se","pval")

hist(model2_coeff_dep$beta); mean(model2_coeff_dep$beta)
hist(model2_coeff_nodep$beta); mean(model2_coeff_nodep$beta)


length(which(abs(model2_coeff_dep$beta) > 0.02435613 ))/length(model2_coeff_dep$beta)
length(which(abs(model2_coeff_nodep$beta) > 0.01536438))/length(model2_coeff_nodep$beta)


model2_coeff_dep$network_type <- "depression";model2_coeff_nodep$network_type <- "no_depression"
model2_coeff <- as.data.frame(rbind(model2_coeff_dep,model2_coeff_nodep))



m2_beta <- model2_coeff %>%
  ggplot( aes(x=beta, fill=network_type)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","grey20"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + xlab("Beta") +  
  geom_vline(aes(xintercept = mean(model2_coeff_dep$beta)),color="grey54", linetype="dashed", size=1.5) +
  geom_vline(aes(xintercept = mean(model2_coeff_nodep$beta)),color="grey20", linetype="dashed", size=1.5) +
  geom_vline(aes(xintercept = 0.02435613),color="darkblue", linetype="dashed", size=1.5) + 
  theme(text = element_text(size=40),
        legend.position = "bottom",
        legend.key.size = unit(3,"line"),
        legend.text = element_text(size = 36),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) +
  scale_x_continuous(breaks=c(-0.03, -0.02, -0.01, 0, 0.01,0.02,0.03))

m2_beta



tiff("Figures/Figure4.tiff", units="cm", width=60, height=30, res=600)
m2_beta
dev.off()
