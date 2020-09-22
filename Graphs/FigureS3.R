#------------------------------------------------
#Figure S3: Permutation test randomising the indicator (within/outside episode) per subject 100 times and comparing network strength within subject using this indicator. 
#------------------------------------------------


library(ggplot2)
library(patchwork)
library(here)
library(dplyr)

setwd('/Users/seankelley/Twitter_Depression_Kelley/')

#random networks - random order of episode identifer in a priori LIWC network 
model1_coeff <- read.csv('Data/Results/all_tweets/model1_coeff_randomize_dechoud.csv',header = F)
colnames(model1_coeff) <- c("beta","se","pval")

#descriptive statistics of mean/standard deviation
mean(model1_coeff$beta); sd(model1_coeff$beta)

#number of betas greater than the observed in real data 
#number of p-values below 0.05
length(which(abs(model1_coeff$beta) > 0.025542))/length(model1_coeff$beta)
length(which(abs(model1_coeff$pval) < 0.05))/length(model1_coeff$pval)

#is the mean randomised network beta significantly different from 0?
summary(glm(beta ~ 1, data = model1_coeff))


#beta distribution
m1_beta <- ggplot(data = model1_coeff, aes(x = beta))+ geom_histogram(color= "black",fill="white",bins=15) +
  xlab("Beta") +  geom_vline(aes(xintercept = 0.025542),
                             color="darkblue", linetype="dashed",  size=1.5) +
  theme(text = element_text(size=40),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  coord_cartesian(xlim=c(-0.01, 0.03))

#p-value distribution
m1_pval <- ggplot(data = model1_coeff, aes(x = pval))+ geom_histogram(color= "black",fill="white",bins=15) +
  xlab("p-value") +geom_vline(aes(xintercept = 0.00548), 
                            color="darkblue", linetype="dashed", size=1.5) + 
  geom_vline(aes(xintercept = 0.05), 
             color="red", linetype="dashed", size=1.5) + 
                theme(text = element_text(size=40),
                 axis.text.x = element_text(colour = "black"),
                 axis.text.y = element_text(colour = "black"),
                 panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 plot.margin=unit(c(0,1,0,0),"cm")) +
  coord_cartesian(xlim=c(0, 1))

#mean centrality after controlling for number of days 
combined <- (m1_beta + m1_pval) + plot_annotation(tag_levels = 'A')

combined

tiff("Figures/model1_dechoud_permutation_test.tiff", units="cm", width=80, height=30, res=600)
combined
dev.off()


