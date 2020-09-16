library(ggplot2)
library(patchwork)
library(here)
library(dplyr)


setwd('D:/Twitter_Depression_Kelley/')


#100 random networks - random order dechoudhury network
model1_coeff <- read.csv('Data/Results/all_tweets/model1_coeff_randomize_dechoud.csv',header = F)

colnames(model1_coeff) <- c("beta","se","pval")

mean(model1_coeff$beta); sd(model1_coeff$beta)
length(which(abs(model1_coeff$beta) > 0.025542))/length(model1_coeff$beta)
length(which(abs(model1_coeff$pval) < 0.05))/length(model1_coeff$pval)

#is the mean significantly different from 0?
summary(glm(beta ~ 1, data = model1_coeff))
#crude mean centrality 

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



#---------------------------------------------------------------------------------


#100 random networks based on sentiments with/without a significant association with depression 
#for within-episode effect on elevated global network connectivity 

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
  geom_histogram( color="black", alpha=0.2, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("red", "darkblue"), labels = c("Association with depression","No association with depression")) +
  labs(fill="") + xlab("Beta") +  
  geom_vline(aes(xintercept = mean(model2_coeff_dep$beta)),color="red", linetype="dashed", size=1.5) +
  geom_vline(aes(xintercept = mean(model2_coeff_nodep$beta)),color="darkblue", linetype="dashed", size=1.5) +
  theme(text = element_text(size=40),
        legend.text=element_text(size=36),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) 

m2_pval <- model2_coeff %>%
  ggplot( aes(x=pval, fill=network_type)) +
  geom_histogram( color="black", alpha=0.2, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("red", "darkblue"), labels = c("Association with depression","No association with depression")) +
  labs(fill="") + xlab("p-value") +  
  geom_vline(aes(xintercept = 0.05),
             color="black", linetype="dashed", size=1.5) +
  theme(text = element_text(size=40),
        legend.text=element_text(size=28),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) 

m2_beta


tiff("Figures/randnets_random_depression.tiff", units="cm", width=80, height=30, res=600)
m2_beta
dev.off()
