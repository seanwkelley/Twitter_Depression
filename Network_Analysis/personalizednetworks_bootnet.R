library(dplyr)
library(ggplot2)
library(lmerTest)
library(broom)
library(reshape2)
library(lme4)
library(stringr)
library(roll)
library(zoo)
library(rlang)
library(bootnet)
library(qgraph)
library(graphicalVAR)
library(NetworkComparisonTest)
set.seed(2020)
#Participants are included if they have at least 5 days with Tweets and at least 
#50% of the tweets from their account are in English 

#within-subject test of difference of sentiments during and off depressive episdoe
#remove participants without a depressive episode in the past year OR
#participants with only non-depressed/depressed data 

#############################################################
#define functions 
#############################################################

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


'%!in%' <- function(x,y)!('%in%'(x,y))

stability_theme = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 28, family = "Arial"),
  axis.title.x = element_text(size = 28),
  axis.title.y = element_text(size = 28),
  axis.text = element_text(size = 28),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

#############################################################
#############################################################
setwd('D:/Twitter_Depression_Kelley/')

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

#############################################################
#############################################################

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

FYP_df <- FYP_df[which(FYP_df$Date != ''),]
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2

#############################################################
network_centrality <- list(); edge_num <- list()
length.days <- list(); network_stability <- list()


PCC_array <- list() 
for(id in unique(FYP_df$Id)){
  
  print(id)
  
  en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,pro3,you,swear,article,negate)
  
  SDS <- as.numeric(participants %>% filter(Id == id) %>% select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)
  
  #series length must be a minimum of 30 days and each node must have some variance 
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(boot_net1 <- bootnet(en_var,nBoots = 100,model = "graphicalVAR",default = "cor",
                      nCores = 4, type = "case",
                     statistics = "strength",caseN = 100),silent = TRUE)
    
    net1_stab <- corStability(boot_net1)

    network_stability[[id]] <- c(as.numeric(net1_stab),dim(en_var)[1])

    
  }
}


full.sample.stab <- as.data.frame(do.call(rbind, network_stability))
colnames(full.sample.stab) <- c("strength","days")
full.sample.stab$Id <- row.names(full.sample.stab)
 
ggplot(data = full.sample.stab, aes(x = days, y = strength)) + geom_point() + 
  stability_theme  + ylab("Strength Stability\n") + geom_hline(yintercept = 0.25,color="blue", linetype="dashed", size=1)

#############################################################
network_stability_dep <- list()
network_stability_nodep <- list()


for(id in unique(FYP_df$Id)){
  
  print(paste0(which(unique(FYP_df$Id) == id)/length(unique(FYP_df$Id))," ",id))
  
  #de Choudhury variables 
  en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,pro3,you,swear,article,negate,Depressed_today)
  

  depression_network <- en_var %>% filter(Depressed_today == 1) %>% select(-Depressed_today)
  nondepression_network <- en_var %>% filter(Depressed_today == 0) %>% select(-Depressed_today)
  
  depression_network <- as.matrix(depression_network); nondepression_network <- as.matrix(nondepression_network)
  
  if(dim(depression_network)[1] >= 15 & all(apply(depression_network, 2, sd) != 0)){
    if(dim(nondepression_network)[1] >= 15 & all(apply(nondepression_network, 2, sd) != 0)){
      
      
      try(depression_boot <- bootnet(depression_network,nBoots = 100,model = "graphicalVAR",default = "cor",
                                     nCores = 4, type = "case",
                                     statistics = "strength",caseN = 100),silent = TRUE)
      
      try(nodepression_boot <- bootnet(nondepression_network,nBoots = 100,model = "graphicalVAR",default = "cor",
                                       nCores = 4, type = "case",
                                       statistics = "strength",caseN = 100),silent = TRUE)
      
      
      depression_stab <- corStability(depression_boot); nodepression_stab <- corStability(nodepression_boot)
      
      network_stability_dep[[id]] <- c(as.numeric(depression_stab),dim(depression_network)[1])
      network_stability_nodep[[id]] <- c(as.numeric(nodepression_stab),dim(nondepression_network)[1])
      
      
    }
  }
}

within.stab <- as.data.frame(do.call(rbind, network_stability_dep))
within.stab[,3] <- 1

outside.stab <- as.data.frame(do.call(rbind, network_stability_nodep))
outside.stab[,3] <- 0

net.stab <- as.data.frame(rbind(within.stab,outside.stab))
colnames(net.stab) <- c("strength","days","episode")

net.stab$Id <- rownames(net.stab); net.stab <- net.stab[order(net.stab$Id),]

g1 <- ggplot(data = net.stab, aes(x = days, y = strength)) + geom_point() + 
  stability_theme  + ylab("Strength Stability\n") + geom_hline(yintercept = 0.25,color="blue", linetype="dashed", size=1)


g2 <- ggplot(data = net.stab, aes(y = strength, x = as.factor(episode), fill = as.factor(episode))) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) + scale_fill_manual(values=c("darkblue", "red"))+
  stability_theme + xlab("") + ylab("Strength Stability\n") +
  scale_x_discrete(labels=c("0" = "Outside Episode", "1" = "Within Episode"))


