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
library(Hmisc)

#need this to ensure, set seed is same across different versions of R
RNGkind(sample.kind = "Rounding")
set.seed(2002)
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


#############################################################
#############################################################
setwd('D:/Twitter_Depression_Kelley/')

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")
participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
dc_all <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
colnames(dc_all)[1] <- "Id"
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_all$Id))
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2

FYP_df <- FYP_df[,c(3,8:94,96)]
FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")
FYP_df_mean[,c(2:87,89)]= remove_all_outliers(FYP_df_mean[c(2:87,89)])
FYP_df_mean[,c(2:87,89)] = scale(FYP_df_mean[,c(2:87,89)])
FYP <- merge(participants,FYP_df_mean,by='Id')

correlation_depression <- FYP %>% select(colnames(FYP)[c(34:119,121)],Depression_zscore)
correlation_depression_mat <- rcorr(as.matrix(correlation_depression), type = "pearson")
dep.net.var <- names(which(correlation_depression_mat$P[,88] <= 0.05))
nodep.net.var <- names(which(correlation_depression_mat$P[,88] > 0.05))

#dc_ep <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode_15d.csv')
#colnames(dc_ep)[1] <- "Id"
#FYP_df <- FYP_df %>% filter(Id %in% unique(dc_ep$Id))

#############################################################

LIWC_var <- list()
for(i in 1:1000) {
  LIWC_var[[i]] <- sample(dep.net.var,9)  
}

network_strength <- list()

id_variables <- unique(FYP_df$Id)

#100 random networks significantly or not significantly associated with current depression severity  
for(i in 1:100) {
  
  network_centrality <- list()
  
  network_variables <- LIWC_var[[i]]
  print(network_variables)
  for(id in id_variables){
    
    print(paste0(i," ",which(unique(FYP_df$Id) == id)/length(unique(FYP_df$Id))," ",id))
    #random variables
    en_var <- FYP_df %>% filter(Id == id) %>% select(network_variables)

    SDS <- as.numeric(participants %>% filter(Id == id) %>% select(Depression_zscore))
    Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
    
    en_var <- as.matrix(en_var)
    
    if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, function(x) length(x[x!=0])) >=2)){
        
      try(net1 <- graphicalVAR(en_var, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                               maxit.out = 100),silent = TRUE)
      
      #strength centrality of contemporaneous network
      net1_PCC <- qgraph(net1$PCC)
      net1_centrality <- centrality(net1_PCC)$InDegree
        
      network_centrality[[id]] <- c(net1_centrality,Dep_ep,dim(en_var)[1],SDS)
      
    }
  } 
  
  
  #node edge strength 
  within_network <- do.call(rbind, network_centrality)
  colnames(within_network )[10:12] <- c("Depressive_Episode_pastyear","Days","Depression_zscore")
  within_network <- as.data.frame(within_network)
  within_network$Mean_Centrality <- rowMeans(within_network[,1:9])
  within_network <- within_network %>% select(colnames(within_network)[1:9],Mean_Centrality,Depressive_Episode_pastyear,Depression_zscore,Days)
  within_network$Id <- rownames(within_network)
  

  within_network <- within_network[order(within_network$Id),]
  
  within_network[,1:10] <- remove_all_outliers(within_network[1:10])
  
  model1 <- summary(glm(Mean_Centrality ~ Depression_zscore,data = within_network))
  model2 <- summary(glm(Mean_Centrality ~ Depression_zscore + Days,data = within_network))
  
  model1_coeff <- c(model1$coefficients[2,1],model1$coefficients[2,2],model1$coefficients[2,4])
  model2_coeff <- c(model2$coefficients[2,1],model2$coefficients[2,2],model2$coefficients[2,4])
  
  write.table(t(model1_coeff),file = "Data/Results/all_tweets/model1_coeff_rand_dep_pro3_depep2.csv",append = T,row.names = F,col.names = F,sep=",")
  write.table(t(model2_coeff),file = "Data/Results/all_tweets/model2_coeff_rand_dep_pro3_depep2.csv",append = T,row.names = F,col.names = F,sep=",")
  
  print(summary(glm(Mean_Centrality ~ Depression_zscore,data = within_network)))
  print(summary(glm(Mean_Centrality ~ Depression_zscore + Days,data = within_network)))
  
  
  
}



