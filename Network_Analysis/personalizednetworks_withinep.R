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

FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

FYP_df <- FYP_df[which(FYP_df$Date != ''),]
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2
#############################################################
depression_centrality <- list(); nodepression_centrality <- list()

length_depressive.days <- list()
length_no.depressive.days <- list()

for(id in unique(FYP_df$Id)){
  
  print(paste0(which(unique(FYP_df$Id) == id)/length(unique(FYP_df$Id))," ",id))
  #de Choudhury variables 
  
  en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,pro3,you,swear,article,negate,Depressed_today)

  
  depression_network <- en_var %>% filter(Depressed_today == 1) %>% select(-Depressed_today)
  nondepression_network <- en_var %>% filter(Depressed_today == 0) %>% select(-Depressed_today)
  
  SDS <- as.numeric(participants %>% filter(Id == id) %>% select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
  Depressed_today <- (FYP_df %>% filter(Id == id) %>% select(Depressed_today))$Depressed_today
  
  depression_network <- as.matrix(depression_network); nondepression_network <- as.matrix(nondepression_network)
  
  if(dim(depression_network)[1] >= 15 & all(apply(depression_network, 2, sd) != 0)){
    if(dim(nondepression_network)[1] >= 15 & all(apply(nondepression_network, 2, sd) != 0)){

      print(dim(depression_network)[1]); print(dim(nondepression_network)[1])

      try(net_dep <- graphicalVAR(depression_network, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                                  maxit.out = 100,deleteMissings = TRUE,centerWithin = TRUE),silent = TRUE)
      
      try(net_nodep <- graphicalVAR(nondepression_network, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                                    maxit.out = 100,deleteMissings = TRUE,centerWithin = TRUE),silent = TRUE)
      
      net_dep_PCC <- qgraph(net_dep$PCC);net_nodep_PCC <- qgraph(net_nodep$PCC)
      
      net_dep_centrality <- centrality(net_dep_PCC)$InDegree;net_nodep_centrality <- centrality(net_nodep_PCC)$InDegree
      
      nodepression_centrality[[id]] <- c(net_nodep_centrality,Dep_ep,dim(nondepression_network)[1],SDS,0)
      depression_centrality[[id]] <- c(net_dep_centrality,Dep_ep,dim(depression_network)[1],SDS,1)
    }
  }
}

#node edge strength 
nodep_net <- do.call(rbind, nodepression_centrality)
colnames(nodep_net )[10:13] <- c("Depressive_Episode_pastyear","Days","Depression_zscore","Depressed_Today")
nodep_net <- as.data.frame(nodep_net)
nodep_net$Mean_Centrality <- rowMeans(nodep_net[,1:9])
nodep_net <- nodep_net %>% select(colnames(nodep_net)[1:9],Mean_Centrality,Depressive_Episode_pastyear,Depression_zscore,Days,Depressed_Today)
nodep_net$Id <- rownames(nodep_net)

dep_net <- do.call(rbind, depression_centrality)
colnames(dep_net )[10:13] <- c("Depressive_Episode_pastyear","Days","Depression_zscore","Depressed_Today")
dep_net <- as.data.frame(dep_net)
dep_net$Mean_Centrality <- rowMeans(dep_net[,1:9])
dep_net <- dep_net %>% select(colnames(dep_net)[1:9],Mean_Centrality,Depressive_Episode_pastyear,Depression_zscore,Days,Depressed_Today)
dep_net$Id <- rownames(dep_net)

within_network <- as.data.frame(rbind(dep_net,nodep_net))
within_network <- within_network[order(within_network$Id),]
write.csv(within_network,file = "Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode_15d.csv")

within_network[,1:10] <- remove_all_outliers(within_network[1:10])


               

summary(lmer(Mean_Centrality ~ Depressed_Today + (1|Id),data = within_network))
summary(lmer(ngm ~ Depressed_Today + (1|Id),data = within_network))
summary(lmer(psm ~ Depressed_Today +  (1|Id),data = within_network))
summary(lmer(i ~ Depressed_Today + (1|Id),data = within_network))
summary(lmer(we ~ Depressed_Today + (1|Id),data = within_network))
summary(lmer(pro3 ~ Depressed_Today +   (1|Id),data = within_network))
summary(lmer(you ~ Depressed_Today + (1|Id),data = within_network))
summary(lmer(swr ~ Depressed_Today +  (1|Id),data = within_network))
summary(lmer(art ~ Depressed_Today +   (1|Id),data = within_network))
summary(lmer(ngt ~ Depressed_Today +  (1|Id),data = within_network))



summary(lmer(Mean_Centrality ~ Depressed_Today + Days + (1|Id),data = within_network))
summary(lmer(ngm ~ Depressed_Today +  Days + (1|Id),data = within_network))
summary(lmer(psm ~ Depressed_Today +  Days +(1|Id),data = within_network))
summary(lmer(i ~ Depressed_Today +  Days +(1|Id),data = within_network))
summary(lmer(we ~ Depressed_Today + Days +  (1|Id),data = within_network))
summary(lmer(pro3 ~ Depressed_Today +  Days + (1|Id),data = within_network))
summary(lmer(you ~ Depressed_Today +  Days + (1|Id),data = within_network))
summary(lmer(swr ~ Depressed_Today +  Days + (1|Id),data = within_network))
summary(lmer(art ~ Depressed_Today +  Days + (1|Id),data = within_network))
summary(lmer(ngt ~ Depressed_Today +  Days + (1|Id),data = within_network))









