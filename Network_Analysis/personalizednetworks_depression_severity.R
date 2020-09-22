#----------------------------------------------------------------------
#Personalised networks for all participants (N = 946)
#In participants with at least 30 days of Tweets
#Generates data used to plot Figure 2 and Table S2
#----------------------------------------------------------------------



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
setwd('/Users/seankelley/Twitter_Depression_Kelley/')

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
list.depression <- list()

PCC_array <- list() 
for(id in unique(FYP_df$Id)){
  
  print(paste0(which(unique(FYP_df$Id) == id)/length(unique(FYP_df$Id))," ",id))
  
  en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,pro3,you,swear,article,negate)

  SDS <- as.numeric(participants %>% filter(Id == id) %>% select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)

  #series length must be a minimum of 30 days and each node must have some variance 
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(net1 <- graphicalVAR(en_var, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                             maxit.out = 100),silent = TRUE)
    
    #strength centrality of contemporaneous network
    net1_PCC <- qgraph(net1$PCC)
    net1_centrality <- centrality(net1_PCC)$InDegree
    
    #node number of edges 
    net1_edge <- net1$PCC
    net1_edge[which(net1_edge != 0)] <- 1
    net1_edge <- colSums(net1_edge)
  
    
    network_centrality[[id]] <- c(net1_centrality,Dep_ep,dim(en_var)[1],SDS)
    edge_num[[id]] <- c(net1_edge,Dep_ep,SDS)

    list.depression[[id]] <- SDS
    PCC_array[[id]] <- net1$PCC
    
  }
}

#node edge strength 
indiv_nets <- do.call(rbind, network_centrality)
colnames(indiv_nets)[10:12] <- c("Depressive_Episode_pastyear","Days","Depression_zscore")
indiv_nets <- as.data.frame(indiv_nets)
indiv_nets$Mean_Centrality <- rowMeans(indiv_nets[,1:9])
indiv_nets <- indiv_nets %>% select(colnames(indiv_nets)[1:9],Mean_Centrality,Depressive_Episode_pastyear,Days,Depression_zscore)
write.csv(indiv_nets,file = "Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv")


#mean edge strengths 
PCC_array <- array(as.numeric(unlist(PCC_array)), dim=c(9, 9, 942))
PCC_array_mean <- as.data.frame(apply(PCC_array, c(1,2), mean))
colnames(PCC_array_mean) <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")

#############################################################
