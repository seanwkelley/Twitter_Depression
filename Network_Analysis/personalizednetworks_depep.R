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
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep_FYPSG.csv",collapse = "")


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/FYP.SG_Twitter_Participants.csv')

#############################################################
#############################################################

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

FYP_df <- FYP_df[which(FYP_df$Date != ''),]

#############################################################
network_centrality <- list(); edge_num <- list()

for(id in unique(FYP_df$Id)){
  
  print(id)
  
  en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,shehe,they,you,swear,article,negate)
  
  SDS <- as.numeric(participants %>% filter(Id == id) %>% select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)
  
  #series length must be a minimum of 30 days and each node must have some variance 
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(net1 <- graphicalVAR(en_var, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                             maxit.out = 100),silent = TRUE)
    
    net1_PCC <- qgraph(net1$PCC)
    net1_centrality <- centrality(net1_PCC)$InDegree
    
    #node number of edges 
    net1_edge <- net1$PCC
    net1_edge[which(net1_edge != 0)] <- 1
    net1_edge <- colSums(net1_edge)
    
    inexpectedinfluence <- centrality(net1_PCC)$InExpectedInfluence
    
    network_centrality[[id]] <- c(net1_centrality,Dep_ep,SDS)
    edge_num[[id]] <- c(net1_edge,Dep_ep,SDS)
    
  }
}

#node edge strength 
indiv_nets <- do.call(rbind, network_centrality)
colnames(indiv_nets)[11:12] <- c("Depressive_Episode_pastyear","SDS_Total")
indiv_nets <- as.data.frame(indiv_nets)
indiv_nets$Mean_Centrality <- rowMeans(indiv_nets[,1:10])
indiv_nets <- indiv_nets %>% select(colnames(indiv_nets)[1:10],Mean_Centrality,Depressive_Episode_pastyear,SDS_Total)

#remove outliers 
indiv_nets[,1:11] <- remove_all_outliers(indiv_nets[1:11])
summary(glm(Mean_Centrality ~ Depressive_Episode_pastyear,family = gaussian(),data = indiv_nets))
summary(glm(ngm ~ Depressive_Episode_pastyear,family = gaussian(),data = indiv_nets))

summary(glm(Mean_Centrality ~ SDS_Total,family = gaussian(),data = indiv_nets))
summary(glm(ngm ~ SDS_Total,family = gaussian(),data = indiv_nets))

#node edge number 
node_edges <- do.call(rbind, edge_num)
colnames(node_edges)[11:12] <- c("Depressive_Episode_pastyear","SDS_Total")
node_edges <- as.data.frame(node_edges)
node_edges$Mean_Centrality <- rowMeans(node_edges[,1:10])
node_edges <- node_edges %>% select(colnames(node_edges)[1:10],Mean_Centrality,Depressive_Episode_pastyear,SDS_Total)

#############################################################
#figures 

ggplot(data = indiv_nets, aes(x = SDS_Total,y=ngm)) + geom_point() + xlab("Sentiment Mean")  +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.x = element_text(size = 20),
                     axis.title.y = element_text(size = 20)) + 
  ggtitle("Sentiment: negemo \nIncludes: Tweets + Retweets + Likes") 

ggplot(data = indiv_nets, aes(x = SDS_Total,y=Mean_Centrality)) + geom_point() + xlab("Current Depression (SDS)")  +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.x = element_text(size = 20),
                     axis.title.y = element_text(size = 20)) + ylab("Mean Node Strength")

#box plots for differences between sentiments for participants with/without a depressive episode in the past year 
indiv_nets$Depressive_Episode_pastyear <- as.factor(indiv_nets$Depressive_Episode_pastyear)
#mean word count
ggplot(data = indiv_nets, aes(x = Depressive_Episode_pastyear,y=Mean_Centrality)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("No Depressive Episode","Depressive Episode")) + ylab("Mean Node Strength") + 
  theme(axis.title.x=element_blank())




