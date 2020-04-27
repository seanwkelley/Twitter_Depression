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

participants <- read.csv('Data/Participant_Data/FYP_Twitter_Participants.csv')

participants$SDS_2 <- 5 - participants$SDS_2
participants$SDS_5 <- 5 - participants$SDS_5
participants$SDS_6 <- 5 - participants$SDS_6
participants$SDS_11 <- 5 - participants$SDS_11
participants$SDS_12 <- 5 - participants$SDS_12
participants$SDS_14 <- 5 - participants$SDS_14
participants$SDS_16 <- 5 - participants$SDS_16
participants$SDS_17 <- 5 - participants$SDS_17
participants$SDS_18 <- 5 - participants$SDS_18
participants$SDS_20 <- 5 - participants$SDS_20

#SDS: Score system
#Below 50 is considered normal range - 502 people (54.5%)
#50-59 is mild depression - 259 people (28.1%)
#60-69 is moderate depression - 138 people (14.9%)
#above 70 is severe depression - 24 people (2.9%)

participants$SDS_Total <- rowSums(participants %>% select(colnames(participants)[grepl("SDS",colnames(participants))]))

#############################################################
#############################################################

#keep participants from free recruitment (OCI_6 coded as NA) and those who successfully completed the 
#attention check
participants <- participants[which(is.na(participants$OCI_6) | participants$OCI_6 == 1),]

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

FYP_df <- FYP_df[which(FYP_df$Date != ''),]
#create a 3rd person pronoun category 
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2
#############################################################
depression_centrality <- list(); nodepression_centrality <- list()

for(id in unique(FYP_df$Id)){
  
  print(id)
  
  en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,shehe,they,you,swear,article,negate,Depressed_today)
  #en_var <- FYP_df %>% filter(Id == id) %>% select(WC,bio,WPS,negemo,shehe,adverb,leisure,Exclam,death,Sixltr,Depressed_today)
  
  
  depression_network <- en_var %>% filter(Depressed_today == 1) %>% select(-Depressed_today)
  nondepression_network <- en_var %>% filter(Depressed_today == 0) %>% select(-Depressed_today)
  
  SDS <- as.numeric(participants %>% filter(Id == id) %>% select(SDS_Total))
  Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
  Depressed_today <- (FYP_df %>% filter(Id == id) %>% select(Depressed_today))$Depressed_today
  
  depression_network <- as.matrix(depression_network); nondepression_network <- as.matrix(nondepression_network)

  if(dim(depression_network)[1] >= 15 & all(apply(depression_network, 2, sd) != 0)){
    if(dim(nondepression_network)[1] >= 15 & all(apply(nondepression_network, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(net_dep <- graphicalVAR(depression_network, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                         maxit.out = 100,deleteMissings = TRUE),silent = TRUE)
    
    try(net_nodep <- graphicalVAR(nondepression_network, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                            maxit.out = 100,deleteMissings = TRUE),silent = TRUE)
    
    net_dep_PCC <- qgraph(net_dep$PCC);net_nodep_PCC <- qgraph(net_nodep$PCC)
    
    net_dep_centrality <- centrality(net_dep_PCC)$InDegree;net_nodep_centrality <- centrality(net_nodep_PCC)$InDegree
    
    nodepression_centrality[[id]] <- c(net_nodep_centrality,Dep_ep,SDS,0)
    depression_centrality[[id]] <- c(net_dep_centrality,Dep_ep,SDS,1)
    }
  }
}

#node edge strength 
nodep_net <- do.call(rbind, nodepression_centrality)
colnames(nodep_net )[11:13] <- c("Depressive_Episode_pastyear","SDS_Total","Depressed_Today")
nodep_net <- as.data.frame(nodep_net)
nodep_net$Mean_Centrality <- rowMeans(nodep_net[,1:10])
nodep_net <- nodep_net %>% select(colnames(nodep_net)[1:10],Mean_Centrality,Depressive_Episode_pastyear,SDS_Total,Depressed_Today)
nodep_net$Id <- rownames(nodep_net)

dep_net <- do.call(rbind, depression_centrality)
colnames(dep_net )[11:13] <- c("Depressive_Episode_pastyear","SDS_Total","Depressed_Today")
dep_net <- as.data.frame(dep_net)
dep_net$Mean_Centrality <- rowMeans(dep_net[,1:10])
dep_net <- dep_net %>% select(colnames(dep_net)[1:10],Mean_Centrality,Depressive_Episode_pastyear,SDS_Total,Depressed_Today)
dep_net$Id <- rownames(dep_net)

within_network <- as.data.frame(rbind(dep_net,nodep_net))
within_network <- within_network[order(within_network$Id),]

summary(lmer(Mean_Centrality ~ Depressed_Today + (1|Id),data = within_network))
summary(lmer(ngm ~ Depressed_Today + (1|Id),data = within_network))

#############################################################
#figures 

#box plots for differences between sentiments for participants with/without a depressive episode in the past year 
within_network$Depressed_Today <- as.factor(within_network$Depressed_Today)

#mean word count
ggplot(data = within_network, aes(x = Depressed_Today,y=Mean_Centrality)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("No Depressive Episode","Depressive Episode")) +
  theme(axis.title.x=element_blank())


ggplot(data = within_network, aes(x = Depressed_Today,y=ngm)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("No Depressive Episode","Depressive Episode")) +
  theme(axis.title.x=element_blank())

