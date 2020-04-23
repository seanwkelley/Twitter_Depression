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
path2 = paste0('Data/Sentiments/',tweet_type,"/CSD_Episodes.csv",collapse = "")

#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

episode = read.csv(path2,stringsAsFactors = FALSE)
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
#remove participants with either no depressed episodes reported in the past year OR 
#participants with a depressive episode that covers the entire past year

#############################################################
network_centrality <- list()

for(id in unique(FYP_df$Id)){
  
  print(id)
  en_var <- FYP_df %>% filter(Id == id) %>% select(c(WC,bio,WPS,negemo,shehe,adverb,leisure,Exclam,death,
                                                     Sixltr,relig,verb,compare,we,sad))
  
  SDS <- as.numeric(participants %>% filter(Id == id) %>% select(SDS_Total))
  Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
  print(dim(en_var)[1])
  net1 <- graphicalVAR(en_var, nLambda = 25, verbose = TRUE, gamma = 0)
  net1_PCC <- qgraph(net1$PCC)
  net1_centrality <- centrality(net1_PCC)$InDegree
  
  network_centrality[[id]] <- c(net1_centrality,Dep_ep,SDS)
  }
}
indiv_nets <- do.call(rbind, network_centrality)
colnames(indiv_nets)[16:17] <- c("Depressive_Episode_pastyear","SDS_Total")

