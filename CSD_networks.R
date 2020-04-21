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
remove_ids <- list()
for(i in 1:length(unique(FYP_df$Id))) {
  print(i)
  if( sd(as.numeric(as.character(FYP_df[which(FYP_df$Id == unique(FYP_df$Id)[i]),94]))) == 0) {
    ld <- length(which(FYP_df[which(FYP_df$Id == unique(FYP_df$Id)[1]),94] == 1))
    ld2 <- length(which(FYP_df[which(FYP_df$Id == unique(FYP_df$Id)[1]),94] == 0))
    if(ld >= 10 & ld2 >= 10){
    remove_ids[[i]] <- as.character(unique(FYP_df$Id)[i])
    }
  }
}

remove_ids <- unlist(remove_ids)
FYP_df_subset <- FYP_df[which(FYP_df$Id %!in% remove_ids),]

#remove outliers in any of the sentiments (LIWC and ANEW)
FYP_df_subset[,6:93]= remove_all_outliers(FYP_df_subset[6:93])
FYP_df_subset$Depressed_today <- as.factor(FYP_df_subset$Depressed_today)

#############################################################################
#############################################################################

#f1 <- FYP_df_subset %>% select(Day,Id, WPS,relig,WC,bio,leisure, shehe, death, negemo,work,
#               QMark,adverb, verb, filler, compare, sad,Depressed_today)

f1 <- FYP_df_subset %>% select(Day,Id, WPS,relig,WC,bio,leisure, shehe, death, negemo,work,
               QMark,adverb, verb, filler, compare, sad,focuspresent, money, focusfuture,
               you, Sixltr,Depressed_today)



f1_mean <- aggregate(. ~ Depressed_today + Id,data = f1,FUN = mean)
f1_mean.dep <- f1_mean %>% filter(Depressed_today == 1) %>% select(-c(Depressed_today,Day))
f1_mean.nodep <- f1_mean %>% filter(Depressed_today == 0) %>% select(-c(Depressed_today,Day))


#############################################################################
N1 <- estimateNetwork(f1_mean.dep[,2:16],default = "EBICglasso",tuning=0,corMethod = "cor_auto")
N2 <- estimateNetwork(f1_mean.nodep[,2:16],default = "EBICglasso",tuning=0,corMethod = "cor_auto")

N1_graph <- qgraph(N1$graph,layout="spring",labels = colnames(N1$graph))
centralityPlot(list(Depressed = N1,Not_Depressed=N2),include = c("Closeness","Betweenness","Strength"),
               scale = "raw")

#network plots 
qgraph(N1$graph,layout="spring",labels = colnames(N1$graph))
layout_N1 <- qgraph(N1$graph,layout="spring",labels = colnames(N1$graph))$layout
qgraph(N2$graph,layout=layout_N1,labels = colnames(N1$graph))
