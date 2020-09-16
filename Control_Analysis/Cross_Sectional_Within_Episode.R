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
library(here)

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
here()

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'
dc_ep <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode.csv')
colnames(dc_ep)[1] <- "Id"
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_ep$Id))
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
#############################################################

#within-episode and outside episode networks
dc_dep <- FYP_df %>% select(negemo,posemo,i,we,shehe,they,you,swear,article,negate,Id,Depressed_today) %>% filter(Depressed_today == 1)
dc_nodep <- FYP_df %>% select(negemo,posemo,i,we,shehe,they,you,swear,article,negate,Id,Depressed_today) %>% filter(Depressed_today == 0)


#dc_dep <- FYP_df %>% select(Id,colnames(FYP_df)[8:94]) %>% filter(Depressed_today == 1)
#dc_nodep <- FYP_df %>% select(Id,colnames(FYP_df)[8:94]) %>% filter(Depressed_today == 0)



dc_dep_mean <- aggregate(. ~ Id , data = dc_dep, FUN = "mean") %>% select(-c(Id,Depressed_today))
dc_nodep_mean <- aggregate(. ~ Id , data = dc_nodep, FUN = "mean") %>% select(-c(Id,Depressed_today))

dc_nodep_net <- estimateNetwork(dc_nodep_mean,default = "EBICglasso",tuning=0,corMethod = "cor_auto")
dc_layout <- qgraph(dc_nodep_net$graph,layout="circle",labels = colnames(dc_nodep_net$graph),
                    label.norm = "OOOOOO",label.cex = 4)
mean(centrality(dc_nodep_net)$InDegree)


dc_dep_net <- estimateNetwork(dc_dep_mean,default = "EBICglasso",tuning=0,corMethod = "cor_auto")
qgraph(dc_dep_net$graph,layout="circle",labels = colnames(dc_dep_net$graph),
       label.norm = "OOOOOO",label.cex = 4)
mean(centrality(dc_dep_net)$InDegree)


NCT_centrality <- NCT(dc_nodep_mean,dc_dep_mean,gamma = 0,it = 100, test.centrality = T, centrality = "strength")
NCT_centrality$glstrinv.pval
NCT_centrality$diffcen.pval


NCT_edges <- NCT(dc_nodep_mean,dc_dep_mean,gamma = 0.5,it = 100, test.edges = T,edges = "all")
NCT_centrality$glstrinv.pval


depression_stab <- bootnet(dc_dep_net,nBoots = 100, nCores = 8, type = "case",
                        statistics = "strength",caseN = 100)

corStability(depression_stab)


nodepression_stab <- bootnet(dc_nodep_net,nBoots = 100, nCores = 8, type = "case",
                           statistics = "strength",caseN = 100)

corStability(nodepression_stab)

centralityPlot(list(Outside = dc_nodep_net, Within = dc_dep_net),scale = "raw",
               include = c("Strength"),orderBy = "Strength")

