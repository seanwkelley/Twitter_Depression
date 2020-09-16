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

participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')
#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'
dc_ep <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode.csv')
colnames(dc_ep)[1] <- "Id"
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_ep$Id))

FYP_df <- FYP_df[which(FYP_df$Date != ''),]
#############################################################
depression <- list(); nodepression <- list()

length_depressive.days <- list()
length_no.depressive.days <- list()



for(id in unique(FYP_df$Id)){
  
  print(id)
  
  #de Choudhury variables 
  en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,shehe,they,you,swear,article,negate,Depressed_today)
  
  #sensitivty analysis - remove shehe
  #en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,they,you,swear,article,negate,Depressed_today)
  
  depression_network <- en_var %>% filter(Depressed_today == 1) %>% select(-Depressed_today)
  nondepression_network <- en_var %>% filter(Depressed_today == 0) %>% select(-Depressed_today)
  
  SDS <- as.numeric(participants %>% filter(Id == id) %>% select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
  Depressed_today <- (FYP_df %>% filter(Id == id) %>% select(Depressed_today))$Depressed_today
  
  depression_network <- as.matrix(depression_network); nondepression_network <- as.matrix(nondepression_network)
  
  if(dim(depression_network)[1] >= 15 & all(apply(depression_network, 2, sd) != 0)){
    if(dim(nondepression_network)[1] >= 15 & all(apply(nondepression_network, 2, sd) != 0)){
      
      
      length_depressive.days <- c(length_depressive.days,dim(depression_network)[1])
      length_no.depressive.days <- c(length_no.depressive.days,dim(nondepression_network)[1])
      
      try(net_dep <- graphicalVAR(depression_network, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                                  maxit.out = 100,deleteMissings = TRUE,centerWithin = TRUE),silent = TRUE)
      
      try(net_nodep <- graphicalVAR(nondepression_network, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                                    maxit.out = 100,deleteMissings = TRUE,centerWithin = TRUE),silent = TRUE)
      
      net_dep_PCC <-net_dep$PCC;net_nodep_PCC <-net_nodep$PCC
      

      nodepression[[id]] <- net_nodep_PCC
      depression[[id]] <- net_dep_PCC
    }
  }
}



depression.array <- array(as.numeric(unlist(depression)), dim=c(10, 10, 284))
depression_mean <- as.data.frame(apply(depression.array, c(1,2), mean))
colnames(depression_mean) <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")


nodepression.array <- array(as.numeric(unlist(nodepression)), dim=c(10, 10, 284))
nodepression_mean <- as.data.frame(apply(nodepression.array, c(1,2), mean))
colnames(nodepression_mean) <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")


rownames(depression_mean) <- colnames(depression_mean)
depression_mean <- depression_mean %>%  select(sort(current_vars())) %>% arrange(rownames(depression_mean))

rownames(nodepression_mean) <- colnames(nodepression_mean)
nodepression_mean <- nodepression_mean %>%  select(sort(current_vars())) %>% arrange(rownames(nodepression_mean))



tiff("Figures/Outside_Episode_Network.tiff",width = 8, height = 8, units = 'in', res = 600)
qgraph(nodepression_mean,fade=F,layout = "circle",
       color=  c("#647da2","#8bcbdc","#afd588","#eab779","#ede35f","#db70ad",
                 "#b79265","#62a3c0","#66aa9a","#d06873"),
       vTrans = 255,
       borders = FALSE,minimum = 0.008,
       label.norm = "OOOOOO",label.cex = 2.5,labels = "",
       label.fill.vertical = 1,label.fill.horizontal = 1,
       label.color = "black",label.font = 2,
       node.width = 1.25,edge.width = 5,esize = 5, lty = 1,font = 2)
dev.off()



tiff("Figures/Within_Episode_Network.tiff",width = 8, height = 8, units = 'in', res = 600)
qgraph(depression_mean,fade=F,layout = "circle",
       color=  c("#647da2","#8bcbdc","#afd588","#eab779","#ede35f","#db70ad",
                 "#b79265","#62a3c0","#66aa9a","#d06873"),
       vTrans = 255,
       borders = FALSE,minimum = 0.008,
       label.norm = "OOOOOO",label.cex = 2.5,labels = "",
       label.fill.vertical = 1,label.fill.horizontal = 1,
       label.color = "black",label.font = 2,
       node.width = 1.25,edge.width = 5,esize = 5, lty = 1,font = 2)
dev.off()
