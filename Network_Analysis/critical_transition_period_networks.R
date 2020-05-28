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
  s <- sd(na.omit(x))
  m <- mean(na.omit(x))
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


gaussian_detrend <- function(df,bandwidth_ksmooth,var) {
  for(i in 5:14){
    var_smooth <- suppressWarnings(ksmooth(df$Day,na.approx(df[,i]) ,
                                           n.points = length(df$Day), 
                                           kernel =  "normal", bandwidth = bandwidth_ksmooth))
    
    #remove any trends by subtracting the smoothed series from the original 
    df[,i] <- suppressWarnings((df[,i]) - var_smooth$y)
  }
  df
}
'%!in%' <- function(x,y)!('%in%'(x,y))

#############################################################
#############################################################
setwd('D:/Twitter_Depression_Kelley/')

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep_ct30.csv",collapse = "")
#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

ct <-  15 #minimum number of days to inlcude in close/far network 


#############################################################
#############################################################

FYP_df_subset <- FYP_df
FYP_df_subset <- FYP_df_subset[which(FYP_df_subset$Date != ""),]
FYP_df_subset$Depressed_today <- as.factor(FYP_df_subset$Depressed_today)

######################################################################################
#############################################################################
#Autocorrelation and Variance - Critical Slowing Down 
#do analysis with and without linear interpolation of missing days 
#1. linear interpolation of missing values 
#3. Gaussian Kernel Smoothing and substraction from original time series 
#4. 7 day rolling-window for increase in autocorrelation and variance 
#5. Compare 14 day period prior to onset of a depressive episode with random time series (permutation test)
#############################################################################

#list of unique participant ids 

id_list <- unique(FYP_df_subset$Id)

close_strength <- matrix(nrow = length(id_list),ncol = 12); far_strength <- matrix(nrow = length(id_list),ncol = 12)

number <- 0 
for(handle in id_list) {
  print(handle)
  number = number + 1
  ex1 <- FYP_df_subset %>% filter(Id == handle) %>% select(Id,Day,Depressed_today,critical_period,
                                                           negemo,posemo,i,we,shehe,they,you,swear,
                                                           article,negate)
  
  ex1$Depressed_today <- as.numeric(as.character(ex1$Depressed_today))
  
    
      #dataframe with de choudhury variables 
      close_period <- ex1 %>% filter(critical_period == 1) %>% select(-c(Id,Day,Depressed_today,critical_period)) 
      far_period <- ex1 %>% filter(Depressed_today == 0 & critical_period == 0) %>% select(-c(Id,Day,Depressed_today,critical_period))

  
      if(dim(close_period)[1] >= ct & dim(far_period)[1] >= ct){
        print(dim(close_period)[1])
        print(dim(far_period)[1])

      if(all(apply(close_period, 2, sd) != 0) & all(apply(far_period, 2, sd) != 0)){
        try(close_net <- graphicalVAR(close_period, nLambda = 10, verbose = TRUE, gamma = 0.1,scale = TRUE, maxit.in = 100,
                                      maxit.out = 100,deleteMissings = TRUE),silent = TRUE)
        
        try(far_net <- graphicalVAR(far_period, nLambda = 10, verbose = TRUE, gamma = 0.1,scale = TRUE, maxit.in = 100,
                                    maxit.out = 100,deleteMissings = TRUE),silent = TRUE)
        
        
        close_net_PCC <- qgraph(close_net$PCC)
        far_net_PCC <- qgraph(far_net$PCC)
        
        close_indegree <- centrality(close_net_PCC)$OutDegree
        far_indegree <- centrality(far_net_PCC)$OutDegree
        
        close_strength[number,] <- c(close_indegree,'close',handle)
        far_strength[number,] <- c(far_indegree,'far',handle) 
      }

      }
      
  
}

#############################################################################
close_strength2 <- na.omit(close_strength)
far_strength2 <- na.omit(far_strength)

within_network <- as.data.frame(rbind(close_strength2,far_strength2))
colnames(within_network) <- c("negemo","posemo","i","we","shehe","they",
                              "you","swear","article","negate","Period","Id")
within_network <- within_network[order(within_network$Id),]
within_network[,1:10] <- lapply(within_network[,1:10], function(x) as.numeric(as.character(x)))

within_network$Mean_Strength <- rowMeans(within_network[,1:10])
within_network[,c(1:10,13)] <- remove_all_outliers(within_network[c(1:10,13)])

summary(lmer(Mean_Strength ~ Period + (1|Id),data = within_network))
summary(lmer(negemo ~ Period + (1|Id),data = within_network))
summary(lmer(posemo ~ Period + (1|Id),data = within_network))
summary(lmer(i ~ Period + (1|Id),data = within_network))
summary(lmer(we ~ Period + (1|Id),data = within_network))
summary(lmer(you ~ Period + (1|Id),data = within_network))
summary(lmer(they ~ Period + (1|Id),data = within_network))
summary(lmer(swear ~ Period + (1|Id),data = within_network))
summary(lmer(article ~ Period + (1|Id),data = within_network))
summary(lmer(negate ~ Period + (1|Id),data = within_network))
summary(lmer(shehe ~ Period + (1|Id),data = within_network))


