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
gaussian_detrend <- function(df,bandwidth_ksmooth) {
  for(i in 2:11){
    var_smooth <- suppressWarnings(ksmooth(df$Day,na.approx(df[,i]) ,
                                           n.points = length(df$Day), 
                                           kernel =  "normal", bandwidth = bandwidth_ksmooth))
    
    #remove any trends by subtracting the smoothed series from the original 
    df[,i] <- suppressWarnings((df[,i]) - var_smooth$y)
  }
  df
}
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
participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')


ct <- 30 #number of days in network
rw_adjust <- 2*ct  #minimum duration between depressive episodes to have one period prior to onset and one period away from onset 
percent_complete = 0.5

#############################################################
#############################################################

FYP_df_subset <- FYP_df
FYP_df_subset <- FYP_df_subset[which(FYP_df_subset$Date != ""),]
FYP_df_subset$Depressed_today <- as.factor(FYP_df_subset$Depressed_today)

######################################################################################
#Participants are included if they have at least 1 depressive with the following characteristics:
#1. Have at least 80% of days in the 30 day period prior to a critical transiton with Tweet days
#2. Have at least 60 days from the end of one depressive episode to the start of another depressive episode 

#participants with 80% of days in the critical transition period (ct) prior to onset of a depressive episode and 
#at least rw_adjust days in between depressive episodes 
list_names <- unique(episode$Id[which((episode$Critical_Transition >= (ct*percent_complete)) & (episode$Between_Episodes >= rw_adjust))])
episode2 <- episode[which(episode$Id %in% list_names),]

FYP_df_subset <- FYP_df_subset[which(FYP_df_subset$Id %in% unique(episode2$Id)),]
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
#model will not converge for this participant 
id_list <- id_list[which(id_list != 'tW3Urg7JbpIXumhU')]


close_strength <- matrix(nrow = length(id_list),ncol = 13); far_strength <- matrix(nrow = length(id_list),ncol = 13)

number <- 0 
for(handle in id_list) {
  print(handle)
  number <- number + 1
  ex1 <- FYP_df_subset %>% filter(Id == handle) %>% select(Day,negemo,posemo,i,we,
                                                           shehe,they,you,swear,article,negate)

  #depressive episodes with at least rw_adjust days between episodes and ct*percent_complete days in ct period prior to onset 
  ex1_ep <- episode2 %>% filter(Id == handle) %>% filter((Critical_Transition >= (ct*percent_complete)) & Between_Episodes >= rw_adjust)
  ex1_ep <- ex1_ep %>% filter(Between_Episodes == max(Between_Episodes))

  for(i in 1:length(ex1_ep$Start)){
    
    
    #first depressive episode in a time series    
    if(ex1_ep$Start[i] == 0){
      
      #select days that occur between start of series and beginning of current depressive episode 
      ex1.1 <- ex1[which(ex1$Day %in% (ex1_ep$Start[i]):(ex1_ep$End[i]-1)),]
      
      if(dim(ex1.1)[1] >= rw_adjust) {
        
        #period closest to the onset of depressive episode 
        close_period <- ex1.1[(dim(ex1.1)[1]-(ct-1)):dim(ex1.1)[1],]
        far_period <- ex1.1[1:ct,]
        
        close_period <- close_period %>% select(-Day)
        far_period <- far_period %>% select(-Day)

        if(all(apply(close_period, 2, sd) != 0) & all(apply(far_period, 2, sd) != 0)) {
        try(close_net <- graphicalVAR(close_period, nLambda = 10, verbose = TRUE, gamma = 0.1,scale = TRUE, maxit.in = 100,
                                    maxit.out = 100,deleteMissings = TRUE),silent = TRUE)
        
        try(far_net <- graphicalVAR(far_period, nLambda = 10, verbose = TRUE, gamma = 0.1,scale = TRUE, maxit.in = 100,
                                      maxit.out = 100,deleteMissings = TRUE),silent = TRUE)
        
        close_net_PCC <- qgraph(close_net$PCC)
        far_net_PCC <- qgraph(far_net$PCC)
        
        close_indegree <- centrality(close_net_PCC)$OutDegree
        far_indegree <- centrality(far_net_PCC)$OutDegree
        
        close_strength[number,] <- c(close_indegree,'close',handle,i)
        far_strength[number,] <- c(far_indegree,'far',handle,i) 
        
        }
      }
      
      
    }
    
    
    #nth depressive episode, start of non-depressed days begins after a depressive episode     
    if(ex1_ep$Start[i] != 0) {
      
      #select days that occur between end of prior depressive episode and beginning of current depressive episode 
      ex1.1 <- ex1[which(ex1$Day %in% (ex1_ep$Start[i]+1):(ex1_ep$End[i]-1)),]
      
      if(dim(ex1.1)[1] >= rw_adjust){
        #period closest to the onset of depressive episode 
        close_period <- ex1.1[(dim(ex1.1)[1]-(ct-1)):dim(ex1.1)[1],]
        far_period <- ex1.1[1:ct,]
        
        close_period <- close_period %>% select(-Day)
        far_period <- far_period %>% select(-Day)
        
        if(all(apply(close_period, 2, sd) != 0) & all(apply(far_period, 2, sd) != 0)){
        try(close_net <- graphicalVAR(close_period, nLambda = 10, verbose = TRUE, gamma = 0.25,scale = TRUE, maxit.in = 100,
                                      maxit.out = 100,deleteMissings = TRUE),silent = TRUE)
        
        try(far_net <- graphicalVAR(far_period, nLambda = 10, verbose = TRUE, gamma = 0.25,scale = TRUE, maxit.in = 100,
                                    maxit.out = 100,deleteMissings = TRUE),silent = TRUE)
        
        close_net_PCC <- qgraph(close_net$PCC)
        far_net_PCC <- qgraph(far_net$PCC)
        
        close_indegree <- centrality(close_net_PCC)$OutDegree
        far_indegree <- centrality(far_net_PCC)$OutDegree
        
        close_strength[number,] <- c(close_indegree,'close',handle,i)
        far_strength[number,] <- c(far_indegree,'far',handle,i) 
        
        }
        
      }

    }
    
    
  }
  
}

#############################################################################

close_strength2 <- na.omit(close_strength)
far_strength2 <- na.omit(far_strength)

within_network <- as.data.frame(rbind(close_strength2,far_strength2))
colnames(within_network) <- c("negemo","posemo","i","we","shehe","they",
                             "you","swear","article","negate","Period","Id","Episode")
within_network <- within_network[order(within_network$Id),]
within_network[,1:10] <- lapply(within_network[,1:10], function(x) as.numeric(as.character(x)))
within_network$Mean_Strength <- rowMeans(within_network[,1:10])

within_network[,c(1:10,14)] <- remove_all_outliers(within_network[c(1:10,14)])

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

##############################################################

#mean word count
ggplot(data = within_network, aes(x = as.factor(Period),y=Mean_Strength)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("Furthest from Onset","Closest to Onset")) +
  theme(axis.title.x=element_blank())


