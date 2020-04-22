library(dplyr)
library(ggplot2)
library(lmerTest)
library(broom)
library(reshape2)
library(lme4)
library(stringr)
library(roll)
library(zoo)
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

#remove outliers 
weight_WC <- function(x, na.rm = TRUE, ...) {
  m <- max(x)
  m
}
# function to apply to all rows
weight_WC <- function(d){
  d[] <- lapply(d, function(x) if (is.numeric(x))
    weight_WC(x) else x)
  d
}


#############################################################
#############################################################
setwd('D:/Twitter_Depression_Kelley/')

tweet_type = "tweet"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")
path2 = paste0('Data/Sentiments/',tweet_type,"/CSD_Episodes.csv",collapse = "")

#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

episode = read.csv(path2,stringsAsFactors = FALSE)
participants <- read.csv('Data/Participant_Data/FYP_Twitter_Participants.csv')

ct <- 14 #interval to compute variance over  
rw_adjust <- 2*ct  #minimum duration between depressive episodes to have one period prior to onset and one period away from onset 
percent_complete = 0.5

var <- 'negemo' #which sentiment to use 

#############################################################
#############################################################

#keep participants from free recruitment (OCI_6 coded as NA) and those who successfully completed the 
#attention check in clickworker 
participants <- participants[which(is.na(participants$OCI_6) | participants$OCI_6 == 1),]

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

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

handle_list <- list();episodes <- list()
mean_sd <- list(); mean_sd_start <- list()
mean_wc <- list(); mean_wc_start <- list()
mean_var <- list(); mean_var_start <- list()

missing_vals <- list(); 

for(handle in id_list) {
  print(handle)
  
  ex1 <- FYP_df_subset %>% filter(Id == handle)
  ex1[,6:93]= remove_all_outliers(ex1[6:93])
  ex1$Depressed_today <- as.numeric(as.character(ex1$Depressed_today))
  ex1 <- ex1[which(!is.na((ex1[,which(colnames(ex1) == var)]))),]
  
  #depressive episodes with at least rw_adjust days between episodes and ct*percent_complete days in ct period prior to onset 
  ex1_ep <- episode2 %>% filter(Id == handle) %>% filter((Critical_Transition >= (ct*percent_complete)) & Between_Episodes >= rw_adjust)
  
  for(i in 1:length(ex1_ep$Start)){
    
    
    #first depressive episode in a time series    
    if(ex1_ep$Start[i] == 0){
      
      #select days that occur between start of series and beginning of current depressive episode 
      ex1.1 <- ex1.1 <- ex1[which(ex1$Day %in% (ex1_ep$Start[i]):(ex1_ep$End[i]-1)),]
      
      if(length(ex1.1$Day) >= 2*ct) {
        
        resid_series2 <- (ex1.1[,which(colnames(ex1.1) == var)])
        WC_series <- (ex1.1[,which(colnames(ex1.1) == 'WC')])
        
        #period closest to the onset of depressive episode 
        sd.mean <- sd(resid_series2[(length(resid_series2)-(ct-1)):length(resid_series2)])
        var.mean <- mean(na.omit(resid_series2[(length(resid_series2)-(ct-1)):length(resid_series2)]))
        wc.mean <- mean(na.omit(WC_series[(length(WC_series)-(ct-1)):length(WC_series)]))
        
        #period furthest from onset of depressive episode
        sd.mean_start <- sd(resid_series2[1:ct])
        var.mean_start <- mean(na.omit(resid_series2[1:ct]))
        wc.mean_start <- mean(na.omit(WC_series[1:ct]))
        
        mean_sd <- c(mean_sd,sd.mean)
        mean_sd_start <- c(mean_sd_start,sd.mean_start)
        
        mean_wc <- c(mean_wc,wc.mean)
        mean_wc_start <- c(mean_wc_start,wc.mean_start)
        
        mean_var <- c(mean_var,var.mean)
        mean_var_start <- c(mean_var_start,var.mean_start)
        
        episodes <- c(episodes,i)
        handle_list <- c(handle_list,handle)
        
        
      }
      if(length(ex1.1$Day) < 2*ct){
        sd.mean <- NA
        sd.mean_start <- NA
        
        wc.mean <- NA
        wc.mean_start <- NA
        
        var.mean <- NA
        var.mean_start <- NA
        
        mean_sd <- c(mean_sd,sd.mean)
        mean_sd_start <- c(mean_sd_start,sd.mean_start)
        
        mean_wc <- c(mean_wc,wc.mean)
        mean_wc_start <- c(mean_wc_start,wc.mean_start)
        
        mean_var <- c(mean_var,var.mean)
        mean_var_start <- c(mean_var_start,var.mean_start)
        
        episodes <- c(episodes,i)
        handle_list <- c(handle_list,handle)
        
      }
      
      
    }
    
    
    #nth depressive episode, start of non-depressed days begins after a depressive episode     
    if(ex1_ep$Start[i] != 0) {
      
      #select days that occur between end of prior depressive episode and beginning of current depressive episode 
      ex1.1 <- ex1[which(ex1$Day %in% (ex1_ep$Start[i]+1):(ex1_ep$End[i]-1)),]
      
      if(length(ex1.1$Day) >= 2*ct){
        
        resid_series2 <- (ex1.1[,which(colnames(ex1.1) == var)])
        WC_series <- (ex1.1[,which(colnames(ex1.1) == 'WC')])
        
        #period closest to the onset of depressive episode 
        sd.mean <- sd(resid_series2[(length(resid_series2)-(ct-1)):length(resid_series2)])
        var.mean <- mean(na.omit(resid_series2[(length(resid_series2)-(ct-1)):length(resid_series2)]))
        wc.mean <- mean(na.omit(WC_series[(length(WC_series)-(ct-1)):length(WC_series)]))
        
        #period furthest from onset of depressive episode
        sd.mean_start <- sd(resid_series2[1:ct])
        var.mean_start <- mean(na.omit(resid_series2[1:ct]))
        wc.mean_start <- mean(na.omit(WC_series[1:ct]))
        
        mean_sd <- c(mean_sd,sd.mean)
        mean_sd_start <- c(mean_sd_start,sd.mean_start)
        
        mean_wc <- c(mean_wc,wc.mean)
        mean_wc_start <- c(mean_wc_start,wc.mean_start)
        
        mean_var <- c(mean_var,var.mean)
        mean_var_start <- c(mean_var_start,var.mean_start)
        
        episodes <- c(episodes,i)
        handle_list <- c(handle_list,handle)
        
      }
      if(length(ex1.1$Day) < 2*ct){
        sd.mean <- NA
        sd.mean_start <- NA
        
        wc.mean <- NA
        wc.mean_start <- NA
        
        var.mean <- NA
        var.mean_start <- NA
        
        mean_sd <- c(mean_sd,sd.mean)
        mean_sd_start <- c(mean_sd_start,sd.mean_start)
        
        mean_var <- c(mean_var,var.mean)
        mean_var_start <- c(mean_var_start,var.mean_start)
        
        mean_wc <- c(mean_wc,wc.mean)
        mean_wc_start <- c(mean_wc_start,wc.mean_start)
        
        episodes <- c(episodes,i)
        handle_list <- c(handle_list,handle)
        
      }
      
    }
    
    
  }
  
}

#############################################################################
handle_list <- unlist(handle_list)

mean_var <- (unlist(mean_var)); mean_var_start <-(unlist(mean_var_start))
mean_sd <- (unlist(mean_sd)); mean_sd_start <-(unlist(mean_sd_start))
mean_wc <- unlist(mean_wc); mean_wc_start <- unlist(mean_wc_start)

mean_sd_name <- rep(1,length(mean_sd));mean_sd_start_name <- rep(0,length(mean_sd_start))
episodes <- unlist(episodes)

#sentiment values closest to onset of depressive episode 
df1 <- as.data.frame(cbind(mean_var,mean_sd,mean_sd_name,handle_list,episodes,mean_wc))
colnames(df1) <- c("Mean","SD","Time","Id","Episode","WC")
df1 <- df1[!is.na(df1$SD),]

#sentiment values farthest from onset of depressive episode 
df2 <- as.data.frame(cbind(mean_var_start,mean_sd_start,mean_sd_start_name,handle_list,episodes,mean_wc_start))
colnames(df2) <- c("Mean","SD","Time","Id","Episode","WC")
df2 <- df2[!is.na(df2$SD),]

df <- rbind(df1,df2)
df$Mean <- as.numeric(as.character(df$Mean))
df$SD <- as.numeric(as.character(df$SD))
df$WC <- as.numeric(as.character(df$WC))
df$Time <- as.numeric(as.character(df$Time))

summary(glmer(Time ~ SD +(1|Id),data =df,binomial(link = "logit")))
summary(glmer(Time ~ SD + Mean + (1|Id),data =df,binomial(link = "logit")))
summary(glmer(Time ~ SD + Mean + WC +(1|Id),data =df,binomial(link = "logit")))

##############################################################
#Figures
df$Time <-  factor(df$Time, levels = c("0", "1"))

#sentiment mean
ggplot(data = df, aes(x = Time,y=Mean)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("14 days farthest","14 days closest")) +
  theme(axis.title.x=element_blank()) + ggtitle("Sentiment: negemo \nIncludes: Only Likes")


ggplot(data = df, aes(x = Time,y=SD)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("14 days farthest","14 days closest")) +
  theme(axis.title.x=element_blank()) + ggtitle("Sentiment: negemo \nIncludes: Only Likes")


ggplot(data = df, aes(x = Time,y=WC)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("14 days farthest","14 days closest")) +
  theme(axis.title.x=element_blank()) + ggtitle("Sentiment: negemo \nIncludes: Only Likes")


ggplot(data = df,
       mapping = aes(x = Time,
                     y = SD,
                     group = Id)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5)  + geom_smooth(se=FALSE, colour="black", size=2) + theme_bw() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))


ggplot(data = df,
       mapping = aes(x = Time,
                     y = WC,
                     group = Id)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5)  + geom_smooth(se=FALSE, colour="black", size=2) + theme_bw() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))

