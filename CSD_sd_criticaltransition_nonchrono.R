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


'%!in%' <- function(x,y)!('%in%'(x,y))

gaussian_detrend <- function(df,bandwidth_ksmooth,var) {
  sd.detrend <- list()
  for(i in 9:93){
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
participants <- read.csv('Data/Participant_Data/FYP_Twitter_Participants.csv')

ct <-  14 #interval to compute variance over   
percent_complete = 0

var <- 'article' #which sentiment to use 

#############################################################
#############################################################

#keep participants from free recruitment (OCI_6 coded as NA) and those who successfully completed the 
#attention check in clickworker 
participants <- participants[which(is.na(participants$OCI_6) | participants$OCI_6 == 1),]

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

FYP_df_subset <- FYP_df
FYP_df_subset$Depressed_today <- as.factor(FYP_df_subset$Depressed_today)

######################################################################################
#Participants are included if they have at least 1 depressive with the following characteristics:
#1. Have at least 50% of days in the 14 day period prior to a critical transiton with Tweet days

#participants with 80% of days in the critical transition period (ct) prior to onset of a depressive episode and 
list_names <- unique(episode$Id[which(episode$Critical_Transition >= ct*percent_complete)])
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
full_analysis <- list()

kc_trend <- list(); handle_list <- list()
mean_sd <- list(); mean_sd_comp <- list()
mean_wc <- list(); mean_wc_comp <- list()
mean_sentiment <- list(); mean_sentiment_comp <- list()

episodes <- list()

num_days <- list()


for(handle in id_list) {
  print(handle)
  
  ex1 <- FYP_df_subset %>% filter(Id == handle)
  
  ex1 <- gaussian_detrend(ex1,bandwidth_ksmooth = 7)
  
  ex1$Depressed_today <- as.numeric(as.character(ex1$Depressed_today))
  ex1 <- ex1[which(!is.na((ex1[,which(colnames(ex1) == var)]))),]
  
  #select days that are not in a depressive episode or in the critical transition period 
  #prior to the onset of a depressive episode 
  nondep_episode = ex1 %>% filter(Depressed_today == 0 & critical_period == 0)
  
  #depressive episodes with ct*percent_complete days in ct period prior to onset 
  ex1_ep <- episode2 %>% filter(Id == handle) %>% filter(Critical_Transition >= ct*percent_complete)
  

  for(i in 1:length(ex1_ep$Start)){
    
    
    #first depressive episode in a time series    
    if(ex1_ep$Start[i] == 0){
      
      ex1.1 <- ex1[which(ex1$Day %in% (ex1_ep$Start[i]):(ex1_ep$End[i]-1)),]
      
      #length of time-series excluding times when depressed or critical transition period prior to depression onset
      #needs to be at least ct days long 
      if(length(nondep_episode$Day) >= ct & length(ex1.1$Day) >= ct) {
        
        #if the start of the depressive episode is less than the max of the nondep series then the 
        #period that is furthest away is the max day
        if(ex1_ep$Start[i] < max(nondep_episode$Day)){
        
        #dataframe with variable of interest + Word count + days in critical period   
        var_series <- (ex1.1[,c(which(colnames(ex1.1) == var),8,95)])
        
        #sentiment variable of interest
        var_series2 <- var_series  %>% select(var)
        var_series2 <- (var_series2[,which(colnames(var_series2) == var)])
  
        
        resid_nondep_episode <- nondep_episode %>% select(c(var,WC))
        resid_nondep_episode_var <-   resid_nondep_episode[,which(colnames(resid_nondep_episode) == var)]


        WC_series <- var_series %>% select(WC)
        WC_series <- WC_series$WC
        WC_nondep_episode <- resid_nondep_episode[,which(colnames(resid_nondep_episode) == "WC")]
        
        
        sd.series <- sd(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))
        sentiment.mean <- mean(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))
        wc.mean <- mean(na.omit(WC_series[(length(WC_series)-(ct-1)):length(WC_series)]))
        
        sd.comp <- sd(na.omit(resid_nondep_episode_var[(length(resid_nondep_episode_var)-(ct-1)):length(resid_nondep_episode_var)]))
        sentiment.comp <- mean(na.omit(resid_nondep_episode_var[(length(resid_nondep_episode_var)-(ct-1)):length(resid_nondep_episode_var)]))
        wc.comp <- mean(na.omit(WC_nondep_episode[(length(WC_nondep_episode)-(ct-1)):length(WC_nondep_episode)]))
        
        
        mean_sd <- c(mean_sd,sd.series)
        mean_wc <- c(mean_wc,wc.mean)
        mean_sentiment <- c(mean_sentiment,sentiment.mean)
        
        mean_sd_comp <- c(mean_sd_comp,sd.comp)
        mean_wc_comp <- c(mean_wc_comp,wc.comp)
        mean_sentiment_comp <- c(mean_sentiment_comp,sentiment.comp)
        
        episodes <- c(episodes,i)
        handle_list <- c(handle_list,handle)
        
        
        }
        #if the start of the depressive episode is greater than the max of the nondep series then the 
        #period that is furthest away is the min day 
      if(ex1_ep$Start[i] > max(nondep_episode$Day)) {
        
        #dataframe with variable of interest + Word count + days in critical period   
        var_series <- (ex1.1[,c(which(colnames(ex1.1) == var),8,95)])
        
        var_series2 <- var_series %>% select(var)
        var_series2 <- (var_series2[,which(colnames(var_series2) == var)])
        
        resid_nondep_episode <- nondep_episode %>% select(c(var,WC))
        resid_nondep_episode_var <-   resid_nondep_episode[,which(colnames(resid_nondep_episode) == var)]


        WC_series <- var_series %>% select(WC)
        WC_series <- WC_series$WC
        WC_nondep_episode <- resid_nondep_episode[,which(colnames(resid_nondep_episode) == "WC")]
        
        
        sd.series <- sd(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))
        sd.series <- acf(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))$acf[2]
        sentiment.mean <- mean(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))
        wc.mean <- mean(na.omit(WC_series[(length(WC_series)-(ct-1)):length(WC_series)]))
        
        #comparsion interval (furthest from onset of depression), min day: min day +14
        sd.comp <- sd(na.omit(resid_nondep_episode_var[1:ct]))
        sentiment.comp <- mean(na.omit(resid_nondep_episode_var[1:ct]))
        wc.comp <- mean(na.omit(WC_nondep_episode[1:ct]))
        
        
        mean_sd <- c(mean_sd,sd.series)
        mean_wc <- c(mean_wc,wc.mean)
        mean_sentiment <- c(mean_sentiment,sentiment.mean)
        
        mean_sd_comp <- c(mean_sd_comp,sd.comp)
        mean_wc_comp <- c(mean_wc_comp,wc.comp)
        mean_sentiment_comp <- c(mean_sentiment_comp,sentiment.comp)
        
        episodes <- c(episodes,i)
        handle_list <- c(handle_list,handle)

      
      }
    }
      if(length(nondep_episode$Day) < ct | length(ex1.1$Day) < ct){
        sd.series <- NA
        sd.comp <- NA
        wc.mean <- NA
        wc.comp <- NA
        sentiment.mean <- NA
        sentiment.comp <- NA

        mean_sd <- c(mean_sd,sd.series)
        mean_sd_comp <- c(mean_sd_comp,sd.comp)
        
        mean_sentiment <- c(mean_sentiment,sentiment.mean)
        mean_sentiment_comp <- c(mean_sentiment_comp,sentiment.comp)
        
        mean_wc <- c(mean_wc,wc.mean)
        mean_wc_comp <- c(mean_wc_comp,wc.comp)
 
        episodes <- c(episodes,i)
        handle_list <- c(handle_list,handle)
        
      }
      
      
    }
    
    
    #nth depressive episode, start of non-depressed days begins after a depressive episode     
    if(ex1_ep$Start[i] != 0) {
      
      ex1.1 <- ex1[which(ex1$Day %in% (ex1_ep$Start[i]+1):(ex1_ep$End[i]-1)),]
      
      if(length(nondep_episode$Day) >= ct & length(ex1.1$Day) >= ct) {
        if(ex1_ep$Start[i] < max(nondep_episode$Day)){
          
          #variable of interest + Word count + days in critical period   
          var_series <- (ex1.1[,c(which(colnames(ex1.1) == var),8,95)])
          
          var_series2 <- var_series %>% select(var)
          var_series2 <- (var_series2[,which(colnames(var_series2) == var)])
          
          resid_nondep_episode <- nondep_episode %>% select(c(var,WC))
          resid_nondep_episode_var <-   resid_nondep_episode[,which(colnames(resid_nondep_episode) == var)]


          #mean word count for during the critical transition period 
          WC_series <- var_series %>% select(WC)
          WC_series <- WC_series$WC
          WC_nondep_episode <- resid_nondep_episode[,which(colnames(resid_nondep_episode) == "WC")]
          
          #sd and wc mean
          sd.series <- sd(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))
          sentiment.mean <- mean(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))
          wc.mean <- mean(na.omit(WC_series[(length(WC_series)-(ct-1)):length(WC_series)]))
          
          #comparsion interval (furthest from onset of depression), max day - ct: max day 
          sd.comp <- sd(na.omit(resid_nondep_episode_var[(length(resid_nondep_episode_var)-(ct-1)):length(resid_nondep_episode_var)]))
          sentiment.comp <- mean(na.omit(resid_nondep_episode_var[(length(resid_nondep_episode_var)-(ct-1)):length(resid_nondep_episode_var)]))
          wc.comp <- mean(na.omit(WC_nondep_episode[(length(WC_nondep_episode)-(ct-1)):length(WC_nondep_episode)]))
          
          
          mean_sd <- c(mean_sd,sd.series)
          mean_wc <- c(mean_wc,wc.mean)
          
          mean_sentiment <- c(mean_sentiment,sentiment.mean)
          mean_sentiment_comp <- c(mean_sentiment_comp,sentiment.comp)
          
          mean_sd_comp <- c(mean_sd_comp,sd.comp)
          mean_wc_comp <- c(mean_wc_comp,wc.comp)
          
          episodes <- c(episodes,i)
          handle_list <- c(handle_list,handle)


        }
        if(ex1_ep$Start[i] > max(nondep_episode$Day)) {
          
          #variable of interest + Word count + days in critical period   
          var_series <- (ex1.1[,c(which(colnames(ex1.1) == var),8,95)])
          
          var_series2 <- var_series %>% filter(critical_period == 1) %>% select(var)
          var_series2 <- (var_series2[,which(colnames(var_series2) == var)])
          
          resid_nondep_episode <- nondep_episode %>% select(c(var,WC))
          resid_nondep_episode_var <-   resid_nondep_episode[,which(colnames(resid_nondep_episode) == var)]

          
          WC_series <- var_series %>% select(WC)
          WC_series <- WC_series$WC
          WC_nondep_episode <- resid_nondep_episode[,which(colnames(resid_nondep_episode) == "WC")]
          
          
          sd.series <- sd(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))
          sentiment.mean <- mean(na.omit(var_series2[(length(var_series2)-(ct-1)):length(var_series2)]))
          wc.mean <- mean(na.omit(WC_series[(length(WC_series)-(ct-1)):length(WC_series)]))
          
          #comparsion interval (furthest from onset of depression), min day: min day +14
          sd.comp <- sd(na.omit(resid_nondep_episode_var[1:ct]))
          sentiment.comp <- mean(na.omit(resid_nondep_episode_var[1:ct]))
          wc.comp <- mean(na.omit(WC_nondep_episode[1:ct]))
          
          
          mean_sd <- c(mean_sd,sd.series)
          mean_wc <- c(mean_wc,wc.mean)
          
          mean_sd_comp <- c(mean_sd_comp,sd.comp)
          mean_wc_comp <- c(mean_wc_comp,wc.comp)
          
          mean_sentiment <- c(mean_sentiment,sentiment.mean)
          mean_sentiment_comp <- c(mean_sentiment_comp,sentiment.comp)
          
          episodes <- c(episodes,i)
          handle_list <- c(handle_list,handle)


        }
      }
      if(length(nondep_episode$Day) < ct | length(ex1.1$Day) < ct){
        sd.series <- NA
        sd.comp <- NA
        
        wc.mean <- NA
        wc.comp <- NA
        
        mean_sd <- c(mean_sd,sd.series)
        mean_sd_comp <- c(mean_sd_comp,sd.comp)
        
        mean_wc <- c(mean_wc,wc.mean)
        mean_wc_comp <- c(mean_wc_comp,wc.comp)
        
        mean_sentiment <- c(mean_sentiment,sentiment.mean)
        mean_sentiment_comp <- c(mean_sentiment_comp,sentiment.comp)
        
        episodes <- c(episodes,i)
        handle_list <- c(handle_list,handle)
        
      }
      
      
      
    }
    
    
  }
  
}

#############################################################################
#Figures 
handle_list <- unlist(handle_list)

mean_sd <- (unlist(mean_sd)); mean_sd_comp <-(unlist(mean_sd_comp))
mean_sentiment <- unlist(mean_sentiment); mean_sentiment_comp <- unlist(mean_sentiment_comp)

mean_wc <- unlist(mean_wc); mean_wc_comp <- unlist(mean_wc_comp)

mean_sd_name <- rep(1,length(mean_sd));mean_sd_comp_name <- rep(0,length(mean_sd_comp))
episodes <- unlist(episodes)

#sentiment values closest to onset of depressive episode 
df1 <- as.data.frame(cbind(mean_sentiment,mean_sd,mean_sd_name,handle_list,episodes,mean_wc))
colnames(df1) <- c("Mean","SD","Time","Id","Episode","WC")
df1 <- df1[!is.na(df1$SD),]

#sentiment values farthest from onset of depressive episode 
df2 <- as.data.frame(cbind(mean_sentiment_comp,mean_sd_comp,mean_sd_comp_name,handle_list,episodes,mean_wc_comp))
colnames(df2) <- c("Mean","SD","Time","Id","Episode","WC")
df2 <- df2[!is.na(df2$SD),]

df <- rbind(df1,df2)
df$Mean <- as.numeric(as.character(df$Mean))
df$SD <- as.numeric(as.character(df$SD))
df$WC <- as.numeric(as.character(df$WC))
df$Time <- as.numeric(as.character(df$Time))

write.csv(df,file = "Data/Results/sd_ct.csv")

summary(glmer(Time ~ SD +(1|Id),data =df,binomial(link = "logit")))
summary(glmer(Time ~ WC +(1|Id),data =df,binomial(link = "logit")))

summary(glmer(Time ~ SD + WC + (1|Id),data =df,binomial(link = "logit")))

########################################################################
#Figures

#mean standard deviation
ggplot(data = df, aes(x = as.factor(Time),y=SD)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("14 days farthest","14 days closest")) +
  theme(axis.title.x=element_blank()) + ggtitle("Sentiment: i \nIncludes: Only Tweets")

#mean word count
ggplot(data = df, aes(x = as.factor(Time),y=WC)) + geom_boxplot() +
  theme_bw() + theme(axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("14 days farthest","14 days closest")) +
  theme(axis.title.x=element_blank()) + ggtitle("Sentiment: i \nIncludes: Only Tweets")
