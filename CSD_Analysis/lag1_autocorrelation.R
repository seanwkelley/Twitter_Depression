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

#rolling window autocorrelation function 
acf.window <- function(y, n) {
  N <- length(y)
  if (n > N) stop("Window too wide.")
  zero <- 0
  #
  # Compute a rolling sum given the fft of its kernel.
  #
  sum.window <- function(x, k) Re(fft(fft(x) * k, inverse=TRUE)) / length(x)
  #
  # Precompute kernels for summing over windows of length `n` and n-1.
  #
  m <- floor((n+1)/2)
  kernel <-  fft(c(rep(1, m), rep(0, N-n+1), rep(1, n-m-1)))
  kernel.full <- fft(c(rep(1, m), rep(0, N-n), rep(1, n-m)))
  #
  # Lag the original data.
  #
  y.lag <- c(y[-1], zero)           # Lagged values
  y.trunc <- c( y[-N], zero)        # Truncated at the end
  #
  # Compute the needed rolling sums.
  #
  y.sum <- sum.window(y, kernel)
  y.lag.sum <- c(y.sum[-1], zero)
  y.trunc.sum <- c(y.sum[-N], zero)
  y.prod <- sum.window(y.lag * y.trunc, kernel)
  y.mean <- sum.window(y, kernel.full) / n
  y.2 <- sum.window(y^2, kernel.full)
  
  a <- y.prod - y.mean*(y.lag.sum+y.trunc.sum) + y.mean^2*(n-1)
  a <- a / (y.2 - n * y.mean^2)
  
  return(a[m:(N-n+m)])
}

acf.reference <- function(y, n, lag=1) {
  require(zoo)
  rollapply(y, width = n, FUN=function(x) acf(x, plot = FALSE, lag.max = lag)$acf[1+lag])
}


#############################################################
#############################################################
setwd('D:/Twitter_Depression_Kelley/')

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")
path2 = paste0('Data/Sentiments/',tweet_type,"/CSD_Episodes.csv",collapse = ""

#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

episode = read.csv(path2,stringsAsFactors = FALSE)
participants <- read.csv('Data/Participant_Data/FYP_Twitter_Participants.csv')

#set values for autocorrelation analysis 
rolling_window = 30 #length of rolling window 
bandwidth_ksmooth <- 7 #bandwidth is for smoothing and removing high-frequency trends 
ct <- 14 #number of days before critical transition  
rw_adjust <- ct + rolling_window #minimum duration of days before a critical transtion to have at least 14 points for autocorrelation trend 
var <- 'negemo' #which sentiment to use 
#refers to using only the 14 day period prior to a critical transition or the entire autocorrelation time series prior 
series_type <- 'ct_partial' #set to either 'ct_complete' or 'ct_partial'


#############################################################
#############################################################

#keep participants from free recruitment (OCI_6 coded as NA) and those who successfully completed the 
#attention check
participants <- participants[which(is.na(participants$OCI_6) | participants$OCI_6 == 1),]

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

#remove participants with either no depressed episodes reported in the past year OR 
#participants with a depressive episode that covers the entire past year
remove_ids <- list()
for(i in 1:length(unique(FYP_df$Id))) {
  print(i)
  if( sd(as.numeric(as.character(FYP_df[which(FYP_df$Id == unique(FYP_df$Id)[i]),94]))) == 0) {
    remove_ids[[i]] <- as.character(unique(FYP_df$Id)[i])
  }
}

remove_ids <- unlist(remove_ids)
FYP_df_subset <- FYP_df[which(FYP_df$Id %!in% remove_ids),]

#remove outliers in any of the sentiments (LIWC and ANEW)
FYP_df_subset[,6:93]= remove_all_outliers(FYP_df_subset[6:93])
FYP_df_subset$Depressed_today <- as.factor(FYP_df_subset$Depressed_today)

######################################################################################
#Participants are included if they have at least 1 depressive with the following characteristics:
#1. Have at least 80% of days in the 44 day period prior to a critical transiton with Tweet days
#2. Have at least 10 days of Tweets within a depressive episode
#3. Have at least 44 days from the end of one depressive episode to the start of another depressive episode 

#remove brackets from participant Id
for(i in 1:length(episode$Id)) {
  episode$Id[i] <- unlist(strsplit(episode$Id[i],"'"))[2]
}

#participants with 
list_names <- unique(episode$Id[which((episode$Critical_Transition >= (rw_adjust*0.8)) & (episode$Depressive_Episode >= 10) & (episode$Between_Episodes >= rw_adjust))])
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

#VZxIkJN3J3lmNR3F- good example of increase in autocorrelation prior to the onset of a depressive episode 
#list of unique participant ids 
id_list <- unique(episode2$Id)


kc_trend <- list(); handle_list <- list(); mean_autocor <- list()
missing_vals <- list()
for(handle in id_list) {
  print(handle)
  
  ex1 <- FYP_df_subset %>% filter(Id == handle)
  ex1$Depressed_today <- as.numeric(as.character(ex1$Depressed_today))
  #how to subset by sufficient number of days with Tweets prior to onset of depression 
  #select only episodes with a sufficient number of days in the 14 day period prior to the onset of depression
  #between separation needs to be at least 14 days + (rolling-window - 1) to ensure all 14 points are estimated from 
  #non-depressed days 
  ex1_ep <- episode2 %>% filter(Id == handle) %>% filter(Critical_Transition >= (rw_adjust*0.8) & Between_Episodes >= rw_adjust)
  

  for(i in 1:length(ex1_ep$Start)){
    
    #first non-depressed day prior to a depressive episode occurs on Day 0  
    #dont exclude the first day in the series 
    if(ex1_ep$Start[i] == 0){
      
      ex1.1 <- ex1.1 <- ex1[which(ex1$Day %in% (ex1_ep$Start[i]):(ex1_ep$End[i]-1)),]
      mv <- length(which(ex1.1$Date == ''))/length(ex1.1$Day) #percentage of values that will need to be interpolated
      
      
      if(mv <= 1) {
      missing_vals <- c(missing_vals,mv)
      #Gaussian Kernel smoothing with a 7 day window 
      var_smooth <- suppressWarnings(ksmooth(ex1.1$Day,na.approx(ex1.1[,which(colnames(ex1.1) == var)]) ,n.points = length(ex1.1$Day), kernel =  "normal", bandwidth = bandwidth_ksmooth))
      
      #remove any trends by subtracting the smoothed series from the original 
      resid_series <- suppressWarnings(na.approx(ex1.1[,which(colnames(ex1.1) == var)]) - var_smooth$y)
      
      #rolling autocorrelation series using a pre-defined rolling window  
      autocor_resid <- acf.window(resid_series,rolling_window)
      
      if(series_type == 'ct_complete'){
      kc <- cor(1:length(autocor_resid),autocor_resid[1:length(autocor_resid)],method = "kendall")
      auto.mean <- mean(autocor_resid[1:length(autocor_resid)])
      }
      if(series_type == 'ct_partial'){
      kc <- cor(1:ct,autocor_resid[(length(autocor_resid)-(ct-1)):length(autocor_resid)],method = "kendall")
      auto.mean <- mean(autocor_resid[(length(autocor_resid)-(ct-1)):length(autocor_resid)])
      }
      kc_trend <- c(kc_trend,kc)
      }
      
      
    }
    
    
    #nth depressive episode, start of non-depressed days begins after another depressive episode
    #both start and end dates correspond to a depressed day and need to be excluded by adjusting range to
    #start + 1 and end -1 
    if(ex1_ep$Start[i] != 0) {
      
      ex1.1 <- ex1[which(ex1$Day %in% (ex1_ep$Start[i]+1):(ex1_ep$End[i]-1)),]
      mv <- length(which(ex1.1$Date == ''))/length(ex1.1$Day)
      
      
      if(mv <= 1){
      missing_vals <- c(missing_vals,mv)
      #Gaussian Kernel smoothing with a 7 day window 
      var_smooth <- suppressWarnings(ksmooth(ex1.1$Day,na.approx(ex1.1[,which(colnames(ex1.1) == var)]) ,n.points = length(ex1.1$Day), kernel =  "normal", bandwidth = bandwidth_ksmooth))
      
      #remove any trends by subtracting the smoothed series from the interpolated version 
      #extrapolate the interpolation to missing endpoints (start/end dates are missing)
      resid_series <- suppressWarnings(na.approx(ex1.1[,which(colnames(ex1.1) == var)],rule = 2) - var_smooth$y)
      
      #rolling autocorrelation series using a pre-defined rolling window  
      autocor_resid <- acf.window(resid_series,rolling_window)
      
      if(series_type == 'ct_complete'){
        kc <- cor(1:length(autocor_resid),autocor_resid[1:length(autocor_resid)],method = "kendall")
        auto.mean <- mean(autocor_resid[1:length(autocor_resid)])
      }
      if(series_type == 'ct_partial'){
        kc <- cor(1:ct,autocor_resid[(length(autocor_resid)-(ct-1)):length(autocor_resid)],method = "kendall")
        auto.mean <- mean(autocor_resid[(length(autocor_resid)-(ct-1)):length(autocor_resid)])
      }
      kc_trend <- c(kc_trend,kc)
      }
      
      
    }
    
    
    #kc_trend <- c(kc_trend,kc)
    mean_autocor <- c(mean_autocor,auto.mean)
    handle_list <- c(handle_list,handle)
    print(kc)
    print(auto.mean)
    
  }
  
}
mean_autocor <- unlist(mean_autocor)
#############################################################################
#Figures 
handle_list <- unlist(handle_list)
#save(kc_trend, file = "Results/autocorrelation_trend_7_partial.RData")
#save(handle_list, file = "Results/autocorrelation_trend_30.RData")
#save(mean_autocor, file = "Results/autocorrelation_mean_30_partial.RData")


kc_trend <- as.data.frame(unlist(kc_trend))
kc_trend$percent_missing <- unlist(missing_vals)*100
colnames(kc_trend) <- c("autocorrelation","percent_missing")

ggplot(kc_trend, aes(x=autocorrelation)) +  geom_histogram(color="black", fill="white", bins = 25)+ xlim(-1, 1)+ 
theme_bw() + ggtitle(" Sentiment: negemo \n Autocorrelation\n Rolling window: 30 days \n Bandwidth: 7 days \n Complete Series") +
xlab("Trend in Lag-1 Autocorrelation (Kendall)") 

mean(kc_trend$autocorrelation); sd(kc_trend$autocorrelation)

ggplot(kc_trend, aes(x=percent_missing,y=autocorrelation)) + geom_point() + 
  theme_bw() + ggtitle(" Sentiment: negemo \n Rolling window: 30 days \n Bandwidth: 7 days \n Complete Series") +
  xlab("") + ylab("Percent Missing Data")


ggplot(kc_trend, aes(x=percent_missing)) +  geom_histogram(color="black", fill="white", bins = 25)+ 
  theme_bw() + ggtitle(" Sentiment: negemo \n Rolling window: 30 days \n Bandwidth: 7 days \n Complete Series") +
  xlab("Percent Missing Days in Series Prior to Depression Onset") 

