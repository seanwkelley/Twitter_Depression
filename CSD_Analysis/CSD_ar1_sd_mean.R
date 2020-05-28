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


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/FYP_Twitter_Participants.csv')

#set values for autocorrelation analysis 
rolling_window = 30 #length of rolling window 
bandwidth_ksmooth <- 7 #bandwidth is for smoothing and removing high-frequency trends 
var <- 'i' #which sentiment to use 



#############################################################
#############################################################

#keep participants from free recruitment (OCI_6 coded as NA) and those who successfully completed the 
#attention check
participants <- participants[which(is.na(participants$OCI_6) | participants$OCI_6 == 1),]

#reverse scored items: 2, 5, 6, 11, 12, 14, 16, 17, 18, 20  
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

participants$SDS_Total <- rowSums(participants %>% select(colnames(participants)[grepl("SDS",colnames(participants))]))

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

#remove outliers in any of the sentiments (LIWC and ANEW)
FYP_df[,6:93]= remove_all_outliers(FYP_df[6:93])
FYP_df$Depressed_today <- as.factor(FYP_df$Depressed_today)

#############################################################
#############################################################

dep_ids <- list()
for(i in 1:length(unique(FYP_df$Id))) {
  print(i)
  if( sd(as.numeric(as.character(FYP_df[which(FYP_df$Id == unique(FYP_df$Id)[i]),94]))) != 0) {
    dep_ids[[i]] <- as.character(unique(FYP_df$Id)[i])
  }
  
}

dep_ids <- unlist(dep_ids)
nodep_ids <- (participants %>% filter(Dep_ep_pastyear== 0) %>% select(Id))

participants <- participants %>% filter(Id %in% dep_ids | Id %in% nodep_ids$Id)

FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

#############################################################################
#############################################################################

#VZxIkJN3J3lmNR3F- good example of increase in autocorrelation prior to the onset of a depressive episode 
#list of unique participant ids 

id_list <- unique(FYP_df$Id)


kc_trend <- list(); handle_list <- list(); mean_autocor <- list(); mean_sd <- list(); mean_sentiment <-list()
mean_wc <- list()
missing_vals <- list()
for(handle in id_list) {
  
  print(handle)
  
  ex1 <- FYP_df %>% filter(Id == handle)
  #ex1 <- FYP_df_subset %>% filter(Id == handle)
  ex1$Depressed_today <- as.numeric(as.character(ex1$Depressed_today))
  mv <- length(which(ex1$Date == ''))/length(ex1$Day) #percentage of values that will need to be interpolated
  
  if(length(ex1$Day) >= 2*rolling_window) {
    missing_vals <- c(missing_vals,mv)
    #Gaussian Kernel smoothing with a 7 day window 
    var_smooth <- suppressWarnings(ksmooth(ex1$Day,na.approx(ex1[,which(colnames(ex1) == var)]) ,n.points = length(ex1$Day), kernel =  "normal", bandwidth = bandwidth_ksmooth))
    
    #remove any trends by subtracting the smoothed series from the original 
    resid_series <- suppressWarnings(na.approx(ex1[,which(colnames(ex1) == var)]) - var_smooth$y)
    resid_series2 <- na.omit(ex1[,which(colnames(ex1) == var)])
    
    #rolling autocorrelation series using a pre-defined rolling window  
    autocor_resid <- acf.reference(resid_series,rolling_window,1)
    #autocor_resid <- acf.reference(resid_series2,length(resid_series2),1)
    
    sd_resid <- sd(na.omit(resid_series))
    
    wc.mean <- mean(na.omit(ex1$WC))
    auto.mean <- mean(autocor_resid)
    sd.mean <- mean(sd_resid)
    sentiment_mean <- mean(na.omit(ex1[,which(colnames(ex1) == var)]))
    
    mean_sentiment <- c(mean_sentiment,sentiment_mean)
    mean_autocor <- c(mean_autocor,auto.mean)
    mean_sd <- c(mean_sd,sd.mean)
    mean_wc <- c(mean_wc,wc.mean)
    handle_list <- c(handle_list,handle)
    
  }
  
}

mean_autocor <- unlist(mean_autocor)
mean_sd <- unlist(mean_sd)
handle_list <- unlist(handle_list)
mean_sentiment <- unlist(mean_sentiment)
missing_vals <- unlist(missing_vals)
mean_wc <- unlist(mean_wc)

df <- as.data.frame(cbind(handle_list,mean_autocor,mean_sd,mean_sentiment,mean_wc,missing_vals))
colnames(df) <- c("Id","Autocor","SD","Sentiment","WC","Percent_Missing")
df$Autocor <- as.numeric(as.character(df$Autocor))
df$SD <- as.numeric(as.character(df$SD))
df$Sentiment <- as.numeric(as.character(df$Sentiment))
df$WC <- as.numeric(as.character(df$WC))
df$Percent_Missing <- as.numeric(as.character(df$Percent_Missing))
df$SDS_Total <- participants$SDS_Total[which(participants$Id %in% df$Id)]
df$Dep_Episode <- participants$Dep_ep_pastyear[which(participants$Id %in% df$Id)]
df$Dep_Episode <- as.factor(df$Dep_Episode)

###############################################################################
df <- df %>% filter(Percent_Missing < 0.2)
###############################################################################

summary(glm(Dep_Episode ~ SD, family = binomial(link="logit"),data = df))
summary(glm(Dep_Episode ~ SD + WC, family = binomial(link="logit"),data = df))





summary(glm(Dep_Episode ~ SDS_Total, family = binomial(link="logit"),data = df))
summary(glm(Dep_Episode ~ Autocor, family = binomial(link="logit"),data = df))

summary(glm(Dep_Episode ~ Percent_Missing, family = binomial(link="logit"),data = df))

summary(glm(Sentiment ~ SDS_Total, family = gaussian(),data = df))
summary(glm(SD ~ SDS_Total, family = gaussian(),data = df))

ggplot(data = df, aes(x = Percent_Missing,y=Autocor)) + geom_point() + theme_bw()
ggplot(data = df, aes(x = SDS_Total,y=Autocor)) + geom_point() + theme_bw()

ggplot(data = df, aes(x = Dep_Episode,y=Autocor)) + geom_boxplot() + theme_bw()
ggplot(data = df, aes(x = Dep_Episode,y=SD)) + geom_boxplot() + theme_bw()
ggplot(data = df, aes(x = Dep_Episode,y=Percent_Missing)) + geom_boxplot() + theme_bw() 