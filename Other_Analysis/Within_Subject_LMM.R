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


#############################################################
#############################################################
setwd('D:/Twitter_Depression_Kelley/')

#sentiment analysis based on tweets, retweets, and likes  
tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")
path2 = paste0('Data/Sentiments/',tweet_type,"/CSD_Episodes.csv",collapse = "")

#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

episode = read.csv(path2,stringsAsFactors = FALSE)
participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

episode <- episode %>% filter(Depressive_Episode >= 0)
#############################################################
#keep participants from free recruitment (OCI_6 coded as NA) and those who successfully completed the 
#attention check


FYP_df$Depressed_today <- as.factor(FYP_df$Depressed_today)
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
FYP_df <- FYP_df %>% filter(Id %in% episode$Id)
######################################################################################
#Participants are included if they have at least 1 depressive with the following characteristics:
#1. Have at least 50% of days in the 14 day period prior to a critical transiton with Tweet days
#2. Have at least 10 days of Tweets within a depressive episode

#participants with 
#remove outliers in any of the sentiments (LIWC and ANEW) within-subject 
FYP_df[,6:93] <- remove_all_outliers(FYP_df[6:93])
#############################################################################

#linear mixed model with random slopes and random intercepts 
#Within-subject effect of depression on negative emotions 
fm <- lmer(negemo ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(fm)

fm <- lmer(posemo ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(fm)

fm <- lmer(shehe ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df_subset)
summary(fm)
#############################################################################
#Figures - boxplot and spaghetti plot 

with_sub <- aggregate(negemo ~ Depressed_today + Id,data = FYP_df,FUN = mean)

#spaghetti plots of differences in negemo during and off a depressive episode 
ggplot(data = with_sub,
       mapping = aes(x = Depressed_today,
                     y = negemo,
                     group = Id)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5)  + geom_smooth(se=FALSE, colour="black", size=2) + theme_bw() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))


#boxplot of within-subject negemo during and off a depressive episode 
ggplot(with_sub, aes(x=Depressed_today, y=negemo)) + 
  geom_boxplot(notch = TRUE) +  geom_jitter(shape=16, position=position_jitter(0.2)) + theme_bw() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))
