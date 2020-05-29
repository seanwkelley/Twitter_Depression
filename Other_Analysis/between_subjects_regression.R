library(dplyr)
library(ggplot2)
library(lmerTest)
library(Hmisc)
library(bootnet)
library(qgraph)
library(NetworkComparisonTest)
#Sentiment analysis averaged over past year 
#Tweets, retweets, and likes 
setwd('D:/Twitter_Depression_Kelley/')

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
#sentiment analysis based on tweets 

FYP_df <- read.csv('Data/Sentiments/all_tweets/VADER_ANEW_LIWC_complete.csv')
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

#average sentiments over the past year 
FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")

#remove outliers greater or less than 3 sd from the mean 
FYP_df_mean[,6:93]= remove_all_outliers(FYP_df_mean[6:93])

#create a 3rd person pronoun category 
FYP_df_mean$pro3 <- (FYP_df_mean$shehe + FYP_df_mean$they)/2


#merge sentiments and participants data 
FYP <- merge(participants,FYP_df_mean,by='Id')

FYP$Dep_ep_pastyear <- as.factor(FYP$Dep_ep_pastyear)
FYP$Gender <- as.factor(FYP$Gender)
FYP$Education <- as.factor(FYP$Education)
FYP$Employment <- as.factor(FYP$Employment)
##################################################################
#between-subjects regression of depression z-score and de choudhury variables 

#Betas (p-value) of de Choudhury variables 
summary(glm(negemo ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(posemo ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(i ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(we ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(swear ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(negate ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(article ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(they ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(shehe ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(you ~ Depression_zscore, family = "gaussian",data = FYP))

##################################################################

summary(glm(WC ~ Dep_ep_pastyear + Age + Gender + Education + Employment, family = "gaussian",data = FYP))




