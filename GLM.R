library(dplyr)
library(ggplot2)
library(lmerTest)
library(Hmisc)

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



participants <- read.csv('Data/Participant_Data/FYP_Twitter_Participants.csv')
participants <- participants[which(is.na(participants$OCI_6) | participants$OCI_6 == 1),]

#average sentiments over the past year 
FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")

#remove outliers greater or less than 3 sd from the mean 
FYP_df_mean[,6:93]= remove_all_outliers(FYP_df_mean[6:93])

#create a 3rd person pronoun category 
FYP_df_mean$pro3 <- (FYP_df_mean$shehe + FYP_df_mean$they)/2


#merge sentiments and participants data 
FYP <- merge(participants,FYP_df_mean,by='Id')

#reverse scored items: 2, 5, 6, 11, 12, 14, 16, 17, 18, 20  
FYP$SDS_2 <- 5 - FYP$SDS_2
FYP$SDS_5 <- 5 - FYP$SDS_5
FYP$SDS_6 <- 5 - FYP$SDS_6
FYP$SDS_11 <- 5 - FYP$SDS_11
FYP$SDS_12 <- 5 - FYP$SDS_12
FYP$SDS_14 <- 5 - FYP$SDS_14
FYP$SDS_16 <- 5 - FYP$SDS_16
FYP$SDS_17 <- 5 - FYP$SDS_17
FYP$SDS_18 <- 5 - FYP$SDS_18
FYP$SDS_20 <- 5 - FYP$SDS_20

#SDS: Score system
#Below 50 is considered normal range - 502 people (54.5%)
#50-59 is mild depression - 259 people (28.1%)
#60-69 is moderate depression - 138 people (14.9%)
#above 70 is severe depression - 24 people (2.9%)

FYP$SDS_Total <- rowSums(FYP %>% select(colnames(FYP)[grepl("SDS",colnames(FYP))]))
FYP$Dep_ep_pastyear <- as.factor(FYP$Dep_ep_pastyear)
FYP$Depression_Physician <- as.factor(FYP$Depression_Physician)

##################################################################
#between-subjects regression of SDS total score and de choudhury variables 

#Betas (p-value) of de Choudhury variables 
summary(glm(negemo ~ SDS_Total, family = "gaussian",data = FYP))
summary(glm(posemo ~ SDS_Total, family = "gaussian",data = FYP))
summary(glm(i ~ SDS_Total, family = "gaussian",data = FYP))
summary(glm(we ~ SDS_Total, family = "gaussian",data = FYP))
summary(glm(swear ~ SDS_Total, family = "gaussian",data = FYP))
summary(glm(negate ~ SDS_Total, family = "gaussian",data = FYP))
summary(glm(article ~ SDS_Total, family = "gaussian",data = FYP))


#ANEW variables - dominance and arousal
summary(glm(dominance ~ SDS_Total, family = "gaussian",data = FYP))
summary(glm(arousal ~ SDS_Total, family = "gaussian",data = FYP))

##################################################################
#Elastic Net Regression 

#LIWC features for elasticnet regression 

write.csv(FYP[,290:378], file = "elasticNet/liwc_features.csv",
          row.names = FALSE)

#SDS total score 
sds_target <- as.data.frame(FYP[,379])
colnames(sds_target) <- "SDS_Total"

write.csv(sds_target, file = "elasticNet/sds_target.csv",
          row.names = FALSE)


###################################################################################
#Figures 

#Histogram of age
ggplot(FYP, aes(x=Age)) + geom_histogram(bins=25,color="black", fill="white") + theme_bw() + 
  ggtitle("Age Distribution") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))

#histogram of SDS total score 
ggplot(FYP, aes(x=SDS_Total)) + geom_histogram(bins=25,color="black", fill="white") + theme_bw() +
  ggtitle("Depression Distribution") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))


#boxplot of between-subject negemo for participants with/without a depressive episode in the past year
ggplot(FYP, aes(x=Dep_ep_pastyear, y=negemo)) + 
  geom_boxplot(notch = TRUE)  + theme_bw() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))