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
library(Hmisc)
library(corrplot)
library(here)

set.seed(2002)
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
here()

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")
participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
dc_all <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
colnames(dc_all)[1] <- "Id"
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_all$Id))

FYP_df <- FYP_df[,c(3,8:94)]
FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")
FYP_df_mean[,2:87]= remove_all_outliers(FYP_df_mean[2:87])
FYP_df_mean[,2:87] = scale(FYP_df_mean[,2:87])
FYP <- merge(participants,FYP_df_mean,by='Id')

correlation_depression <- FYP %>% select(colnames(FYP)[34:119],Depression_zscore)
correlation_depression_mat <- rcorr(as.matrix(correlation_depression), type = "pearson")
dep.net.var <- names(which(correlation_depression_mat$P[,87] <= 0.05))
nodep.net.var <- names(which(correlation_depression_mat$P[,87] > 0.05))

corrplot(correlation_depression_mat$r, order="hclust", 
         p.mat = correlation_depression_mat$P, sig.level = 0.05, insig = "blank")

qgraph(correlation_depression_mat$r,graph = "cor")

tiff("Figures/correlation_plot.tiff", units="cm", width=40, height=40, res=600)

corrplot(correlation_depression_mat$r, order="hclust", 
         p.mat = correlation_depression_mat$P, sig.level = 0.05, insig = "blank")

dev.off()

tiff("Figures/correlation_plot2.tiff", units="cm", width=40, height=40, res=600)

qgraph(correlation_depression_mat$r,graph = "cor")

dev.off()

