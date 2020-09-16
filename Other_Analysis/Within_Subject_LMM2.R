library(dplyr)
library(ggplot2)
library(lmerTest)
library(Hmisc)
library(bootnet)
library(qgraph)
library(NetworkComparisonTest)
set.seed(2020)
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

FYP_df <- read.csv('Data/Sentiments/all_tweets/VADER_ANEW_LIWC_complete_dep.csv')
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'
participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')
dc_ep <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
colnames(dc_ep)[1] <- "Id"
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_ep$Id))
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2
#remove outliers greater or less than 3 sd from the mean 
FYP_df[,c(6:93,96)]= remove_all_outliers(FYP_df[c(6:93,96)])


df <- list()
for(id in unique(FYP_df$Id)){
  print(id)
  en_var <- FYP_df %>% filter(Id == id) %>% select(Id,negemo,posemo,i,we,pro3,you,swear,article,negate,Depressed_today)
  en_var[,2:10] = remove_all_outliers(en_var[2:10])
  df[[id]] <- en_var
}


#create a 3rd person pronoun category 
mod1 <- lmer(negemo ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod1)

mod2 <- lmer(posemo ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod2)

mod3 <- lmer(i ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod3)

mod4 <- lmer(we ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod4)

mod10 <- lmer(you ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod10)

mod5 <- lmer(pro3 ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod5)

mod7 <- lmer(swear ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod7)

mod8 <- lmer(article ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod8)

mod9 <- lmer(negate ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
summary(mod9)



