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
setwd('/Users/seankelley/Twitter_Depression_Kelley/')

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


#create a 3rd person pronoun category 
mod.negemo <- lmer(negemo ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
mod.posemo <- lmer(posemo ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
mod.i <- lmer(i ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
mod.we <- lmer(we ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
mod.you <- lmer(you ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
mod.pro3 <- lmer(pro3 ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
mod.swear <- lmer(swear ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
mod.article <- lmer(article ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
mod.negate <- lmer(negate ~ Depressed_today +  ( 1 + Depressed_today|Id),data = FYP_df)
#----------------------------------------------------------------------
coeff.df = data.frame(matrix(vector(),9 , 4,
                             dimnames=list(c(), c("LIWC_Feature","Beta","SE","p_val"))),
                      stringsAsFactors=F)

coeff.df[1,1] <- "Articles"; coeff.df[1,2] <- coefficients(summary(mod.article))[2]; coeff.df[1,3] <- coefficients(summary(mod.article))[4]; coeff.df[1,4] <- coefficients(summary(mod.article))[10]
coeff.df[2,1] <- "1st Person Singular"; coeff.df[2,2] <- coefficients(summary(mod.i))[2]; coeff.df[2,3] <- coefficients(summary(mod.i))[4]; coeff.df[2,4] <- coefficients(summary(mod.i))[10]
coeff.df[3,1] <- "Negation"; coeff.df[3,2] <- coefficients(summary(mod.negate))[2]; coeff.df[3,3] <- coefficients(summary(mod.negate))[4]; coeff.df[3,4] <- coefficients(summary(mod.negate))[10]
coeff.df[4,1] <- "Negemo"; coeff.df[4,2] <- coefficients(summary(mod.negemo))[2]; coeff.df[4,3] <- coefficients(summary(mod.negemo))[4]; coeff.df[4,4] <- coefficients(summary(mod.negemo))[10]
coeff.df[5,1] <- "Posemo"; coeff.df[5,2] <- coefficients(summary(mod.posemo))[2]; coeff.df[5,3] <- coefficients(summary(mod.posemo))[4]; coeff.df[5,4] <- coefficients(summary(mod.posemo))[10]
coeff.df[6,1] <- "3rd Person"; coeff.df[6,2] <- coefficients(summary(mod.pro3))[2]; coeff.df[6,3] <- coefficients(summary(mod.pro3))[4]; coeff.df[6,4] <- coefficients(summary(mod.pro3))[10]

coeff.df[7,1] <- "Swear"; coeff.df[7,2] <- coefficients(summary(mod.swear))[2]; coeff.df[7,3] <- coefficients(summary(mod.swear))[4]; coeff.df[7,4] <- coefficients(summary(mod.swear))[10]
coeff.df[8,1] <- "1st Person Plural (we)"; coeff.df[8,2] <- coefficients(summary(mod.we))[2]; coeff.df[8,3] <- coefficients(summary(mod.we))[4]; coeff.df[8,4] <- coefficients(summary(mod.we))[10]
coeff.df[9,1] <- "2nd Person Singular (you)"; coeff.df[9,2] <- coefficients(summary(mod.you))[2]; coeff.df[9,3] <- coefficients(summary(mod.you))[4]; coeff.df[9,4] <- coefficients(summary(mod.you))[10]

#Output dataframe to text file
write.table(coeff.df, file = "Data/Results/all_tweets/TableS3.txt", sep = ";", quote = FALSE, row.names = F)







