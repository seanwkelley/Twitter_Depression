#----------------------------------------------
#Table S5: Bivariate correlations for all LIWC features and depression severity in full sample
#----------------------------------------------

library(dplyr)
library(ggplot2)
library(lmerTest)
library(Hmisc)
library(bootnet)
library(qgraph)
library(NetworkComparisonTest)
set.seed(2020)

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


tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")
participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')


FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
dc_all <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
colnames(dc_all)[1] <- "Id"

#filter to only include participants with >= 30 days of Tweets 
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_all$Id))


FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2

FYP_df <- FYP_df[,c(3,8:94,96)]
FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")
FYP_df_mean[,c(2:87,89)]= remove_all_outliers(FYP_df_mean[c(2:87,89)])
FYP_df_mean[,c(2:87,89)] = scale(FYP_df_mean[,c(2:87,89)])
FYP <- merge(participants,FYP_df_mean,by='Id')


#recode categorical variables as factors 
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
summary(glm(pro3 ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(you ~ Depression_zscore, family = "gaussian",data = FYP))

#----------------------------------------------------------------------
correlation_depression <- FYP %>% dplyr::select(colnames(FYP)[c(34:119,121)],Depression_zscore)

coeff.df = data.frame(matrix(vector(),87 , 4,
                             dimnames=list(c(), c("Beta","lwr.ci","upr.ci","p_val"))),
                      stringsAsFactors=F)

for(i in 1:length(colnames(correlation_depression)[1:87])){
  
  mod1 <- cor.test(correlation_depression[,i],correlation_depression[,"Depression_zscore"])
  coeff.df[i,1] <- mod1$estimate
  coeff.df[i,2] <- as.numeric(mod1$conf.int)[1]
  coeff.df[i,3] <- as.numeric(mod1$conf.int)[2]
  coeff.df[i,4] <- mod1$p.value
  
  
}
coeff.df$Text_Feature <- colnames(correlation_depression)[1:87]
coeff.df <- coeff.df %>% dplyr::select(Text_Feature,Beta, lwr.ci,upr.ci, p_val)
coeff.df[,2:5] <- apply(coeff.df[,2:5],2,function(x) as.numeric(as.character(x)))
coeff.df[,2:5] <- apply(coeff.df[,2:5],2,function(x) round(x,digits = 3))
coeff.df$CI <- paste0(coeff.df$lwr.ci,", ", coeff.df$upr.ci)
coeff.df <- coeff.df %>% dplyr::select(Text_Feature,Beta, CI, p_val)


write.table(coeff.df, file = "Data/Results/all_tweets/TableS5.txt", sep = ";", quote = FALSE, row.names = F)