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
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2

FYP_df <- FYP_df[,c(3,8:94,96)]
FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")
FYP_df_mean[,c(2:87,89)]= remove_all_outliers(FYP_df_mean[c(2:87,89)])
FYP_df_mean[,c(2:87,89)] = scale(FYP_df_mean[,c(2:87,89)])
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
summary(glm(pro3 ~ Depression_zscore, family = "gaussian",data = FYP))
summary(glm(you ~ Depression_zscore, family = "gaussian",data = FYP))

#----------------------------------------------------------------------
correlation_depression <- FYP %>% select(colnames(FYP)[c(34:119,121)],Depression_zscore)

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
coeff.df <- coeff.df %>% select(Text_Feature,Beta, lwr.ci,upr.ci, p_val)
coeff.df[,2:5] <- apply(coeff.df[,2:5],2,function(x) as.numeric(as.character(x)))
coeff.df[,2:5] <- apply(coeff.df[,2:5],2,function(x) round(x,digits = 3))
coeff.df$CI <- paste0(coeff.df$lwr.ci,", ", coeff.df$upr.ci)
coeff.df <- coeff.df %>% select(Text_Feature,Beta, CI, p_val)
write.table(coeff.df, file = "Data/Results/all_tweets/sumstats2.txt", sep = ";", quote = FALSE, row.names = F)
#----------------------------------------------------------------------
mean(na.omit(FYP$WC)); sd(na.omit(FYP$WC))
as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
  summarise(WC = mean(WC, na.rm = TRUE)))

as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
  summarise(WC = sd(WC, na.rm = TRUE)))


summary(glm(WC ~ Dep_ep_pastyear, family = "gaussian",data = FYP))

mean(FYP$Age); sd(FYP$Age)
FYP %>% group_by(Dep_ep_pastyear) %>%
  summarise(Age = mean(Age, na.rm = TRUE))

FYP %>% group_by(Dep_ep_pastyear) %>%
  summarise(Age = sd(Age, na.rm = TRUE))

summary(glm(Age ~ Dep_ep_pastyear, family = "gaussian",data = FYP))

#Gender
table(FYP$Gender)
(table(FYP$Gender)/946)*100
chisq.test(FYP$Gender,FYP$Dep_ep_pastyear)
FYP %>% group_by(Dep_ep_pastyear,Gender) %>%
  summarise(n())

#employment
table(FYP$Employment)
chisq.test(FYP$Employment,FYP$Dep_ep_pastyear)
FYP %>% group_by(Dep_ep_pastyear,Employment) %>%
  summarise(n())

#country of residence 
FYP$Country <- as.numeric(as.character(FYP$Country))
FYP$Country[which(FYP$Country != 9 & FYP$Country != 31 & FYP$Country != 82
      & FYP$Country != 185 & FYP$Country != 187)] <- 999
table(FYP$Country)
(table(FYP$Country)/946)*100

chisq.test(FYP$Country,FYP$Dep_ep_pastyear)
#----------------------------------------
#depressive episodes in past year and physician diagnosed depression 
FYP %>% group_by(Dep_ep_pastyear,Country) %>%
  summarise(n())

chisq.test(FYP$Depression_Physician,FYP$Dep_ep_pastyear)
FYP %>% group_by(Dep_ep_pastyear,Depression_Physician) %>%
  summarise(n())


#----------------------------------------
#Education levels 
table(FYP$Education)
FYP %>% group_by(Dep_ep_pastyear,Education) %>%
  summarise(n())

chisq.test(FYP$Education,FYP$Dep_ep_pastyear)
#----------------------------------------
#Twitter data 
mean(FYP$tweet); sd(FYP$tweet)
as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
                summarise(tweet = mean(tweet, na.rm = TRUE)))
as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
  summarise(tweet = sd(tweet, na.rm = TRUE)))
summary(glm(tweet ~ Dep_ep_pastyear, family = "gaussian",data = FYP))


mean(FYP$retweet); sd(FYP$retweet)
as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
                summarise(retweet = mean(retweet, na.rm = TRUE)))
as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
                summarise(retweet = sd(retweet, na.rm = TRUE)))
summary(glm(retweet ~ Dep_ep_pastyear, family = "gaussian",data = FYP))

mean(FYP$like); sd(FYP$like)
as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
                summarise(like = mean(like, na.rm = TRUE)))

as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
                summarise(like = sd(like, na.rm = TRUE)))

summary(glm(like ~ Dep_ep_pastyear, family = "gaussian",data = FYP))

#----------------------------------------

as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
                summarise(WC = mean(WC, na.rm = TRUE)))

as.data.frame(FYP %>% group_by(Dep_ep_pastyear) %>%
                summarise(WC = sd(WC, na.rm = TRUE)))
#------------------------------------------------------------------------

correlation_depression <- FYP %>% select(colnames(FYP)[41:126],Depression_zscore)
correlation_depression_mat <- rcorr(as.matrix(correlation_depression), type = "pearson")
network_dep <- names(which(correlation_depression_mat$P[,87] <= 0.05))
network_nodep <- names(which(correlation_depression_mat$P[,87] > 0.05))
