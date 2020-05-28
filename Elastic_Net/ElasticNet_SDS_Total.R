library(dplyr)
library(ggplot2)
library(lmerTest)
library(Hmisc)

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

enet <- read.csv('Data/elasticNet/enet_10f.csv')
colnames(enet) <- c("Name","Beta")
enet <- enet %>% filter(Beta != 0)
enet2 <- as.data.frame(enet[,-1])
rownames(enet2) <- enet[,1]
colnames(enet2) <- "Beta"

participants <- read.csv('Data/Participant_Data/FYP_Twitter_Participants_20.04.csv')
FYP_df <- read.csv('Data/Sentiments/all_tweets/VADER_ANEW_LIWC_complete.csv')
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'


FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")
FYP_df_mean[,4:91]= remove_all_outliers(FYP_df_mean[4:91])
FYP_df_mean <- as.data.frame(cbind(FYP_df_mean[,1:3],scale(FYP_df_mean[,4:91])))

#merge sentiments and participants data 
FYP <- merge(participants,FYP_df_mean,by='Id')
##############################################################################
FYP <- FYP %>% select(c(rownames(enet2)),colnames(FYP)[grepl("SDS_",colnames(FYP))])
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



p.mat = rcorr(as.matrix(na.omit(FYP)))$P[1:dim(enet2)[1],(dim(enet2)[1]+1):dim(FYP)[2]]
r.mat = rcorr(as.matrix(na.omit(FYP)))$r[1:dim(enet2)[1],(dim(enet2)[1]+1):dim(FYP)[2]]

r.mat_SDS <- rcorr(as.matrix(na.omit(FYP)))$r[27:46,27:46]
p.mat_SDS <- rcorr(as.matrix(na.omit(FYP)))$P[27:46,27:46]

colnames(r.mat) <- c("1_down","2_morningbest","3_crying","4_troubsleep","5_eat","6_sex","7_loseweight",
                     "8_troubconstip","9_heartbeat","10_tired","11_clearmind","12_easydo","13_restless",
                     "14_hopefuture","15_irritable","16_easydecision","17_useful","18_lifefull","19_betterdead",
                     "20_enjoythings")

colnames(r.mat_SDS) <-  c("1_down","2_morningbest","3_crying","4_troubsleep","5_eat","6_sex","7_loseweight",
                          "8_troubconstip","9_heartbeat","10_tired","11_clearmind","12_easydo","13_restless",
                          "14_hopefuture","15_irritable","16_easydecision","17_useful","18_lifefull","19_betterdead",
                          "20_enjoythings")

row.names(r.mat_SDS) <- c("1_down","2_morningbest","3_crying","4_troubsleep","5_eat","6_sex","7_loseweight",
                          "8_troubconstip","9_heartbeat","10_tired","11_clearmind","12_easydo","13_restless",
                          "14_hopefuture","15_irritable","16_easydecision","17_useful","18_lifefull","19_betterdead",
                          "20_enjoythings")


corrplot::corrplot(r.mat,method = "square",p.mat = p.mat,sig.level = 0.05,insig = "blank", mar = c(2,0,2,0),
         cl.lim=c(-0.40,0.40),is.corr = FALSE)