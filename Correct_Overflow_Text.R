#Some participants have cells (Tweet days) with thousands of words
#this causes an overflow of text from one cell to another and produces an incorrect 
#alignment of sentiments, this code puts the misplaced sentiment results 
#back into the correct row 

########################################################
library(dplyr)
setwd('D:/Twitter_Depression_Kelley/')
FYP_df <- read.csv('Data/Sentiments/all_tweets/VADER_ANEW_LIWC.csv',stringsAsFactors = FALSE)


FYP_df <- FYP_df[which(FYP_df$Date != ""),]
indexes <- which(FYP_df$Date == 'version2_clickworker_data' | FYP_df$Date == 'version4_free_data')
indexes2 <- indexes - 1
FYP_df[indexes2,5:12] <- FYP_df[indexes,1:8]
#exclude indexes
FYP_df <- FYP_df[-indexes,]

write.csv(FYP_df,file = "Data/Sentiments/all_tweets/VADER_ANEW_LIWC2.csv",row.names = FALSE)
########################################################
FYP_df <- read.csv('Data/Sentiments/all_tweets/VADER_ANEW_LIWC_13.05.csv',stringsAsFactors = FALSE)


FYP_df <- FYP_df[which(FYP_df$Date != ""),]
indexes <- which(FYP_df$Date == 'version2_clickworker_data' | FYP_df$Date == 'version4_free_data')
indexes2 <- indexes - 1
FYP_df[indexes2,5:12] <- FYP_df[indexes,1:8]
#exclude indexes
FYP_df <- FYP_df[-indexes,]

write.csv(FYP_df,file = "Data/Sentiments/all_tweets/VADER_ANEW_LIWC_13.05.v2.csv",row.names = FALSE)