#----------------------------------------------
#Table S1: Demographic and Twitter use characteristics of participants through either paid or free recruitment channels 
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

#LIWC text feature data
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
FYP <- merge(participants,FYP_df_mean,by='Id')


#recode categorical variables as factors
FYP$Dep_ep_pastyear <- as.factor(FYP$Dep_ep_pastyear)
FYP$Gender <- as.factor(FYP$Gender)
FYP$Education <- as.factor(FYP$Education)
FYP$Employment <- as.factor(FYP$Employment)
FYP$recruitment_type <- ifelse(FYP$recruitment_type == 'version2_clickworker_data',"paid","free")
FYP$Country <- as.numeric(as.character(FYP$Country))
FYP$Country[which(FYP$Country != 9 & FYP$Country != 31 & FYP$Country != 82
                  & FYP$Country != 185 & FYP$Country != 187 | is.na(FYP$Country))] <- 999

##################################################################
#create an empty data frame
table1 = data.frame(matrix(vector(),27 , 8,
                           dimnames=list(c(), c("demo_feat","FS_mean","FS_sd","free_mean","free_sd",
                                                "paid_mean","paid_sd","p-val"))),
                    stringsAsFactors=F)

numeric_vars_mean <- as.data.frame(FYP %>% dplyr::group_by(recruitment_type) %>%
                                     dplyr::summarise_at(vars(tweet,retweet,like,WC,Age),funs(mean(.,na.rm = TRUE))))

numeric_vars_sd <- as.data.frame(FYP %>% dplyr::group_by(recruitment_type) %>%
                                   dplyr::summarise_at(vars(tweet,retweet,like,WC,Age),funs(sd(.,na.rm = TRUE))))

#tweets in full sample
table1[1,1] <- "tweet";table1[1,2] <- mean(FYP$tweet);table1[1,3] <- sd(FYP$tweet)

#tweet mean/sd by depressive epsiodes
table1[1,4] <- numeric_vars_mean$tweet[1];table1[1,5] <- numeric_vars_sd$tweet[1] 
table1[1,6] <- numeric_vars_mean$tweet[2];table1[1,7] <- numeric_vars_sd$tweet[2] 
table1[1,8] <- coefficients(summary(glm(tweet ~ recruitment_type, family = "gaussian",data = FYP)))[8]

#retweets
table1[2,1] <- "retweet";table1[2,2] <- mean(FYP$retweet);table1[2,3] <- sd(FYP$retweet)
table1[2,4] <- numeric_vars_mean$retweet[1];table1[2,5] <- numeric_vars_sd$retweet[1] 
table1[2,6] <- numeric_vars_mean$retweet[2];table1[2,7] <- numeric_vars_sd$retweet[2] 
table1[2,8] <- coefficients(summary(glm(retweet ~ recruitment_type, family = "gaussian",data = FYP)))[8]

#likes
table1[3,1] <- "like";table1[3,2] <- mean(FYP$retweet);table1[3,3] <- sd(FYP$like)
table1[3,4] <- numeric_vars_mean$like[1];table1[3,5] <- numeric_vars_sd$like[1] 
table1[3,6] <- numeric_vars_mean$like[2];table1[3,7] <- numeric_vars_sd$like[2] 
table1[3,8] <- coefficients(summary(glm(like ~ recruitment_type, family = "gaussian",data = FYP)))[8]


#recruitment type
rt <- (FYP %>% dplyr::group_by(recruitment_type,recruitment_type) %>%
         dplyr::summarise(n()) %>% filter(recruitment_type == "paid"))
table1[4,1] <- "Recruitment Type"; table1[4,2] <- as.numeric(table(FYP$recruitment_type)[2]); table1[4,3] <- as.numeric((table(FYP$recruitment_type)[2]/946)*100)
table1[4,4] <- rt$`n()`[1]; table1[4,5] <- as.numeric((rt$`n()`[1]/266)*100)
table1[4,6] <- rt$`n()`[2]; table1[4,7] <- as.numeric((rt$`n()`[2]/680)*100)
table1[4,8] <- as.numeric(chisq.test(FYP$recruitment_type,FYP$recruitment_type)[3]) 


#Word Count
table1[5,1] <- "Word Count";table1[5,2] <- mean(FYP$WC,na.rm = T);table1[5,3] <- sd(FYP$WC, na.rm = T)
table1[5,4] <- numeric_vars_mean$WC[1];table1[5,5] <- numeric_vars_sd$WC[1] 
table1[5,6] <- numeric_vars_mean$WC[2];table1[5,7] <- numeric_vars_sd$WC[2] 
table1[5,8] <- coefficients(summary(glm(WC ~ recruitment_type, family = "gaussian",data = FYP)))[8]

#Age
table1[6,1] <- "Age";table1[6,2] <- mean(FYP$Age,na.rm = T);table1[6,3] <- sd(FYP$Age, na.rm = T)
table1[6,4] <- numeric_vars_mean$Age[1];table1[6,5] <- numeric_vars_sd$Age[1] 
table1[6,6] <- numeric_vars_mean$Age[2];table1[6,7] <- numeric_vars_sd$Age[2] 
table1[6,8] <- coefficients(summary(glm(Age ~ recruitment_type, family = "gaussian",data = FYP)))[8]

#gender
gender <- table(FYP$recruitment_type,FYP$Gender)
table1[7,1] <- "Male";table1[7,2] <- as.numeric(table(FYP$Gender)[1]); table1[7,3] <- (as.numeric(table(FYP$Gender)[1])/946)*100
table1[8,1] <- "Female";table1[8,2] <- as.numeric(table(FYP$Gender)[2]); table1[8,3] <- (as.numeric(table(FYP$Gender)[2])/946)*100
table1[9,1] <- "Transgender Male";table1[9,2] <- as.numeric(table(FYP$Gender)[3]); table1[9,3] <- (as.numeric(table(FYP$Gender)[3])/946)*100
table1[10,1] <- "Transgender Female";table1[10,2] <- as.numeric(table(FYP$Gender)[4]); table1[10,3] <- (as.numeric(table(FYP$Gender)[4])/946)*100
table1[11,1] <- "Non-Binary";table1[11,2] <- as.numeric(table(FYP$Gender)[5]); table1[11,3] <- (as.numeric(table(FYP$Gender)[5])/946)*100
table1[12,1] <- "Other";table1[12,2] <- as.numeric(table(FYP$Gender)[6]); table1[12,3] <- (as.numeric(table(FYP$Gender)[6])/946)*100


table1[7,4] <- (gender[1,1]); table1[7,5] <- (as.numeric(gender[1,1]/266))*100
table1[7,6] <- (gender[2,1]); table1[7,7] <- (as.numeric(gender[2,1]/680))*100

table1[8,4] <- (gender[1,2]); table1[8,5] <- (as.numeric(gender[1,2]/266))*100
table1[8,6] <- (gender[2,2]); table1[8,7] <- (as.numeric(gender[2,2]/680))*100

table1[9,4] <- (gender[1,3]); table1[9,5] <- (as.numeric(gender[1,3]/266))*100
table1[9,6] <- (gender[2,3]); table1[9,7] <- (as.numeric(gender[2,3]/680))*100

table1[10,4] <- (gender[1,4]); table1[10,5] <- (as.numeric(gender[1,4]/266))*100
table1[10,6] <- (gender[2,4]); table1[10,7] <- (as.numeric(gender[2,4]/680))*100

table1[11,4] <- (gender[1,5]); table1[11,5] <- (as.numeric(gender[1,5]/266))*100
table1[11,6] <- (gender[2,5]); table1[11,7] <- (as.numeric(gender[2,5]/680))*100

table1[12,4] <- (gender[1,6]); table1[12,5] <- (as.numeric(gender[1,6]/266))*100
table1[12,6] <- (gender[2,6]); table1[12,7] <- (as.numeric(gender[2,6]/380))*100
table1[7,8] <- as.numeric(chisq.test(FYP$Gender,FYP$recruitment_type)[3]) 


#country
country <- table(FYP$recruitment_type,FYP$Country)
table1[13,1] <- "Ireland"; table1[13,2] <- table(FYP$Country)[3]; table1[13,3] <- (as.numeric(table(FYP$Country)[3])/946)*100
table1[14,1] <- "United Kingdom"; table1[14,2] <- table(FYP$Country)[4]; table1[14,3] <- (as.numeric(table(FYP$Country)[4])/946)*100
table1[15,1] <- "United States"; table1[15,2] <- table(FYP$Country)[5]; table1[15,3] <- (as.numeric(table(FYP$Country)[5])/946)*100
table1[16,1] <- "Canada"; table1[16,2] <- table(FYP$Country)[2]; table1[16,3] <- (as.numeric(table(FYP$Country)[2])/946)*100
table1[17,1] <- "Australia"; table1[17,2] <- table(FYP$Country)[1]; table1[17,3] <- (as.numeric(table(FYP$Country)[1])/946)*100
table1[18,1] <- "Other"; table1[18,2] <- table(FYP$Country)[6]; table1[18,3] <- (as.numeric(table(FYP$Country)[6])/946)*100

table1[13,4] <- country[1,3]; table1[13,5] <- (as.numeric(country[1,3])/266)*100
table1[13,6] <- country[2,3]; table1[13,7] <- (as.numeric(country[2,3])/680)*100

table1[14,4] <- country[1,4]; table1[14,5] <- (as.numeric(country[1,4])/266)*100
table1[14,6] <- country[2,4]; table1[14,7] <- (as.numeric(country[2,4])/680)*100

table1[15,4] <- country[1,5]; table1[15,5] <- (as.numeric(country[1,5])/266)*100
table1[15,6] <- country[2,5]; table1[15,7] <- (as.numeric(country[2,5])/680)*100

table1[16,4] <- country[1,2]; table1[16,5] <- (as.numeric(country[1,2])/266)*100
table1[16,6] <- country[2,2]; table1[16,7] <- (as.numeric(country[2,2])/680)*100

table1[17,4] <- country[1,1]; table1[17,5] <- (as.numeric(country[1,1])/266)*100
table1[17,6] <- country[2,1]; table1[17,7] <- (as.numeric(country[2,1])/680)*100

table1[18,4] <- country[1,6]; table1[18,5] <- (as.numeric(country[1,6])/266)*100
table1[18,6] <- country[2,6]; table1[18,7] <- (as.numeric(country[2,6])/680)*100

table1[13,8] <- as.numeric(chisq.test(FYP$Country,FYP$recruitment_type)[3]) 


#education
education <- table(FYP$Education,FYP$recruitment_type)
table1[19,1] <- "Less than high school"; table1[19,2] <- table(FYP$Education)[1]; table1[19,3] <- (as.numeric(table(FYP$Education)[1])/946)*100
table1[20,1] <- "High School"; table1[20,2] <- table(FYP$Education)[2]; table1[20,3] <- (as.numeric(table(FYP$Education)[2])/946)*100
table1[21,1] <- "Some University"; table1[21,2] <- table(FYP$Education)[3]; table1[21,3] <- (as.numeric(table(FYP$Education)[3])/946)*100
table1[22,1] <- "Bachelor's degree"; table1[22,2] <- table(FYP$Education)[4]; table1[22,3] <- (as.numeric(table(FYP$Education)[4])/946)*100
table1[23,1] <- "Master's degree"; table1[23,2] <- table(FYP$Education)[5]; table1[23,3] <- (as.numeric(table(FYP$Education)[5])/946)*100
table1[24,1] <- "Professional degree"; table1[24,2] <- table(FYP$Education)[6]; table1[24,3] <- (as.numeric(table(FYP$Education)[6])/946)*100
table1[25,1] <- "Doctorate"; table1[25,2] <- table(FYP$Education)[7]; table1[25,3] <- (as.numeric(table(FYP$Education)[7])/946)*100


table1[19,4] <- education[1,1]; table1[19,5] <- (as.numeric(education[1,1])/266)*100
table1[19,6] <- education[1,2]; table1[19,7] <- (as.numeric(education[1,2])/680)*100

table1[20,4] <- education[2,1]; table1[20,5] <- (as.numeric(education[2,1])/266)*100
table1[20,6] <- education[2,2]; table1[20,7] <- (as.numeric(education[2,2])/680)*100

table1[21,4] <- education[3,1]; table1[21,5] <- (as.numeric(education[3,1])/266)*100
table1[21,6] <- education[3,2]; table1[21,7] <- (as.numeric(education[3,2])/680)*100

table1[22,4] <- education[4,1]; table1[22,5] <- (as.numeric(education[4,1])/266)*100
table1[22,6] <- education[4,2]; table1[22,7] <- (as.numeric(education[4,2])/680)*100

table1[23,4] <- education[5,1]; table1[23,5] <- (as.numeric(education[5,1])/266)*100
table1[23,6] <- education[5,2]; table1[23,7] <- (as.numeric(education[5,2])/680)*100

table1[24,4] <- education[6,1]; table1[24,5] <- (as.numeric(education[6,1])/266)*100
table1[24,6] <- education[6,2]; table1[24,7] <- (as.numeric(education[6,2])/680)*100

table1[25,4] <- education[7,1]; table1[25,5] <- (as.numeric(education[7,1])/266)*100
table1[25,6] <- education[7,2]; table1[25,7] <- (as.numeric(education[7,2])/680)*100

table1[19,8] <- as.numeric(chisq.test(FYP$Education,FYP$recruitment_type)[3]) 

#employment
table1[26,1] <- "Currently Employed"; table1[26,2] <- table(FYP$Employment)[2]; table1[26,3] <- (as.numeric(table(FYP$Employment)[2])/946)*100
table1[26,4] <- table(FYP$Employment,FYP$recruitment_type)[2,1]; table1[26,5] <- (as.numeric(table(FYP$Employment,FYP$recruitment_type)[2,1])/266)*100  
table1[26,6] <- table(FYP$Employment,FYP$recruitment_type)[2,2]; table1[26,7] <- (as.numeric(table(FYP$Employment,FYP$recruitment_type)[2,2])/680)*100  
table1[26,8] <- as.numeric(chisq.test(FYP$Employment,FYP$recruitment_type)[3]) 


#physician diagnosed depression
table1[27,1] <- "Physician diagnosed depression"; table1[27,2] <- table(FYP$Depression_Physician)[2]; table1[27,3] <- (as.numeric(table(FYP$Depression_Physician)[2])/946)*100
table1[27,4] <- table(FYP$Depression_Physician,FYP$recruitment_type)[2,1]; table1[27,5] <- (as.numeric(table(FYP$Depression_Physician,FYP$recruitment_type)[2,1])/266)*100  
table1[27,6] <- table(FYP$Depression_Physician,FYP$recruitment_type)[2,2]; table1[27,7] <- (as.numeric(table(FYP$Depression_Physician,FYP$recruitment_type)[2,2])/680)*100  
table1[27,8] <- as.numeric(chisq.test(FYP$Depression_Physician,FYP$recruitment_type)[3]) 


table1 <- table1 %>% select(-c(FS_mean,FS_sd)) %>% select(demo_feat,paid_mean,paid_sd,free_mean,free_sd,p.val)

write.table(table1, file = "Data/Results/all_tweets/TableS1.txt", sep = ";", quote = FALSE, row.names = F)



