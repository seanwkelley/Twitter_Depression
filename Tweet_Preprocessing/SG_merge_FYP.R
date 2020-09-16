#merge data from Qualtrics (clickworker/free) and SurveyGizmo 
library(dplyr)
library(ggplot2)

'%!in%' <- function(x,y)!('%in%'(x,y))

setwd('D:/Twitter_Depression_Kelley/')

#surveygizmo data
SG_df <- read.csv('Data/Participant_Data/SG/SurveyGizmo_Participants.csv',stringsAsFactors = FALSE)


#need to convert SG_df into the FYP_df format 
#need lat/long, date_submitted, start and end dates for depressive episodes 

colnames(SG_df)[31:38] <- c("CESD_1","CESD_2","CESD_3","CESD_4","CESD_5","CESD_6","CESD_7","CESD_8")

colnames(SG_df)[39] <- "Dep_ep_pastyear"

colnames(SG_df)[which(colnames(SG_df)=='IP.Address')] <- "IPAddress"
colnames(SG_df)[which(colnames(SG_df)=='Latitude')] <- "LocationLatitude"
colnames(SG_df)[which(colnames(SG_df)=='Longitude')] <- "LocationLongitude"
colnames(SG_df)[which(colnames(SG_df)=='Time.Started')] <- "StartDate"
colnames(SG_df)[which(colnames(SG_df)=='Date.Submitted')] <- "EndDate"
colnames(SG_df)[which(colnames(SG_df)=='Employment.Status')] <- "Employment"


colnames(SG_df)[40:44] <- c("Depression_Dates.1_1_1","Depression_Dates.1_2_1","Depression_Dates.1_3_1",
                            "Depression_Dates.1_4_1","Depression_Dates.1_5_1")

colnames(SG_df)[46:50] <- c("Depression_Dates.1_1_2","Depression_Dates.1_2_2","Depression_Dates.1_3_2",
                            "Depression_Dates.1_4_2","Depression_Dates.1_5_2")

SG_df <- SG_df %>% select(StartDate,EndDate,IPAddress,LocationLatitude,LocationLongitude,Age,Gender,Have.you.ever.been.diagnosed.by.a.physician.with.depression.,
                          Approximately.when.were.you.diagnosed.with.depression.,Dep_ep_pastyear,Please.indicate.the.country.in.which.you.currently.reside.,
                          Employment,How.often.do.you.Tweet.,Highest.Level.of.Education.Achieved,Twitter_Handle,colnames(SG_df)[31:38],
                          colnames(SG_df)[40:44],colnames(SG_df)[46:50],Id)


SG_df$recruitment_type <- 'surveygizmo'

#add in a depression sum score for SG and FYP 


SG_df$CESD_1 <- factor(SG_df$CESD_1, levels = c("Rarely or none of the time (less than 1 day)","Some or a little of the time (1-2 days)",
                                                "Occasionally or a moderate amount of time (3-4 days)","Most or all of the time (5-7 days)"))
SG_df$CESD_2 <- factor(SG_df$CESD_2, levels = c("Rarely or none of the time (less than 1 day)","Some or a little of the time (1-2 days)",
                                                "Occasionally or a moderate amount of time (3-4 days)","Most or all of the time (5-7 days)"))
SG_df$CESD_3 <- factor(SG_df$CESD_3, levels = c("Rarely or none of the time (less than 1 day)","Some or a little of the time (1-2 days)",
                                                "Occasionally or a moderate amount of time (3-4 days)","Most or all of the time (5-7 days)"))
SG_df$CESD_4 <- factor(SG_df$CESD_4, levels = c("Rarely or none of the time (less than 1 day)","Some or a little of the time (1-2 days)",
                                                "Occasionally or a moderate amount of time (3-4 days)","Most or all of the time (5-7 days)"))
SG_df$CESD_5 <- factor(SG_df$CESD_5, levels = c("Rarely or none of the time (less than 1 day)","Some or a little of the time (1-2 days)",
                                                "Occasionally or a moderate amount of time (3-4 days)","Most or all of the time (5-7 days)"))
SG_df$CESD_6 <- factor(SG_df$CESD_6, levels = c("Rarely or none of the time (less than 1 day)","Some or a little of the time (1-2 days)",
                                                "Occasionally or a moderate amount of time (3-4 days)","Most or all of the time (5-7 days)"))
SG_df$CESD_7 <- factor(SG_df$CESD_7, levels = c("Rarely or none of the time (less than 1 day)","Some or a little of the time (1-2 days)",
                                                "Occasionally or a moderate amount of time (3-4 days)","Most or all of the time (5-7 days)"))
SG_df$CESD_8 <- factor(SG_df$CESD_8, levels = c("Rarely or none of the time (less than 1 day)","Some or a little of the time (1-2 days)",
                                                "Occasionally or a moderate amount of time (3-4 days)","Most or all of the time (5-7 days)"))

SG_df <- SG_df %>% mutate_at(vars(starts_with("CESD")),funs(factor)) %>%   
  mutate_at(vars(starts_with("CESD")),funs(as.numeric))

cesd <- c("CESD_1","CESD_2","CESD_3","CESD_4","CESD_5","CESD_6","CESD_7","CESD_8")
SG_df[,cesd] <- SG_df[,cesd] - 1  

#reverse code items 4 and 6 
SG_df[,c("CESD_4","CESD_6")] <- 3 - SG_df[,c("CESD_4","CESD_6")] 

SG_df$CESD_total <- rowSums(SG_df[,cesd])

#rename additional columns to FYP format
colnames(SG_df)[which(colnames(SG_df) == 'Have.you.ever.been.diagnosed.by.a.physician.with.depression.')] <- "Depression_Physician"
colnames(SG_df)[which(colnames(SG_df) == 'Please.indicate.the.country.in.which.you.currently.reside.')] <- "Country"
colnames(SG_df)[which(colnames(SG_df) == 'Approximately.when.were.you.diagnosed.with.depression.')] <- "Depression_Diagnosis"
colnames(SG_df)[which(colnames(SG_df) == 'How.often.do.you.Tweet.')] <- "Twitter_Screen"
colnames(SG_df)[which(colnames(SG_df) == 'Highest.Level.of.Education.Achieved')] <- "Education"


SG_df <- SG_df %>% select(colnames(SG_df)[1:23],Depression_Dates.1_1_1,Depression_Dates.1_1_2,Depression_Dates.1_2_1,Depression_Dates.1_2_2,
                          Depression_Dates.1_3_1,Depression_Dates.1_3_2,Depression_Dates.1_4_1,Depression_Dates.1_4_2,Depression_Dates.1_5_1,
                          Depression_Dates.1_5_2,CESD_total,recruitment_type,Id)


SG_df <- SG_df %>% select(-c(paste0(rep("CESD_",8),seq(1,8))))


#######################################
#reorder column names 

SG_df$Dep_ep_pastyear <- ifelse(SG_df$Dep_ep_pastyear == "Yes",1,0)
SG_df$Depression_Physician <- ifelse(SG_df$Depression_Physician == "Yes",1,0)

#change SG_df dates from mm/dd/yyyy to dd/mm/yyyy 
for(i in which(colnames(SG_df) == 'Depression_Dates.1_1_1'):which(colnames(SG_df) == 'Depression_Dates.1_5_2')){
  print(i)
  SG_df[,i] <- as.Date(SG_df[,i], format = "%m/%d/%Y")
  SG_df[,i] <- format(SG_df[,i], "%d/%m/%Y")
}

#convert countries to numeric (FYP format)
SG_df$Gender <- as.numeric(factor(SG_df$Gender, levels = c("Male","Female","Transgender Male",
                                                           "Transgender Female","Non-Binary","Prefer not to say")))
SG_df$Employment <- ifelse(SG_df$Employment == 1,1,0)
SG_df$Education <- as.numeric(factor(SG_df$Education, levels = c("Less than high school or equivalent","High school or equivalent degree",
                                                                 "Some college, no degree", "Bachelor's degree","Master's degree",
                                                                 "Professional degree (e.g. MD, JD)","Doctorate")))


SG_df$Twitter_Screen <- as.numeric(factor(SG_df$Twitter_Screen, levels = c("Don't have a Twitter account","Hardly ever or not at all",
                                                                           "At least once a month","At least once a week",
                                                                           "Almost everyday or everyday","Multiple times per day")))

SG_df[which(SG_df$Country == "United States" | SG_df$Country == "United States of America (USA)"),11] <- 187
SG_df[which(SG_df$Country == "United Kingdom" | SG_df$Country == "United Kingdom (UK)"),11] <- 185
SG_df[which(SG_df$Country == "Ireland" | SG_df$Country == "Ireland, Republic of"),11] <- 82
SG_df[which(SG_df$Country == "Thailand"),11] <- 172
SG_df[which(SG_df$Country == "Sri Lanka"),11] <- 164
SG_df[which(SG_df$Country == "Spain"),11] <- 163
SG_df[which(SG_df$Country == "South Africa"),11] <- 161
SG_df[which(SG_df$Country == "Singapore"),11] <- 156
SG_df[which(SG_df$Country == "Saudi Arabia"),11] <- 151
SG_df[which(SG_df$Country == "Russian Federation"),11] <- 143
SG_df[which(SG_df$Country == "Oman"),11] <- 129
SG_df[which(SG_df$Country == "New Zealand"),11] <- 123
SG_df[which(SG_df$Country == "Netherlands"),11] <- 122
SG_df[which(SG_df$Country == "Malaysia"),11] <- 104
SG_df[which(SG_df$Country == "Kenya"),11] <- 89
SG_df[which(SG_df$Country == "Italy"),11] <- 84
SG_df[which(SG_df$Country == "Indonesia"),11] <- 79
SG_df[which(SG_df$Country == "India"),11] <- 78
SG_df[which(SG_df$Country == "Germany"),11] <- 65
SG_df[which(SG_df$Country == "France"),11] <- 61
SG_df[which(SG_df$Country == "Czech Republic"),11] <- 45
SG_df[which(SG_df$Country == "Canada"),11] <- 31
SG_df[which(SG_df$Country == "Belgium"),11] <- 17
SG_df[which(SG_df$Country == "Austria"),11] <- 10
SG_df[which(SG_df$Country == "Australia"),11] <- 9
SG_df[which(SG_df$Country == "Algeria"),11] <- 3

###########################################################################
#Depression_zscore 
SG_df$Depression_zscore <- (SG_df$CESD_total - mean(SG_df$CESD_total))/sd(SG_df$CESD_total)


###########################################################################
#combine SG and FYP
write.csv(SG_df,file = "SurveyGizmo_Participants_12.05.csv",row.names = FALSE)





