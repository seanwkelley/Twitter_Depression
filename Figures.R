library(dplyr)
library(ggplot2)
library(lmerTest)
library(Hmisc)
library(bootnet)
library(qgraph)
library(NetworkComparisonTest)
library(patchwork)
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
number_ticks <- function(n) {function(limits) pretty(limits, n)}

raincloud_theme = theme(
  panel.background = element_blank(),
  text = element_text(size = 10),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


coefplot_theme = theme(
  panel.background = element_blank(),
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
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
FYP_df_mean[,6:93] = scale(FYP_df_mean[,6:93])



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
########################################################################################
#scatterplots

g1 <- ggplot(FYP,aes(y = negemo,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("negemo ( " * mu ~ ")"))


g2 <- ggplot(FYP,aes(y = posemo,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("posemo ( " * mu ~ ")"))

g3 <- ggplot(FYP,aes(y = i,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE)+ ylab(expression("i ( " * mu ~ ")"))


g4 <- ggplot(FYP,aes(y = we,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE)+ ylab(expression("we ( " * mu ~ ")"))

g5 <- ggplot(FYP,aes(y = you,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE)+ ylab(expression("you ( " * mu ~ ")"))


g6 <- ggplot(FYP,aes(y = shehe,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE)+ ylab(expression("shehe ( " * mu ~ ")"))

g7 <- ggplot(FYP,aes(y = they,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("they ( " * mu ~ ")"))

g8 <- ggplot(FYP,aes(y = swear,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("swear ( " * mu ~ ")"))

g9 <- ggplot(FYP,aes(y = negate,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("negate ( " * mu ~ ")"))

g10 <- ggplot(FYP,aes(y = article,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("article ( " *mu ~")"))

combined <- (g4 + g10 + g2 + g6 + g7 +  plot_layout(ncol = 5)) / (g9 + g8 + g5 + g3 + g1  + plot_layout(ncol = 5))  +
  plot_annotation(caption = "Zung Depression Scale",tag_levels = 'A',  theme = theme(plot.caption = element_text(size = 18,hjust = 0.5)))

combined <- combined & ylim(-2,4) & scale_x_continuous(breaks=number_ticks(4))

########################################################################################
glm_estimates <- as.data.frame(matrix(nrow = 10,ncol = 3))
colnames(glm_estimates) <- c("Sentiment","Estimate","SE")
glm_estimates$Sentiment <- c("negemo","posemo","i","we","shehe","they","you","swear",
                             "article","negate")

glm_estimates[1,2] <- summary(glm(negemo ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[1,3] <- summary(glm(negemo ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[2,2] <- summary(glm(posemo ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[2,3] <- summary(glm(posemo ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[3,2] <- summary(glm(i ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[3,3] <- summary(glm(i ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[4,2] <- summary(glm(we ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[4,3] <- summary(glm(we ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[5,2] <- summary(glm(shehe ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[5,3] <- summary(glm(shehe ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[6,2] <- summary(glm(they ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[6,3] <- summary(glm(they ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[7,2] <- summary(glm(you ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[7,3] <- summary(glm(you ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[8,2] <- summary(glm(swear ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[8,3] <- summary(glm(swear ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[9,2] <- summary(glm(article ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[9,3] <- summary(glm(article ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

glm_estimates[10,2] <- summary(glm(negate ~ SDS_Total,data = FYP))$coefficients[2,1]
glm_estimates[10,3] <- summary(glm(negate ~ SDS_Total,data = FYP))$coefficients[2,2]*1.96

mean.est <- ggplot(glm_estimates, aes(x= reorder(Sentiment, -Estimate), y=Estimate)) + 
  geom_point() +coefplot_theme + 
  geom_errorbar(aes(ymin=Estimate-SE, ymax=Estimate+SE), width=.2,position=position_dodge(0.05)) +
  xlab("LIWC Sentiment\n") + geom_hline(yintercept=0, linetype="dashed", color = "black") +
  ylab("\n Regression Coefficient (95% CI)") +  coord_flip()


(combined | mean.est) +  plot_layout(widths = c(2, 1)) + 
  plot_annotation(caption = "Zung Depression Scale",tag_levels = 'A',
                  theme = theme(plot.caption = element_text(size = 18,hjust = 0.3)))