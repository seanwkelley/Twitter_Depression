library(dplyr)
library(ggplot2)
library(lmerTest)
library(broom)
library(reshape2)
library(lme4)
library(stringr)
library(roll)
library(zoo)
library(patchwork)
set.seed(2020)
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
  s <- sd(na.omit(x))
  m <- mean(na.omit(x))
  y <- x
  y[x > (m + 3*s)] <- NA
  y[x < (m - 3*s)] <- NA
  y
}


gaussian_detrend <- function(df,bandwidth_ksmooth,var) {
  sd.detrend <- list()
  sd.detrend[[1]] <- mean(na.omit(df$WC))
  for(i in 3:dim(df)[2]){
    var_smooth <- suppressWarnings(ksmooth(df$Day,na.approx(df[,i]) ,
                                           n.points = length(df$Day), 
                                           kernel =  "normal", bandwidth = bandwidth_ksmooth))
    
    #remove any trends by subtracting the smoothed series from the original 
    resid_series <- suppressWarnings((df[,i]) - var_smooth$y)
    print(mean(na.omit(resid_series)))
    #sd_resid <- sd(na.omit(resid_series))
    sd_resid <- sd(na.omit(df[,i]))
    sd.detrend[[i]] <- sd_resid
  }
  sd.detrend <- unlist(sd.detrend)
}


# function to apply to all rows
remove_all_outliers <- function(d){
  d[] <- lapply(d, function(x) if (is.numeric(x))
    remove_outliers(x) else x)
  d
}


'%!in%' <- function(x,y)!('%in%'(x,y))

acf.reference <- function(y, n, lag=1) {
  require(zoo)
  rollapply(y, width = n, FUN=function(x) acf(x, plot = FALSE, lag.max = lag)$acf[1+lag])
}


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
setwd('D:/Twitter_Depression_Kelley/')

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

#set values for autocorrelation analysis 
rolling_window = 0 #length of rolling window 
bandwidth_ksmooth <- 7 #bandwidth is for smoothing and removing high-frequency trends 

#############################################################
#############################################################

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]
FYP_df[,6:93]= remove_all_outliers(FYP_df[6:93])
#remove outliers in any of the sentiments (LIWC and ANEW)
FYP_df$Depressed_today <- as.factor(FYP_df$Depressed_today)

#############################################################################
#############################################################################
#list of unique participant ids 

id_list <- unique(FYP_df$Id)

sentiments.detrend <- list()

for(id in id_list) {
  
  print(id)
  
  ex1 <- FYP_df %>% filter(Id == id) %>% select(Day,WC,negemo,posemo,i,we,shehe,they,you,swear,article,negate)
  if(length(ex1$Day) >= rolling_window) {
    
    sentiments.detrend[[id]] <- gaussian_detrend(ex1,bandwidth_ksmooth = 7)
    
  }
  
}

sentiments.detrend <- as.data.frame(do.call(rbind, sentiments.detrend))
colnames(sentiments.detrend) <- c("WC","negemo","posemo","i",
                                  "we","shehe","they","you","swear",
                                  "article","negate")
sentiments.detrend$Id <- rownames(sentiments.detrend)

sentiments.detrend$Depression_zscore <- participants$Depression_zscore[which(participants$Id %in% sentiments.detrend$Id)]
sentiments.detrend$Dep_Episode <- participants$Dep_ep_pastyear[which(participants$Id %in% sentiments.detrend$Id)]

########################################################################################
#scatterplots

g1 <- ggplot(sentiments.detrend,aes(y = negemo,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("negemo ( " * sigma ~ ")"))


g2 <- ggplot(sentiments.detrend,aes(y = posemo,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("posemo ( " * sigma ~ ")"))

g3 <- ggplot(sentiments.detrend,aes(y = i,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE)+ ylab(expression("i ( " * sigma ~ ")"))


g4 <- ggplot(sentiments.detrend,aes(y = we,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE)+ ylab(expression("we ( " * sigma ~ ")"))

g5 <- ggplot(sentiments.detrend,aes(y = you,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE)+ ylab(expression("you ( " * sigma ~ ")"))


g6 <- ggplot(sentiments.detrend,aes(y = shehe,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE)+ ylab(expression("shehe ( " * sigma ~ ")"))

g7 <- ggplot(sentiments.detrend,aes(y = they,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("they ( " * sigma ~ ")"))

g8 <- ggplot(sentiments.detrend,aes(y = swear,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("swear ( " * sigma ~ ")"))

g9 <- ggplot(sentiments.detrend,aes(y = negate,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("negate ( " * sigma ~ ")"))

g10 <- ggplot(sentiments.detrend,aes(y = article,x=Depression_zscore)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 2,se=FALSE) + ylab(expression("article ( " *sigma ~")"))

combined <- (g10 + g2 + g4 + g6 + g9 +  plot_layout(ncol = 5)) / (g7 + g5 + g1 + g3 + g8  + plot_layout(ncol = 5))  +
  plot_annotation(caption = "Depression Z-Score",tag_levels = 'A',  theme = theme(plot.caption = element_text(size = 18,hjust = 0.5)))

combined <- combined & ylim(0,4) & scale_x_continuous(breaks=number_ticks(4))

########################################################################################
glm_estimates <- as.data.frame(matrix(nrow = 10,ncol = 3))
colnames(glm_estimates) <- c("Sentiment","Estimate","SE")
glm_estimates$Sentiment <- c("negemo","posemo","i","we","shehe","they","you","swear",
                             "article","negate")

glm_estimates[1,2] <- summary(glm(negemo ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[1,3] <- summary(glm(negemo ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[2,2] <- summary(glm(posemo ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[2,3] <- summary(glm(posemo ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[3,2] <- summary(glm(i ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[3,3] <- summary(glm(i ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[4,2] <- summary(glm(we ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[4,3] <- summary(glm(we ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[5,2] <- summary(glm(shehe ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[5,3] <- summary(glm(shehe ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[6,2] <- summary(glm(they ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[6,3] <- summary(glm(they ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[7,2] <- summary(glm(you ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[7,3] <- summary(glm(you ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[8,2] <- summary(glm(swear ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[8,3] <- summary(glm(swear ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[9,2] <- summary(glm(article ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[9,3] <- summary(glm(article ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

glm_estimates[10,2] <- summary(glm(negate ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,1]
glm_estimates[10,3] <- summary(glm(negate ~ Depression_zscore,data = sentiments.detrend))$coefficients[2,2]*1.96

mean.est <- ggplot(glm_estimates, aes(x= reorder(Sentiment, -Estimate), y=Estimate)) + 
  geom_point() +coefplot_theme + 
  geom_errorbar(aes(ymin=Estimate-SE, ymax=Estimate+SE), width=.2,position=position_dodge(0.05)) +
  xlab("LIWC Sentiment\n") + geom_hline(yintercept=0, linetype="dashed", color = "black") +
  ylab("\n Regression Coefficient (95% CI)") +  coord_flip()


(combined | mean.est) +  plot_layout(widths = c(2, 1)) + 
  plot_annotation(caption = "Depression Z-Score",tag_levels = 'A',
                  theme = theme(plot.caption = element_text(size = 18,hjust = 0.3)))
