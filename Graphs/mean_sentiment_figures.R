library(dplyr)
library(ggplot2)
library(lmerTest)
library(Hmisc)
library(bootnet)
library(qgraph)
library(NetworkComparisonTest)
library(patchwork)
library(extrafont)
loadfonts(device = "win")
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
  text = element_text(size = 36,family = "Arial"),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 36,family = "Arial"),
  axis.text = element_text(size = 36,family = "Arial"),
  axis.text.x = element_text(vjust = 0.5,colour = "black",family = "Arial"),
  axis.text.y = element_text(colour = "black",family = "Arial"),
  legend.title=element_text(size=36,family = "Arial"),
  legend.text=element_text(size=36,family = "Arial"),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 20,family = "Arial"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


#############################################################
#############################################################
#sentiment analysis based on tweets 

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")
participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
dc_all <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
colnames(dc_all)[1] <- "Id"

#filter participants with at least 30 days of 
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_all$Id))
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2

FYP_df <- FYP_df[,c(3,8:94,96)]
FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")
FYP_df_mean[,c(2:87,89)]= remove_all_outliers(FYP_df_mean[c(2:87,89)])
FYP_df_mean[,c(2:87,89)] = scale(FYP_df_mean[,c(2:87,89)])
FYP <- merge(participants,FYP_df_mean,by='Id')


########################################################################################
#scatterplots

#"FantasticFox1" color scheme 

g1 <- ggplot(FYP,aes(y = negemo,x=Depression_zscore)) + geom_point(alpha= 0.8,color = "#DD8D29",size =3) + xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + ylab("negemo") 


g2 <- ggplot(FYP,aes(y = posemo,x=Depression_zscore)) + geom_point(alpha = 0.8,color = "#E2D200",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + ylab("posemo")

g3 <- ggplot(FYP,aes(y = i,x=Depression_zscore)) + geom_point(alpha = 0.8,color = "#46ACC8",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black")+ ylab("i")

#Rushmore 1 number 3
g4 <- ggplot(FYP,aes(y = we,x=Depression_zscore)) + geom_point(alpha = 0.8,color = "#0B775E",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black")+ ylab("we")

g5 <- ggplot(FYP,aes(y = you,x=Depression_zscore)) + geom_point(alpha = 0.8,color = "#B40F20",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black")+ ylab("you")

#Darjeeling2 color scheme  

g6 <- ggplot(FYP,aes(y = pro3,x=Depression_zscore)) + geom_point(alpha = 0.8,color= "#C51B7D",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black")+ ylab("pro3")

#g7 <- ggplot(FYP,aes(y = they,x=Depression_zscore)) + geom_point(alpha = 0.8,color = "#046C9A",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
#  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + ylab("they")

g8 <- ggplot(FYP,aes(y = swear,x=Depression_zscore)) + geom_point(alpha = 0.8,color = "#8C510A",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + ylab("swear")

g9 <- ggplot(FYP,aes(y = negate,x=Depression_zscore)) + geom_point(alpha = 0.8,color = "#7FBC41",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + ylab("negate")

g10 <- ggplot(FYP,aes(y = article,x=Depression_zscore)) + geom_point(alpha = 0.8,color = "#08306B",size =3) +  xlab("SDS Summed Score") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + ylab("article")

#combined <- (g10 + g4 + g2 + g6 + g5 +  plot_layout(ncol = 5)) / (g8 + g9 + g3 + g1  + plot_layout(ncol = 5))  +
#  plot_annotation(caption = "\n           Depression Symptom Severity (z-score)",tag_levels = 'A', 
#                  theme = theme(plot.caption = element_text(size = 36,hjust = 0.5,family = "Arial")))


combined <- (g10 + g4 + g2 ) / (g6 + g5 + g8) / (g9 + g3 + g1) +
  plot_annotation(caption = "\n           Depression Symptom Severity (z-score)",tag_levels = 'A', 
                  theme = theme(plot.caption = element_text(size = 36,hjust = 0.5,family = "Arial")))

combined <- combined & ylim(-2,4) & scale_x_continuous(breaks=number_ticks(4)) &
  theme(plot.tag = element_text(size = 36,face = "bold",family = "Arial"))


tiff("Figures/Mean_Regression.tiff", units="cm", width=50, height=50, res=600)
combined
dev.off()


########################################################################################

