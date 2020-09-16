library(dplyr)
library(ggplot2)
library(lmerTest)
library(broom)
library(reshape2)
library(lme4)
library(stringr)
library(roll)
library(zoo)
library(rlang)
library(bootnet)
library(qgraph)
library(graphicalVAR)
library(NetworkComparisonTest)
library(mlVAR)
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

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

dc_participants <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
colnames(dc_participants)[1] <- "Id"

pnet_edges <- read.csv('Data/Results/all_tweets/fullsample_edges.csv')

#############################################################
#############################################################

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

FYP_df <- FYP_df[which(FYP_df$Date != ''),]

mlvar_df <- FYP_df %>% select(Id,negemo,posemo,i,we,shehe,they,you,swear,article,negate)
mlvar_df <- mlvar_df %>% filter(Id %in% dc_participants$Id)
mlvar_df_mean <- aggregate(. ~ Id , data = mlvar_df, FUN = "mean")


raincloud_theme2 = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


raincloud_theme3 = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

raincloud_theme4 = theme(
  panel.background = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size=14),
  legend.key=element_blank(), 
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

#-----------------------------------------------------------------------------------------
personal_network <- qgraph(pnet_edges,layout = "spring",fade=FALSE,
                   labels = c("negemo","posemo","i","we","shehe","they","you","swear","article","negate"))

bs_network <- estimateNetwork(mlvar_df_mean[,2:11],default = "EBICglasso",tuning = 0.5)

qgraph(bs_network$graph,fade= FALSE,layout = personal$layout,
       labels = c("negemo","posemo","i","we","shehe","they","you","swear","article","negate"))
#-----------------------------------------------------------------------



network <- as.data.frame(scale(centrality(qgraph(bs_network$graph))$InDegree))
colnames(network) <- "Strength"
network$Closeness <- as.numeric(scale(centrality(qgraph(bs_network$graph))$Closeness))
network$Betweenness <- as.numeric(scale(centrality(qgraph(bs_network$graph))$Betweenness))

network$Sentiment <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")
network$Type <- "Cross-Sectional"



network_personal <- as.data.frame(scale(centrality((personal_network))$InDegree))
colnames(network_personal) <- "Strength"
network_personal$Closeness <- as.numeric(scale(centrality((personal_network))$Closeness))
network_personal$Betweenness <- as.numeric(scale(centrality((personal_network))$Betweenness))
network_personal$Sentiment <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")
network_personal$Type <- "Personalised"

network <- as.data.frame(rbind(network,network_personal))


g1 <- ggplot(data = network,aes(x=Sentiment, y=Strength,group=Type,color=Type)) +
  geom_line() + raincloud_theme2  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\nStrength") + coord_flip() 


g2 <- ggplot(data = network,aes(x=Sentiment, y=Closeness, group=Type, color=Type)) +
  geom_line() + raincloud_theme3  + geom_point() + ylab("\nCloseness") + coord_flip()


g3 <- ggplot(data = network,aes(x=Sentiment, y=Betweenness, group=Type, color=Type)) +
  geom_line() + raincloud_theme4  + geom_point() + ylab("\nBetweenness") + coord_flip()



g1 + g2 + g3


#---------------------------------------------------------------------------

raincloud_theme4 = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

g1 <- ggplot(data = network_personal,aes(x=Sentiment, y=Strength,group=1,color=1)) +
  geom_line() + raincloud_theme2  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\nStrength") + coord_flip() 


g2 <- ggplot(data = network_personal,aes(x=Sentiment, y=Closeness, group=1, color=1)) +
  geom_line() + raincloud_theme3  + geom_point() + ylab("\nCloseness") + coord_flip()


g3 <- ggplot(data = network_personal,aes(x=Sentiment, y=Betweenness, group=1, color=1)) +
  geom_line() + raincloud_theme4  + geom_point() + ylab("\nBetweenness") + coord_flip()



g1 + g2 + g3

