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
dc_ep <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')

colnames(dc_ep)[1] <- "Id"
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_ep$Id))
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2

#############################################################
network_centrality <- list(); edge_num <- list()
length.days <- list(); network_stability <- list()

dep_array <- list()

for(id in unique(FYP_df$Id)){
  
  print(paste0(which(unique(FYP_df$Id) == id)/length(unique(FYP_df$Id))," ",id))
  
  en_var <- FYP_df %>% filter(Id == id) %>% select(negemo,posemo,i,we,pro3,you,swear,article,negate)

  SDS <- as.numeric(participants %>% filter(Id == id) %>% select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% filter(Id == id) %>% select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)
  
  #series length must be a minimum of 30 days and each node must have some variance 
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(net1 <- graphicalVAR(en_var, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                             maxit.out = 100),silent = TRUE)
    
    dep_array[[id]] <- net1$PCC
    
  }
}

#---------------------------------------------------------------------------------------------------

strength_theme = theme(
  panel.background = element_blank(),
  legend.title = element_text(size = 20),
  legend.text = element_text(size=20),
  legend.key=element_blank(), 
  text = element_text(size = 20),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(colour = "black"),
  axis.text = element_text(size = 20),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

dep_array_pcc <- array(as.numeric(unlist(dep_array)), dim=c(9, 9, 946))
dep_array_mean <- as.data.frame(apply(dep_array_pcc, c(1,2), mean))
colnames(dep_array_mean) <- c("negemo","posemo","i","we","pro3","you","swear","article","negate")

#low.dep_array <- array(as.numeric(unlist(low.dep_array)), dim=c(10, 10, 468))
#low.dep_array_mean <- as.data.frame(apply(low.dep_array, c(1,2), mean))
#colnames(low.dep_array_mean) <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")


#high.dep_array <- array(as.numeric(unlist(high.dep_array)), dim=c(10, 10, 474))
#high.dep_array_mean <- as.data.frame(apply(high.dep_array, c(1,2), mean))
#colnames(high.dep_array_mean) <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")



#network_low.dep <- as.data.frame(centrality(qgraph(low.dep_array_mean))$InDegree)
#colnames(network_low.dep) <- "Strength"
#network_low.dep$Sentiment <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")
#network_low.dep$Depression <- "Low"

#network_high.dep <- as.data.frame(centrality(qgraph(high.dep_array_mean))$InDegree)
#colnames(network_high.dep) <- "Strength"
#network_high.dep$Sentiment <- c("negemo","posemo","i","we","shehe","they","you","swear","article","negate")
#network_high.dep$Depression <- "High"

#network_var <- as.data.frame(rbind(network_low.dep,network_high.dep)) 
network_var <- dep_array_mean


#c("#DD8D29","#E2D200","#46ACC8","#0B775E",
#  "#C51B7D","#046C9A","#B40F20","#8C510A",
#  "#08306B","#7FBC41")


#c("#eab779","#ede35f","#8bcbdc","#66aa9a","#db70ad",
#  "#62a3c0","#d06873","#b79265","#647da2","#afd588")




#####################################

#160 opacity 
q1 <- qgraph(low.dep_array_mean,fade=F,layout = "circle",
       color=  c("#647da2","#8bcbdc","#afd588","#eab779","#ede35f","#db70ad",
                 "#b79265","#62a3c0","#66aa9a","#d06873"),
       vTrans = 255,
       borders = FALSE,minimum = 0.008,
       label.norm = "OOOOOO",label.cex = 2.5,
       label.fill.vertical = 1,label.fill.horizontal = 1,
       label.color = "black",label.font = 2,
       node.width = 1.25,edge.width = 5,esize = 5, lty = 1,font = 2)

rownames(dep_array_mean) <- colnames(dep_array_mean)
dep_array_mean <- dep_array_mean %>%  select(sort(current_vars())) %>% arrange(rownames(dep_array_mean))


#rownames(low.dep_array_mean) <- colnames(low.dep_array_mean)
#low.dep_array_mean <- low.dep_array_mean %>%  select(sort(current_vars())) %>% arrange(rownames(low.dep_array_mean))


#rownames(high.dep_array_mean) <- colnames(high.dep_array_mean)
#high.dep_array_mean <- high.dep_array_mean %>%  select(sort(current_vars())) %>% arrange(rownames(high.dep_array_mean))




tiff("Figures/Depression_network.tiff",width = 8, height = 8, units = 'in', res = 600)
qgraph(dep_array_mean,fade=F,layout = "circle",
       color=  c("#647da2","#8bcbdc","#afd588","#eab779","#ede35f",
                 "#db70ad","#b79265","#66aa9a","#d06873"),
       vTrans = 255,
       borders = FALSE,minimum = 0.008,labels = "",
       label.norm = "OOOOOO",label.cex = 2.5,
       label.fill.vertical = 1,label.fill.horizontal = 1,
       label.color = "black",label.font = 2,
       node.width = 1.25,edge.width = 5,esize = 5, lty = 1,font = 2)
dev.off()



#tiff("Figures/High_Depression.tiff",width = 8, height = 8, units = 'in', res = 600)
#qgraph(high.dep_array_mean,fade=F,layout = "circle",
#       color=  c("#647da2","#8bcbdc","#afd588","#eab779","#ede35f","#db70ad",
#                 "#b79265","#62a3c0","#66aa9a","#d06873"),
#       vTrans = 255,
#       borders = FALSE,minimum = 0.008,labels = "",
#       label.norm = "OOOOOO",label.cex = 2.5,
#       label.fill.vertical = 1,label.fill.horizontal = 1,
#       label.color = "black",label.font = 2,
#       node.width = 1.25,edge.width = 5,esize = 5, lty = 1,font = 2)
#dev.off()

