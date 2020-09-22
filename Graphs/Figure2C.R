#--------------------------------------
#Figure 2C: Mean Personalised Depression Network (N = 946)
#--------------------------------------

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


#network_var <- dep_array_mean
rownames(dep_array_mean) <- colnames(dep_array_mean)
dep_array_mean <- dep_array_mean %>%  select(sort(current_vars())) %>% arrange(rownames(dep_array_mean))



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

