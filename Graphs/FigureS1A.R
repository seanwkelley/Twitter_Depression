#----------------------------------------------------------------------
#Figure S1A. Split half reliability of primary 9-node network.The edge strength among
#36 unique connections between nodes in split halves (n = 473 for each half) of the sample 
#----------------------------------------------------------------------

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

raincloud_theme = theme(
  panel.background = element_blank(),
  text = element_text(size = 46,family = "Arial"),
  axis.title.x = element_text(size = 46),
  axis.title.y = element_text(size = 46),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  legend.title=element_text(size=46),
  legend.text=element_text(size=46),
  legend.position = "right",
  plot.margin = unit(c(1,1,1,1), "cm"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


#############################################################
#############################################################
setwd('/Users/seankelley/Twitter_Depression_Kelley/')

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')

#############################################################
#############################################################

#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]

FYP_df <- FYP_df[which(FYP_df$Date != ''),]

FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2


dc_all <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
colnames(dc_all)[1] <- "Id"

#filter participants with at least 30 days of Tweets
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_all$Id))
#############################################################
network_centrality <- list(); edge_num <- list()
length.days <- list(); network_stability <- list()
list.depression <- list()

PCC_array = PCC_array2 <- list() 
s1 <- sample(1:946,473)


i = 0 
for(id in unique(FYP_df$Id)[s1]){
  
  i = i +1 
  print(i/length(unique(FYP_df$Id)[s1]))
  
  en_var <- FYP_df %>% filter(Id == id) %>% dplyr::select(negemo,posemo,i,we,pro3,you,swear,article,negate)
  
  SDS <- as.numeric(participants %>% dplyr::filter(Id == id) %>% dplyr::select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% dplyr::filter(Id == id) %>% dplyr::select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)
  
  #series length must be a minimum of 30 days and each node must have some variance 
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(net1 <- graphicalVAR(en_var, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                             maxit.out = 100),silent = TRUE)
    
    list.depression[[id]] <- SDS
    PCC_array[[id]] <- net1$PCC[upper.tri(net1$PCC,diag = F)]
    
  }
}

#mean edge strengths 
PCC_array <- array(as.numeric(unlist(PCC_array)), dim=c(36, length(PCC_array)))
PCC_array_mean <- as.data.frame(apply(PCC_array, 1, mean,na.rm = TRUE))


#############################################################
i = 0
for(id in unique(FYP_df$Id)[-s1]){
  i = i + 1
  print(i/length(unique(FYP_df$Id)[-s1]))
  
  en_var <- FYP_df %>% filter(Id == id) %>% dplyr::select(negemo,posemo,i,we,pro3,you,swear,article,negate)
  
  SDS <- as.numeric(participants %>% dplyr::filter(Id == id) %>% dplyr::select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% dplyr::filter(Id == id) %>% dplyr::select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)
  
  #series length must be a minimum of 30 days and each node must have some variance 
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(net1 <- graphicalVAR(en_var, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                             maxit.out = 100),silent = TRUE)
    
    list.depression[[id]] <- SDS
    PCC_array2[[id]] <- net1$PCC[upper.tri(net1$PCC,diag = F)]
    
  }
}


PCC_array2 <- array(as.numeric(unlist(PCC_array2)), dim=c(36, length(PCC_array2)))
#PCC_array2[PCC_array2 == 0] <- NA

PCC_array2_mean <- as.data.frame(apply(PCC_array2, 1, mean,na.rm = TRUE))



df_merge <- as.data.frame(cbind(PCC_array_mean,PCC_array2_mean))
colnames(df_merge) <- c("split1","split2")


png("Figures/FigureS1A.png", units="cm", width=40, height=40, res=600)

ggplot(df_merge,aes(y = split2,x=split1)) + geom_point(alpha = 0.8,color = "#08306B",size = 12) +  xlab("Edge Strength: 50% Split 1") + raincloud_theme + 
  geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + ylab("Edge Strength: 50% split 2")

dev.off()


