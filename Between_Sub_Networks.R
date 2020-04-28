library(dplyr)
library(ggplot2)
library(lmerTest)
library(Hmisc)
library(bootnet)
library(qgraph)
library(NetworkComparisonTest)
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

##################################################################
##################################################################

#Top 10 Sentiments selected by Elastic Net (l1 ratio - 0.9) to be most highly associated with SDS Total score 

f1 <- FYP %>% select(Id,SDS_Total, Dep_ep_pastyear,WC,bio,WPS,negemo,shehe,adverb,leisure,Exclam,death,
                     Sixltr)


N1_full <- estimateNetwork(f1[,3:12],default = "EBICglasso",tuning=0.5,corMethod = "cor_auto")
qgraph(N1_full$graph,layout="spring",labels = colnames(N1_full$graph))

centralityPlot(list(Complete_Sample = N1_full),include = c("Closeness","Betweenness","Strength"),
               scale = "z-score")


f1.dep <- f1 %>% filter(Dep_ep_pastyear == 1) %>% select(-c(Dep_ep_pastyear))
f1.nodep <- f1 %>% filter(Dep_ep_pastyear == 0) %>% select(-c(Dep_ep_pastyear))

#depressed network
N1 <- estimateNetwork(f1.dep[,3:11],default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")

#non-depressed network
N2 <- estimateNetwork(f1.nodep[,3:11],default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")

N1_graph <- qgraph(N1$graph,layout="spring",labels = colnames(N1$graph))

#centrality values using raw-values
centralityPlot(list(Depressed = N1,Not_Depressed=N2),include = c("Closeness","Betweenness","Strength"),
               scale = "raw")

#network plots 
qgraph(N1$graph,layout="spring",labels = colnames(N1$graph))
layout_N1 <- qgraph(N1$graph,layout="spring",labels = colnames(N1$graph))$layout
qgraph(N2$graph,layout=layout_N1,labels = colnames(N1$graph))


#permutation test 
EN_NCT <- NCT(N1,N2,binary.data = FALSE,
                paired = FALSE, weighted = TRUE, abs = TRUE, test.centrality = TRUE, gamma = 0.5,
                centrality = c("closeness","betweenness","strength"))

###########################################################################################

dep1 <- f1 %>% filter(SDS_Total < 38) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep2 <- f1 %>% filter(SDS_Total >= 38 & SDS_Total < 48) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep3 <- f1 %>% filter(SDS_Total >= 48 & SDS_Total < 56) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep4 <- f1 %>% filter(SDS_Total >= 56) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))

#depressed network
dep1_Net <- estimateNetwork(dep1,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")
dep2_Net <- estimateNetwork(dep2,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")
dep3_Net <- estimateNetwork(dep3,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")
dep4_Net <- estimateNetwork(dep4,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")


centralityPlot(list(Q1 = dep1_Net,Q2=dep2_Net,Q3 = dep3_Net,Q4 = dep4_Net),include = c("Closeness","Betweenness","Strength"),
               scale = "raw")



#permutation test 
EN_NCT <- NCT(dep1_Net,dep4_Net,binary.data = FALSE,it = 1000,
              paired = FALSE, weighted = TRUE, abs = TRUE, test.centrality = TRUE, gamma = 0.5,
              centrality = c("closeness","betweenness","strength"))


dep1 <- f1 %>% filter(SDS_Total < 50) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep2 <- f1 %>% filter(SDS_Total >= 50 & SDS_Total <= 59) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep3 <- f1 %>% filter(SDS_Total >= 60) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))


#depressed network
dep1_Net <- estimateNetwork(dep1,default = "EBICglasso",tuning=0.25,corMethod = "cor_auto",missing = "pairwise")
dep2_Net <- estimateNetwork(dep2,default = "EBICglasso",tuning=0.25,corMethod = "cor_auto",missing = "pairwise")
dep3_Net <- estimateNetwork(dep3,default = "EBICglasso",tuning=0.25,corMethod = "cor_auto",missing = "pairwise")


centralityPlot(list(Q1 = dep1_Net,Q2=dep2_Net,Q3 = dep3_Net),include = c("Closeness","Betweenness","Strength"),
               scale = "raw")



#permutation test 
EN_NCT <- NCT(dep1_Net,dep3_Net,binary.data = FALSE,it = 100,
              paired = FALSE, weighted = TRUE, abs = TRUE, test.centrality = TRUE, gamma = 0.5,
              centrality = c("closeness","betweenness","strength"))
############################################################################################

#network stability for N1_full (all participants), N1 (depressed), and N2 (non-depressed) 
N1_full_stab <- bootnet(N1_full,nBoots = 1000, nCores = 8, type = "case",
                        statistics = c("closeness","betweenness","strength"),caseN = 1000)
plot(N1_full_stab,statistics = c("closeness","betweenness","strength"))
corStability(N1_full_stab)



N1_stab <- bootnet(N1,nBoots = 1000, nCores = 8, type = "case",
                   statistics = c("closeness","betweenness","strength"),caseN = 1000)
plot(N1_stab,statistics = c("closeness","betweenness","strength"))
corStability(N1_stab)



#network stability for N1 (depressed) and N2 (non-depressed) 
N2_stab <- bootnet(N2,nBoots = 1000, nCores = 8, type = "case",
                   statistics = c("closeness","betweenness","strength"),caseN = 1000)
plot(N2_stab,statistics = c("closeness","betweenness","strength"))
corStability(N2_stab)



##################################################################
##################################################################
#Variables selected from de Choudhury 


f1 <- FYP %>% select(Id,SDS_Total, Dep_ep_pastyear,negemo,posemo,i,we,shehe,they,you,swear,article,negate)   

f1_cor <- FYP %>% select(negemo,posemo,i,we,pro3,you,swear,article,negate,SDS_Total)   

r.mat = rcorr(as.matrix(na.omit(f1_cor)))$r
p.mat = rcorr(as.matrix(na.omit(f1_cor)))$P

corrplot::corrplot(r.mat,method = "square",p.mat = p.mat,sig.level = 0.05,insig = "blank", mar = c(2,0,2,0),
                   cl.lim=c(-1,1),is.corr = FALSE)



N1_full <- estimateNetwork(f1[,3:11],default = "EBICglasso",tuning=0.5,corMethod = "cor_auto")
qgraph(N1_full$graph,layout="spring",labels = colnames(N1_full$graph))

centralityPlot(list(Complete_Sample = N1_full),include = c("Closeness","Betweenness","Strength"),
               scale = "z-score")


f1.dep <- f1 %>% filter(Dep_ep_pastyear == 1) %>% select(-c(Dep_ep_pastyear))
f1.nodep <- f1 %>% filter(Dep_ep_pastyear == 0) %>% select(-c(Dep_ep_pastyear))

#depressed network
N1 <- estimateNetwork(f1.dep[,2:11],default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")

#non-depressed network
N2 <- estimateNetwork(f1.nodep[,2:11],default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")

N1_graph <- qgraph(N1$graph,layout="spring",labels = colnames(N1$graph))

#centrality values using raw-values
centralityPlot(list(Depressed = N1,Not_Depressed=N2),include = c("Closeness","Betweenness","Strength"),
               scale = "raw")

dc.net_comp <- NCT(N1,N2,binary.data = FALSE,
                paired = FALSE, weighted = TRUE, abs = TRUE, test.centrality = TRUE, gamma = 0.5,
                centrality = c("closeness","betweenness","strength"))
###########################################################################################3

dep1 <- f1 %>% filter(SDS_Total < 38) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep2 <- f1 %>% filter(SDS_Total >= 38 & SDS_Total < 48) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep3 <- f1 %>% filter(SDS_Total >= 48 & SDS_Total < 56) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep4 <- f1 %>% filter(SDS_Total >= 56) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))

#depressed network
dep1_Net <- estimateNetwork(dep1,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")
dep2_Net <- estimateNetwork(dep2,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")
dep3_Net <- estimateNetwork(dep3,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")
dep4_Net <- estimateNetwork(dep4,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto",missing = "pairwise")


centralityPlot(list(Q1 = dep1_Net,Q2=dep2_Net,Q3 = dep3_Net,Q4 = dep4_Net),include = c("Closeness","Betweenness","Strength"),
               scale = "raw")
EN_NCT <- NCT(dep1_Net,dep4_Net,binary.data = FALSE,it = 1000,
              paired = FALSE, weighted = TRUE, abs = TRUE, test.centrality = TRUE, gamma = 0.5,
              centrality = c("closeness","betweenness","strength"))

##############################################################################################
dep1 <- f1 %>% filter(SDS_Total <= 31) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))
dep2 <- f1 %>% filter(SDS_Total >= 63) %>% select(-c(Id,Dep_ep_pastyear,SDS_Total))


#depressed network
dep1_Net <- estimateNetwork(dep1,default = "EBICglasso",tuning=0.25,corMethod = "cor_auto",missing = "pairwise")
dep2_Net <- estimateNetwork(dep2,default = "EBICglasso",tuning=0.25,corMethod = "cor_auto",missing = "pairwise")
dep3_Net <- estimateNetwork(dep3,default = "EBICglasso",tuning=0.25,corMethod = "cor_auto",missing = "pairwise")


centralityPlot(list(Q1 = dep1_Net,Q2=dep2_Net),include = c("Closeness","Betweenness","Strength"),
               scale = "raw")



#permutation test 
EN_NCT <- NCT(dep1_Net,dep2_Net,binary.data = FALSE,it = 100,
              paired = FALSE, weighted = TRUE, abs = TRUE, test.centrality = TRUE, gamma = 0.5,
              centrality = c("closeness","betweenness","strength"))
