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
library(mlVAR)
library(graphicalVAR)
library(NetworkComparisonTest)
library(patchwork)
set.seed(2020)
setwd('D:/Twitter_Depression_Kelley/')

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
  legend.title = element_text(size = 14),
  legend.text = element_text(size=12),
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

dc_ep <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
dc_ws <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode.csv')

colnames(dc_ep)[1] <- "Id"; colnames(dc_ws)[1] <- "Id"
dc_Ws <- dc_ws[,-14]

dc_ep[,1:11] <- remove_all_outliers(dc_ep[1:11])

###############################################################################################
#get sentiments

tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')


#sentiments from participants who passed attention check or are from free recruitment 
FYP_df <- FYP_df[which(FYP_df$Id %in% participants$Id),]
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
FYP_df[,6:93] <- remove_all_outliers(FYP_df[6:93])
###############################################################################################
#subset sentiments by participants from the de choudhury within-subject networks 
FYP_df.1 <- FYP_df %>% filter(Id %in% unique(dc_ep$Id))
participants.1 <- participants %>% filter(Id %in% unique(dc_ep$Id))

#network from all participants with a depressive episode in the past year
dechoud <- FYP_df.1 %>% select(Id,negemo,posemo,i,we,shehe,they,you,swear,article,negate)

################################################################################################

depressed <- FYP_df.1 %>% select(Id,negemo,posemo,i,we,shehe,they,you,swear,article,negate) %>% 
  filter(Id %in% participants.1$Id[which(participants.1$Dep_ep_pastyear == 1)])

nondepressed <- FYP_df.1 %>% select(Id,negemo,posemo,i,we,shehe,they,you,swear,article,negate) %>% 
  filter(Id %in% participants.1$Id[which(participants.1$Dep_ep_pastyear == 0)])


depressed_mean <- aggregate(. ~ Id , data = depressed, FUN = "mean") %>% select(-Id)
nondepressed_mean <- aggregate(. ~ Id , data = nondepressed, FUN = "mean") %>% select(-Id)


dep_net <- estimateNetwork(depressed_mean,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto")
nondep_net <- estimateNetwork(nondepressed_mean,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto")

#between subjects graphs using the within-subject mean of each variable over the past year 
qgraph(dep_net$graph,layout="spring",labels = colnames(dep_net$graph))
layout_dep <- qgraph(dep_net$graph,layout="spring",labels = colnames(dep_net$graph))$layout

qgraph(nondep_net$graph,layout=layout_dep,labels = colnames(dep_net$graph))


Depression_NCT <- NCT(dep_net,nondep_net,paired = FALSE, weighted = TRUE, abs = TRUE,
                      test.centrality = TRUE,test.edges = TRUE,it = 1000,
                      centrality = c("closeness","betweenness","strength"))

dep_stab <- bootnet(dep_net,nBoots = 1000, nCores = 8, type = "case",
                        statistics = c("closeness","betweenness","strength"),caseN = 100)

dep.stab.plot <- plot(dep_stab,statistics = c("closeness","betweenness","strength"))
corStability(dep_stab)


nodep_stab <- bootnet(nondep_net,nBoots = 1000, nCores = 8, type = "case",
                    statistics = c("closeness","betweenness","strength"),caseN = 100)

nodep.stab.plot <- plot(nodep_stab,statistics = c("closeness","betweenness","strength"))
corStability(nodep_stab)

#################################################################################################


network <- as.data.frame(centrality(dep_net)$InDegree)
colnames(network) <- "Strength"
network2 <- as.data.frame(centrality(nondep_net)$InDegree)
colnames(network2) <- "Strength"


network.close <- as.data.frame(centrality(dep_net)$Closeness) *100
colnames(network.close) <- "Closeness"
network.close2 <- as.data.frame(centrality(nondep_net)$Closeness) *100 
colnames(network.close2) <- "Closeness"


network.between <- as.data.frame(centrality(dep_net)$Betweenness)
colnames(network.between) <- "Betweenness"
network.between2 <- as.data.frame(centrality(nondep_net)$Betweenness)
colnames(network.between2) <- "Betweenness"

network_dep <- as.data.frame(cbind(network,network.close,network.between))
network_dep$Sentiment <- rownames(network_dep)
network_dep$Episode <- "Yes" 

network_nondep <- as.data.frame(cbind(network2,network.close2,network.between2))
network_nondep$Sentiment <- rownames(network_nondep)
network_nondep$Episode <- "No" 


network_var <- as.data.frame(rbind(network_dep,network_nondep)) 


g1 <- ggplot(data = network_var,aes(x=Sentiment, y=Strength,group=Episode,color=Episode)) +
  geom_line() + raincloud_theme2  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\nStrength") + coord_flip() + scale_color_brewer(palette = "Dark2")

g2 <- ggplot(data = network_var,aes(x=Sentiment, y=Closeness,group=Episode,color=Episode)) +
  geom_line() + raincloud_theme3  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\nCloseness") + coord_flip() + scale_color_brewer(palette = "Dark2")


g3 <- ggplot(data = network_var,aes(x=Sentiment, y=Betweenness,group=Episode,color=Episode)) +
  geom_line() + raincloud_theme4  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\nBetweenness") + coord_flip() +
  scale_color_brewer(name="Depressive Episode",labels = c("No","Yes"),
                     palette = "Dark2") 


g1+g2+g3


#-------------------------------------------------------------------

#plot of cs coefficient stability 
cs.stability.nodep = theme(
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


cs.stability.dep = theme(
  panel.background = element_blank(),
  legend.title = element_text(size = 14),
  legend.text = element_text(size=12),
  legend.key=element_blank(),
  legend.justification = "right",
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

dep.stab.data <- dep.stab.plot$data
nodep.stab.data <- nodep.stab.plot$data

dep.ep.stability <- ggplot(dep.stab.data ,aes(x = nPerson/max(nPerson),y=mean,group = type)) + 
  geom_line(aes(y = mean, color =type),size=1) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     trans = "reverse",breaks = c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3)) +
  cs.stability.dep + xlab("\nSampled People") + 
  ylab("Average Correlation \n with Original Sample\n") + ylim(-1,1) +
  labs(color = "") + geom_hline(yintercept = 0,linetype="dashed",color="blue",size=0.75) +
  geom_ribbon(aes(ymin=q2.5, ymax=q97.5,fill= type), alpha=0.2,show.legend = F) + 
  ggtitle("Depressive Episode: Yes")


no.dep.ep.stability <- ggplot(nodep.stab.data ,aes(x = nPerson/max(nPerson),y=mean,group = type)) + 
  geom_line(aes(y = mean, color =type),size=1) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     trans = "reverse",breaks = c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3)) +
  cs.stability.nodep + xlab("\nSampled People") + 
  ylab("Average Correlation \n with Original Sample\n") + ylim(-1,1) +
  labs(color = "") + geom_hline(yintercept = 0,linetype="dashed",color="blue",size=0.75) +
  geom_ribbon(aes(ymin=q2.5, ymax=q97.5,fill= type), alpha=0.2,show.legend = F) +
  ggtitle("Depressive Episode: No")

no.dep.ep.stability + dep.ep.stability

#-------------------------------------------------------------------


#########################################################################################
#median split of participants by depression z-score
#########################################################################################
FYP_df.2 <- merge(FYP_df.1,participants.1,by="Id")

median_split <- median(FYP_df.2$Depression_zscore)

depressed <- FYP_df.2 %>% filter(Depression_zscore >= median_split) %>%
select(Id,negemo,posemo,i,we,shehe,they,you,swear,article,negate) 
  

nondepressed <- FYP_df.2 %>% filter(Depression_zscore < median_split) %>%
select(Id,negemo,posemo,i,we,shehe,they,you,swear,article,negate) 
  
depressed_mean <- aggregate(. ~ Id , data = depressed, FUN = "mean") %>% select(-Id)
nondepressed_mean <- aggregate(. ~ Id , data = nondepressed, FUN = "mean") %>% select(-Id)


dep_net <- estimateNetwork(depressed_mean,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto")
nondep_net <- estimateNetwork(nondepressed_mean,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto")

Depression_NCT <- NCT(dep_net,nondep_net,paired = FALSE, weighted = TRUE, abs = TRUE,
                      test.centrality = TRUE,test.edges = TRUE,it = 1000,
                      centrality = c("closeness","betweenness","strength"))


high_dep_stab <- bootnet(dep_net,nBoots = 1000, nCores = 8, type = "case",
                    statistics = c("closeness","betweenness","strength"),caseN = 100)

high_dep.stab.plot <- plot(high_dep_stab,statistics = c("closeness","betweenness","strength"))
corStability(high_dep_stab)


low_dep_stab <- bootnet(nondep_net,nBoots = 1000, nCores = 8, type = "case",
                      statistics = c("closeness","betweenness","strength"),caseN = 100)

low_dep.stab.plot <- plot(low_dep_stab,statistics = c("closeness","betweenness","strength"))
corStability(low_dep_stab)

#--------------------------------------------------------------------------

network <- as.data.frame(centrality(dep_net)$InDegree)
colnames(network) <- "Strength"
network2 <- as.data.frame(centrality(nondep_net)$InDegree)
colnames(network2) <- "Strength"


network.close <- as.data.frame(centrality(dep_net)$Closeness) * 100
colnames(network.close) <- "Closeness"
network.close2 <- as.data.frame(centrality(nondep_net)$Closeness) * 100
colnames(network.close2) <- "Closeness"


network.between <- as.data.frame(centrality(dep_net)$Betweenness)
colnames(network.between) <- "Betweenness"
network.between2 <- as.data.frame(centrality(nondep_net)$Betweenness)
colnames(network.between2) <- "Betweenness"

network_dep <- as.data.frame(cbind(network,network.close,network.between))
network_dep$Sentiment <- rownames(network_dep)
network_dep$Episode <- "Yes" 

network_nondep <- as.data.frame(cbind(network2,network.close2,network.between2))
network_nondep$Sentiment <- rownames(network_nondep)
network_nondep$Episode <- "No" 


network_var <- as.data.frame(rbind(network_dep,network_nondep)) 



g1 <- ggplot(data = network_var,aes(x=Sentiment, y=Strength,group=Episode,color=Episode)) +
  geom_line() + raincloud_theme2  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\nStrength") + coord_flip() + scale_color_brewer(palette = "Paired")

g2 <- ggplot(data = network_var,aes(x=Sentiment, y=Closeness,group=Episode,color=Episode)) +
  geom_line() + raincloud_theme3  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\nCloseness") + coord_flip() + scale_color_brewer(palette = "Paired")


g3 <- ggplot(data = network_var,aes(x=Sentiment, y=Betweenness,group=Episode,color=Episode)) +
  geom_line() + raincloud_theme4  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\nBetweenness") + coord_flip() +
  scale_y_continuous(breaks = c(0, 2.5,5.0, 7.5, 10.0)) + 
  scale_color_brewer(name="Depression Z-Score",labels = c("Low","High"),
                     palette = "Paired")


g1+g2+g3


#-------------------------------------------------------------------

#plot of cs coefficient stability for low/high depression 
cs.stability.nolegend = theme(
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


cs.stability.legend = theme(
  panel.background = element_blank(),
  legend.title = element_text(size = 14),
  legend.text = element_text(size=12),
  legend.key=element_blank(),
  legend.justification = "right",
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

high_dep.stab.data <- high_dep.stab.plot$data
low_dep.stab.data <- low_dep.stab.plot$data

high_dep.stability <- ggplot(high_dep.stab.data, aes(x = nPerson/max(nPerson),y=mean,group = type)) + 
  geom_line(aes(y = mean, color =type),size=1) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     trans = "reverse",breaks = c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3)) +
  cs.stability.nolegend + xlab("\nSampled People") + 
  ylab("Average Correlation \n with Original Sample\n") + ylim(-1,1) +
  labs(color = "") + geom_hline(yintercept = 0,linetype="dashed",color="blue",size=0.75) +
  geom_ribbon(aes(ymin=q2.5, ymax=q97.5,fill= type), alpha=0.2,show.legend = F) + 
  ggtitle("High Depression")


low_dep.stability <- ggplot(low_dep.stab.data,aes(x = nPerson/max(nPerson),y=mean,group = type)) + 
  geom_line(aes(y = mean, color =type),size=1) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     trans = "reverse",breaks = c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3)) +
  cs.stability.nolegend + xlab("\nSampled People") + 
  ylab("Average Correlation \n with Original Sample\n") + ylim(-1,1) +
  labs(color = "") + geom_hline(yintercept = 0,linetype="dashed",color="blue",size=0.75) +
  geom_ribbon(aes(ymin=q2.5, ymax=q97.5,fill= type), alpha=0.2,show.legend = F) +
  ggtitle("Low Depression")

high_dep.stability  + low_dep.stability 

#-------------------------------------------------------------------

combined <- (no.dep.ep.stability + dep.ep.stability) / (low_dep.stability + high_dep.stability ) 
combined + plot_layout(guides = "collect")


