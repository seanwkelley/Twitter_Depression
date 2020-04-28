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


en_net <- read.csv('Data/Results/all_tweets/Node.Strength_elasticnet_withinepisode.csv')
dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode.csv')

en_net <- en_net[,-1]; dc_net <- dc_net[,-1]
en_net[,1:11] <- remove_all_outliers(en_net[1:11]); dc_net[,1:11] <- remove_all_outliers(dc_net[1:11])

summary(lmer(Mean_Centrality ~ Depressed_Today + (1|Id),data = en_net))
summary(lmer(Mean_Centrality ~ Depressed_Today + (1|Id),data = dc_net))

summary(lmer(ngm ~ Depressed_Today + (1|Id),data = en_net))
summary(lmer(ngm ~ Depressed_Today + (1|Id),data = dc_net))

#mean word count
ggplot(data = en_net, aes(x = as.factor(Depressed_Today),y=Mean_Centrality)) + geom_boxplot() + ylab("Mean Node Strength") + 
  theme_bw() + theme(axis.text.x = element_text(size=15),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("Off Depressive Episode","On Depressive Episode")) +
  theme(axis.title.x=element_blank())


ggplot(data = dc_net, aes(x = as.factor(Depressed_Today),y=Mean_Centrality)) + geom_boxplot() + ylab("Mean Node Strength") + 
  theme_bw() + theme(axis.text.x = element_text(size=15),
                     axis.text.y = element_text(size=15),
                     axis.title.y = element_text(size = 20))+
  scale_x_discrete(labels= c("Off Depressive Episode","On Depressive Episode")) +
  theme(axis.title.x=element_blank())

##################################################################################################


