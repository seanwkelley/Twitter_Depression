library(lmerTest)
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(patchwork)
library(qgraph)
library(bootnet)



setwd('D:/Twitter_Depression_Kelley/')

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )
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
  axis.text.x = element_text(angle = 45, vjust = 0.5,colour = "black"),
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
  axis.text.x = element_text(angle = 45, vjust = 0.5,colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
##########################################################################
#sentiment analysis based on tweets 

FYP_df <- read.csv('Data/Sentiments/all_tweets/VADER_ANEW_LIWC_complete.csv')
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'

participants <- read.csv('Data/Participant_Data/FYP_Twitter_Participants.csv')
participants <- participants[which(is.na(participants$OCI_6) | participants$OCI_6 == 1),]

#average sentiments over the past year 
FYP_df_mean <- aggregate(. ~ Id , data = FYP_df, FUN = "mean")

#remove outliers greater or less than 3 sd from the mean 
FYP_df_mean[,6:93]= remove_all_outliers(FYP_df_mean[6:93])

#create a 3rd person pronoun category 
FYP_df_mean$pro3 <- (FYP_df_mean$shehe + FYP_df_mean$they)/2


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

FYP$SDS_Total <- rowSums(FYP %>% select(colnames(FYP)[grepl("SDS",colnames(FYP))]))
FYP$Dep_ep_pastyear <- as.factor(FYP$Dep_ep_pastyear)
FYP$Depression_Physician <- as.factor(FYP$Depression_Physician)

dechoud <- FYP %>% select(negemo,posemo,i,we,shehe,they,you,swear,article,negate)

dechoud_net <- estimateNetwork(dechoud,default = "EBICglasso",tuning=0.5,corMethod = "cor_auto")
qgraph(dechoud_net$graph,layout="spring",labels = colnames(dechoud_net$graph))

Results1 <- bootnet(dechoud_net, nBoots = 1000, nCores = 8)

N1_full_stab <- bootnet(dechoud_net,nBoots = 1000, nCores = 8, type = "case",
                        statistics = c("closeness","betweenness","strength"),caseN = 100)

plot(N1_full_stab,statistics = c("closeness","betweenness","strength"))
corStability(N1_full_stab)



network <- as.data.frame(centrality(dechoud_net)$InDegree)
colnames(network) <- "Strength"

network.close <- as.data.frame(centrality(dechoud_net)$Closeness)
colnames(network.close) <- "Closeness"


network.between <- as.data.frame(centrality(dechoud_net)$Betweenness)
colnames(network.between) <- "Betweenness"


network_var <- as.data.frame(cbind(network,network.close,network.between))
network_var$Sentiment <- rownames(network_var)

########################################################################

g1 <- ggplot(data = network_var,aes(x=Sentiment, y=Strength,group=1,color=1)) +
  geom_line() + raincloud_theme2  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("Strength\n") + coord_flip() 

g2 <- ggplot(data = network_var,aes(x=Sentiment, y=Closeness, group=1, color=1)) +
  geom_line() + raincloud_theme3  + geom_point() + ylab("Closeness\n") + coord_flip()


g3 <- ggplot(data = network_var,aes(x=Sentiment, y=Betweenness, group=1, color=1)) +
  geom_line() + raincloud_theme3  + geom_point() + ylab("Betweenness\n") + coord_flip() 

g4 <- plot(Results1, labels = TRUE, order = "sample")
g5 <- plot(Results1, "strength", plot = "difference",cex.axis = 1.5)


((g4) | ((g1 + g2 + g3)/g5)) 

########################################################################
h1 <- ggplot(FYP,aes(x=SDS_Total)) + geom_histogram(bins=25,binwidth=1,color="black",fill ="white") +
  raincloud_theme2 + xlab("\nZung Depression Scale") + ylab("Count\n")


h2 <- ggplot(data = FYP, aes(y = SDS_Total, x = as.factor(Dep_ep_pastyear), 
                          fill = as.factor(Dep_ep_pastyear))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = SDS_Total, color = as.factor(Dep_ep_pastyear)),
             position = position_jitter(width = .15), size = 1.25, alpha = 0.8) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  raincloud_theme + xlab("Depressed Period in Past Year") + ylab("Zung Depression Scale\n") +
  scale_x_discrete(labels=c("0" = "No Depressive Episode", "1" = "Depressive Episode"))

h1+ h2 

########################################################################################3

dechoud_table <- as.data.frame(colMeans(na.omit(dechoud)))
dechoud_table[,2] <- apply(na.omit(dechoud),2,sd)
colnames(dechoud_table) <- c("Mean","Standard Deviation")


kable(dechoud_table, digits = 2, format = "html", row.names = TRUE,align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = T,
                font_size = 18,
                position = "center")%>% column_spec(1, bold = T) %>% column_spec(2, width =  "2.5cm") %>%
                cat(., file = "df.html")
