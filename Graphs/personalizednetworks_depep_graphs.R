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
library(DescTools)


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
##################################################################################################

dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
dc_net$Id <- dc_net[,1]
dc_net <- dc_net[,-1]
dc_net[,1:11] <- remove_all_outliers(dc_net[1:11])

summary(glm(Mean_Centrality ~ Depressive_Episode_pastyear,data = dc_net))
summary(glm(ngm~ Depressive_Episode_pastyear,data = dc_net))
summary(glm(swr ~ Depressive_Episode_pastyear,data = dc_net))
summary(glm(Mean_Centrality ~ SDS_Total,data = dc_net))

my_datal <- melt(dc_net, id.vars = c("Id","Depressive_Episode_pastyear"), 
                 measure.vars = c("ngm", "psm", "i", "we","shh","thy","you","swr",
                                  "art","ngt"),
                 variable.name = "NetworkVariable", value.name = "NodeStrength")


my_datal <- my_datal %>% mutate(NetworkVariable=recode(NetworkVariable, 
                                                       "ngm"="negemo",
                                                       "psm"="posemo",
                                                       "shh" = "shehe",
                                                       "thy" = "they",
                                                       "swr" = "swear",
                                                       "art" = "article",
                                                       "ngt" = "negate"))

##################################################################################################
#################################################################################################


g1 <- ggplot(data = dc_net, aes(y = Mean_Centrality, x = as.factor(Depressive_Episode_pastyear), fill = as.factor(Depressive_Episode_pastyear))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Mean_Centrality, color = as.factor(Depressive_Episode_pastyear)), position = position_jitter(width = .15), size = 1.25, alpha = 0.8) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  raincloud_theme + xlab("Depressed Period in Past Year") + ylab("Global Network Strength\n") +
  scale_x_discrete(labels=c("0" = "No Depressive Episode", "1" = "Depressive Episode"))

g1_nopoints <- ggplot(data = dc_net, aes(y = Mean_Centrality, x = as.factor(Depressive_Episode_pastyear), fill = as.factor(Depressive_Episode_pastyear))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  raincloud_theme + xlab("Depressed Period in Past Year") + ylab("Global Network Strength\n") +
  scale_x_discrete(labels=c("0" = "No Depressive Episode", "1" = "Depressive Episode"))

g2 <- ggplot(data = my_datal, aes(y = NodeStrength, x = NetworkVariable, fill = as.factor(Depressive_Episode_pastyear))) +
  geom_point(aes(y = NodeStrength, color = as.factor(Depressive_Episode_pastyear)), position = position_jitterdodge(jitter.width = 0.5,dodge.width = 0.75), size = 1, alpha = .7) +
  geom_boxplot(width = .5, guides = FALSE, outlier.shape = NA, alpha = 0.5,position=position_dodge(width=0.75)) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # coord_flip() +
  theme_bw() +
  raincloud_theme2 + xlab("\nNetwork Sentiment") + ylab("Individual Node Strength\n")


g3 <- ggplot(dc_net,aes(y = Mean_Centrality,x=SDS_Total)) + geom_point() +  xlab("SDS Summed Score") + raincloud_theme2 + 
  geom_smooth(method = "lm",se=FALSE,size=2) + xlab("\nDepression z-score") + 
  ylab("Global Network Strength\n")


combined <- g2 + (g1/g3)  +  plot_layout(widths = c(2, 1))


combined

#--------------------------------------------------------------------------------------------

#node strength centrality plot means

within_ep <- as.data.frame(colMeans(dc_net %>% filter(Depressive_Episode_pastyear == 1) %>% select(ngm, psm, i, we,shh,thy,you,swr,art,ngt),na.rm = T))
within_ep$Episode <- "Within"
within_ep$Sentiment <- c("negemo", "posemo", "i", "we","shehe","they","you","swear","article","negate")
colnames(within_ep) <- c("Strength","Episode","Sentiment")

outside_ep <- as.data.frame(colMeans(dc_net %>% filter(Depressive_Episode_pastyear == 0) %>% select(ngm, psm, i, we,shh,thy,you,swr,art,ngt),na.rm = T))
outside_ep$Episode <- "Outside"
outside_ep$Sentiment <- c("negemo", "posemo", "i", "we","shehe","they","you","swear","article","negate")
colnames(outside_ep) <- c("Strength","Episode","Sentiment")


network_var <- as.data.frame(rbind(within_ep,outside_ep)) 
g_mean <- ggplot(data = network_var,aes(x=Sentiment, y=Strength,group=Episode,color=Episode)) +
  geom_line() + raincloud_theme2  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\n Mean Node Strength") + coord_flip() + scale_color_brewer(palette = "Dark2")
 
g_mean

#--------------------------------------------------------------------------------------------------



episode_depression = theme(
  panel.background = element_blank(),
  legend.position = "none",
  plot.margin = unit(c(1,1,1,1), "cm"),
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


mod.ngm <- (glm(ngm ~ Depressive_Episode_pastyear,data = dc_net))
mod.psm <- (glm(psm ~ Depressive_Episode_pastyear,data = dc_net))
mod.i <- (glm(i ~ Depressive_Episode_pastyear,data = dc_net))
mod.we <- (glm(we ~ Depressive_Episode_pastyear,data = dc_net))
mod.shh <- (glm(shh ~ Depressive_Episode_pastyear,data = dc_net))
mod.thy <- (glm(thy ~ Depressive_Episode_pastyear,data = dc_net))
mod.you <- (glm(you ~ Depressive_Episode_pastyear,data = dc_net))
mod.swr <- (glm(swr~ Depressive_Episode_pastyear,data = dc_net))
mod.art <- (glm(art ~ Depressive_Episode_pastyear,data = dc_net))
mod.ngt <- (glm(ngt~ Depressive_Episode_pastyear,data = dc_net))

ints <- c(coefficients(summary(mod.ngm))[1],coefficients(summary(mod.psm))[1],coefficients(summary(mod.i))[1],
          coefficients(summary(mod.we))[1],coefficients(summary(mod.shh))[1],coefficients(summary(mod.thy))[1],
          coefficients(summary(mod.you))[1],coefficients(summary(mod.swr))[1],coefficients(summary(mod.art))[1],
          coefficients(summary(mod.ngt))[1])


betas <- c(coefficients(summary(mod.ngm))[2],coefficients(summary(mod.psm))[2],coefficients(summary(mod.i))[2],
           coefficients(summary(mod.we))[2],coefficients(summary(mod.shh))[2],coefficients(summary(mod.thy))[2],
           coefficients(summary(mod.you))[2],coefficients(summary(mod.swr))[2],coefficients(summary(mod.art))[2],
           coefficients(summary(mod.ngt))[2])

lwr.ci <- c(confint(mod.ngm,parm = "Depressive_Episode_pastyear")[1],confint(mod.psm,parm = "Depressive_Episode_pastyear")[1],
            confint(mod.i,parm = "Depressive_Episode_pastyear")[1],confint(mod.we,parm = "Depressive_Episode_pastyear")[1],
            confint(mod.shh,parm = "Depressive_Episode_pastyear")[1],confint(mod.thy,parm = "Depressive_Episode_pastyear")[1],
            confint(mod.you,parm = "Depressive_Episode_pastyear")[1],confint(mod.swr,parm = "Depressive_Episode_pastyear")[1],
            confint(mod.art,parm = "Depressive_Episode_pastyear")[1],confint(mod.ngt,parm = "Depressive_Episode_pastyear")[1])

upr.ci <- c(confint(mod.ngm,parm = "Depressive_Episode_pastyear")[2],confint(mod.psm,parm = "Depressive_Episode_pastyear")[2],
            confint(mod.i,parm = "Depressive_Episode_pastyear")[2],confint(mod.we,parm = "Depressive_Episode_pastyear")[2],
            confint(mod.shh,parm = "Depressive_Episode_pastyear")[2],confint(mod.thy,parm = "Depressive_Episode_pastyear")[2],
            confint(mod.you,parm = "Depressive_Episode_pastyear")[2],confint(mod.swr,parm = "Depressive_Episode_pastyear")[2],
            confint(mod.art,parm = "Depressive_Episode_pastyear")[2],confint(mod.ngt,parm = "Depressive_Episode_pastyear")[2])



between_ci = data.frame(matrix(vector(), 10, 4,
                            dimnames=list(c(), c("Beta","lwr.ci","upr.ci","Sentiment"))),
                     stringsAsFactors=F)

between_ci$Beta <- betas; between_ci$lwr.ci <- lwr.ci; between_ci$upr.ci <- upr.ci
between_ci$Sentiment <- c("negemo", "posemo", "i", "we","shehe","they","you","swear","article","negate")

difference.plot <- ggplot(data = between_ci,aes(x=Sentiment, y=Beta,group=1)) +
  geom_line() + episode_depression  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab("\n Average Depressive Episode Effect") + coord_flip() + geom_errorbar(aes(ymin=lwr.ci, ymax=upr.ci), width=.1) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "blue", size=1.5)  

depep_box <- ggplot(data = my_datal, aes(y = NodeStrength, x = NetworkVariable, fill = as.factor(Depressive_Episode_pastyear))) +
    geom_boxplot(width = .5, guides = FALSE, outlier.shape = NA, alpha = 0.5,position=position_dodge(width=0.75)) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_bw() +
  episode_depression + xlab("\nNetwork Sentiment") + ylab("Individual Node Strength\n")

#-----------------------------------------------------------------------------------

g_mean + (g1/g3)  +  plot_layout(widths = c(1, 1))
difference.plot + (g1/g3)  +  plot_layout(widths = c(1, 1))

depep_box + (g1_nopoints/g3)  +  plot_layout(widths = c(2, 1))

g

