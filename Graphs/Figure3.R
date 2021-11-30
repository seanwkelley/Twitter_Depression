#----------------------------------
#Figure 3: Personalised network connectivity increases during a depressive episode for specific symptoms 
#----------------------------------

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
library(extrafont)
loadfonts(device = "win")

options(scipen=999)

setwd('/Users/seankelley/Twitter_Depression_Kelley/')

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
  text = element_text(size = 46,family = "Arial"),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 46),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  legend.title=element_text(size=46),
  legend.text=element_text(size=46),
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
  text = element_text(size = 28, family = "Arial"),
  axis.title.x = element_text(size = 28),
  axis.title.y = element_text(size = 28),
  axis.text = element_text(size = 28),
  axis.text.x = element_text(angle = 45, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


sensitivity_theme = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 36, family = "Arial"),
  axis.title.x = element_text(size = 36),
  axis.title.y = element_text(size = 36),
  axis.text = element_text(size = 36),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

cbpalette <- c("#DD8D29","#E2D200","#46ACC8","#0B775E","#C51B7D","#B40F20","#8C510A",
               "#08306B","#7FBC41")
##################################################################################################
#personalised networks strength centralities of within/outside episode periods
dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_withinepisode_15d.csv')
dc_net <- dc_net[,-1]
dc_net[,1:11] <- remove_all_outliers(dc_net[1:11])

###################################################################################################
###################################################################################################

#Unadjusted GLMs

summary(lmer(Mean_Centrality ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(ngm ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(psm ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(i ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(we ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(pr3 ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(you ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(swr ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(art ~ Depressed_Today + (1|Id),data = dc_net))
summary(lmer(ngt ~ Depressed_Today + (1|Id),data = dc_net))

#Adjusted GLMs for number of days 
summary(lmer(Mean_Centrality ~ Depressed_Today + Days + (1|Id),data = dc_net))
summary(lmer(ngm ~ Depressed_Today +  Days + (1|Id),data = dc_net))
summary(lmer(psm ~ Depressed_Today +  Days +(1|Id),data = dc_net))
summary(lmer(i ~ Depressed_Today +  Days +(1|Id),data = dc_net))
summary(lmer(we ~ Depressed_Today + Days +  (1|Id),data = dc_net))
summary(lmer(pr3 ~ Depressed_Today +  Days + (1|Id),data = dc_net))
summary(lmer(you ~ Depressed_Today +  Days + (1|Id),data = dc_net))
summary(lmer(swr ~ Depressed_Today +  Days + (1|Id),data = dc_net))
summary(lmer(art ~ Depressed_Today +  Days + (1|Id),data = dc_net))
summary(lmer(ngt ~ Depressed_Today +  Days + (1|Id),data = dc_net))


my_datal <- melt(dc_net, id.vars = c("Id","Depressed_Today"), 
                 measure.vars = c("ngm", "psm", "i", "we","pr3","you","swr",
                                  "art","ngt"),
                 variable.name = "NetworkVariable", value.name = "NodeStrength")


my_datal <- my_datal %>% mutate(NetworkVariable=recode(NetworkVariable, 
                                                       "ngm"="negemo",
                                                       "psm"="posemo",
                                                       "pr3" = "pro3",
                                                       "swr" = "swear",
                                                       "art" = "article",
                                                       "ngt" = "negate"))

my_datal <- my_datal[order(as.character(my_datal$NetworkVariable)),]
my_datal$NetworkVariable <- as.character(my_datal$NetworkVariable)
##################################################################################################
#################################################################################################

g1_violin <- ggplot(data = dc_net, aes(y = Mean_Centrality, x = as.factor(Depressed_Today), fill = as.factor(Depressed_Today))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Mean_Centrality, color = as.factor(Depressed_Today)), position = position_jitter(width = .15), size = 3, alpha = 1) +
  geom_boxplot(width = .2, size = 1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values = c("grey54","grey36"))+
  scale_fill_manual(values = c("grey54","grey36"))+                  
  theme_bw() +
  raincloud_theme + xlab("Depressed Period in Past Year") + 
  ylab("Global Network Strength\n") +
  scale_x_discrete(labels=c("0" = "Outside Episode", "1" = "Within Episode"))

#boxplots of individual node strengths 
#g2 <- ggplot(data = my_datal, aes(y = NodeStrength, x = NetworkVariable,
#                            fill = interaction(NetworkVariable,Depressed_Today))) +
#  geom_point(aes(y = NodeStrength, color = interaction(NetworkVariable,Depressed_Today)), 
#             position = position_jitterdodge(jitter.width = 1.5,dodge.width = 0.75), size = 1.75, alpha = 1) +
#  geom_boxplot(width = .5, guides = FALSE, outlier.shape = NA, alpha = 0.65,position=position_dodge(width=0.75))+
#  expand_limits(x = 1) +  theme_bw() +
#  raincloud_theme2 +
#  scale_color_manual(values = c("#647da2","#8bcbdc","#afd588","#eab779","#ede35f",
#                                "#db70ad","#b79265","#62a3c0","#66aa9a","#d06873",
#                                "#08306B","#46ACC8","#7FBC41","#DD8D29","#E2D200",
#                                "#C51B7D","#8C510A","#046C9A","#0B775E","#B40F20")) + 
#  
#  scale_fill_manual(values = c("#647da2","#8bcbdc","#afd588","#eab779","#ede35f",
#                               "#db70ad","#b79265","#62a3c0","#66aa9a","#d06873",
#                               "#08306B","#46ACC8","#7FBC41","#DD8D29","#E2D200",
#                               "#C51B7D","#8C510A","#046C9A","#0B775E","#B40F20"))  +
#  xlab("\nLIWC Text Feature") + ylab("Individual Node Strength\n")




#combined <- (g2 + g1_violin)
#tiff("Figures/Within_episode.tiff", units="cm", width=70, height=30, res=600)
#combined
#dev.off()

#-----------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------



episode_depression = theme(
  panel.background = element_blank(),
  legend.title =  element_blank(),
  legend.text = element_text(size=46),
  legend.key=element_blank(),
  legend.justification = "right",
  plot.margin = unit(c(1,1,1,2), "cm"),
  text = element_text(size = 46, family = "Arial"),
  axis.title.x = element_text(size = 46,vjust=-2),
  axis.title.y = element_text(size = 46),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  plot.tag = element_text(size = 18,face = "bold"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

#cbpalette <- c("#DD8D29","#E2D200","#46ACC8","#0B775E","#C51B7D","#B40F20","#8C510A",
#               "#08306B","#7FBC41")

#------------------------------------------------------------------------------
mod.ngm <- (lmer(ngm ~ Depressed_Today + (1|Id),data = dc_net))
mod.psm <- (lmer(psm ~ Depressed_Today + (1|Id),data = dc_net))
mod.i <- (lmer(i ~ Depressed_Today + (1|Id),data = dc_net))
mod.we <- (lmer(we ~ Depressed_Today + (1|Id),data = dc_net))
mod.pro3 <- (lmer(pr3 ~ Depressed_Today + (1|Id),data = dc_net))

mod.you <- (lmer(you ~ Depressed_Today + (1|Id),data = dc_net))
mod.swr <- (lmer(swr ~ Depressed_Today + (1|Id),data = dc_net))
mod.art <- (lmer(art ~ Depressed_Today + (1|Id),data = dc_net))
mod.ngt <- (lmer(ngt ~ Depressed_Today + (1|Id),data = dc_net))

ints <- c(coefficients(summary(mod.ngm))[1],coefficients(summary(mod.psm))[1],coefficients(summary(mod.i))[1],
           coefficients(summary(mod.we))[1],coefficients(summary(mod.pro3))[1],
           coefficients(summary(mod.you))[1],coefficients(summary(mod.swr))[1],coefficients(summary(mod.art))[1],
           coefficients(summary(mod.ngt))[1])

betas <- c(coefficients(summary(mod.ngm))[2],coefficients(summary(mod.psm))[2],coefficients(summary(mod.i))[2],
           coefficients(summary(mod.we))[2],coefficients(summary(mod.pro3))[2],
           coefficients(summary(mod.you))[2],coefficients(summary(mod.swr))[2],coefficients(summary(mod.art))[2],
           coefficients(summary(mod.ngt))[2])

lwr.ci <- c(confint(mod.ngm,parm = "Depressed_Today")[1],confint(mod.psm,parm = "Depressed_Today")[1],
            confint(mod.i,parm = "Depressed_Today")[1],confint(mod.we,parm = "Depressed_Today")[1],
            confint(mod.pro3,parm = "Depressed_Today")[1],
            confint(mod.you,parm = "Depressed_Today")[1],confint(mod.swr,parm = "Depressed_Today")[1],
            confint(mod.art,parm = "Depressed_Today")[1],confint(mod.ngt,parm = "Depressed_Today")[1])

upr.ci <- c(confint(mod.ngm,parm = "Depressed_Today")[2],confint(mod.psm,parm = "Depressed_Today")[2],
            confint(mod.i,parm = "Depressed_Today")[2],confint(mod.we,parm = "Depressed_Today")[2],
            confint(mod.pro3,parm = "Depressed_Today")[2],
            confint(mod.you,parm = "Depressed_Today")[2],confint(mod.swr,parm = "Depressed_Today")[2],
            confint(mod.art,parm = "Depressed_Today")[2],confint(mod.ngt,parm = "Depressed_Today")[2])


zscore_ci = data.frame(matrix(vector(), 9, 4,
                            dimnames=list(c(), c("Beta","lwr.ci","upr.ci", "Sentiment"))),
                     stringsAsFactors=F)


zscore_ci$Beta <- betas; zscore_ci$lwr.ci <- lwr.ci
zscore_ci$upr.ci <- upr.ci; zscore_ci$Sentiment <- c("Neg. Emo.", "Pos. Emo.", "1st Pers. Sing.", "1st Pers. Pl.",
                                                     "3rd Pers.","2nd Pers.","Swear","Articles","Negate")

zscore_ci$Sentiment <- factor(zscore_ci$Sentiment, levels = zscore_ci$Sentiment[order(zscore_ci$Sentiment,decreasing = T)])


zscore.plot <- ggplot(data = zscore_ci,aes(x=Sentiment, y=Beta)) + 
  geom_errorbar(aes(ymin=lwr.ci, ymax=upr.ci), width=.25,size=1.5,color = cbpalette) +  
  geom_point(color = cbpalette,size=3) +
  episode_depression  + geom_point(color = cbpalette) + xlab("LIWC Text Feature\n") + 
  ylab(expression("Regression Coefficient (95% CI)")) + coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "navyblue", size=1.5)  


combined <- g1_violin + zscore.plot 

combined

png("Figures/Figure3.png", units="cm", width=93, height=40, res=600)
combined
dev.off()
