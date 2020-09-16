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
library(extrafont)
loadfonts(device = "win")


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
  axis.title.y = element_text(size = 14),
  axis.text = element_text(size = 10),
  axis.text.x = element_text(vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.tag = element_text(size = 18,face = "bold"),
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
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  plot.tag = element_text(size = 18,face = "bold"),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
##################################################################################################
dc_net_og <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
dc_net_og$Id <- dc_net_og[,1]


dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear_nopro3.csv')
dc_net$Id <- dc_net[,1]
dc_net <- dc_net %>% filter(Id %in% dc_net_og$Id)
dc_net <- dc_net[,-1]
dc_net[,1:9] <- remove_all_outliers(dc_net[1:9])


summary(glm(Mean_Centrality ~ Depression_zscore,data = dc_net))
summary(glm(ngm~ Depression_zscore,data = dc_net))
summary(glm(psm ~ Depression_zscore,data = dc_net))
summary(glm(i ~ Depression_zscore,data = dc_net))
summary(glm(we ~ Depression_zscore,data = dc_net))
summary(glm(thy ~ Depression_zscore,data = dc_net))
summary(glm(you ~ Depression_zscore,data = dc_net))
summary(glm(swr ~ Depression_zscore,data = dc_net))
summary(glm(art ~ Depression_zscore,data = dc_net))
summary(glm(ngt ~ Depression_zscore,data = dc_net))


summary(glm(Mean_Centrality ~ Depression_zscore + Days,data = dc_net))
summary(glm(ngm~ Depression_zscore + Days,data = dc_net))
summary(glm(psm ~ Depression_zscore + Days,data = dc_net))
summary(glm(i ~ Depression_zscore + Days,data = dc_net))
summary(glm(we ~ Depression_zscore + Days,data = dc_net))
summary(glm(you ~ Depression_zscore + Days,data = dc_net))
summary(glm(swr ~ Depression_zscore + Days,data = dc_net))
summary(glm(art ~ Depression_zscore + Days,data = dc_net))
summary(glm(ngt ~ Depression_zscore + Days,data = dc_net))


my_datal <- melt(dc_net, id.vars = c("Id","Depression_zscore"), 
                 measure.vars = c("ngm", "psm", "i", "we","you","swr",
                                  "art","ngt"),
                 variable.name = "NetworkVariable", value.name = "NodeStrength")


my_datal <- my_datal %>% mutate(NetworkVariable=recode(NetworkVariable, 
                                                       "ngm"="negemo",
                                                       "psm"="posemo",
                                                       "swr" = "swear",
                                                       "art" = "article",
                                                       "ngt" = "negate"))


#--------------------------------------------------------------------------------------------------



episode_depression = theme(
  panel.background = element_blank(),
  legend.title =  element_blank(),
  legend.text = element_text(size=36),
  legend.key=element_blank(),
  legend.justification = "right",
  plot.margin = unit(c(1,1,1,1), "cm"),
  text = element_text(size = 36, family = "Arial"),
  axis.title.x = element_text(size = 36,vjust=-2),
  axis.title.y = element_text(size = 36),
  axis.text = element_text(size = 36),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  plot.tag = element_text(size = 18,face = "bold"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


cbpalette <- c("#DD8D29","#E2D200","#46ACC8","#0B775E","#B40F20","#8C510A",
               "#08306B","#7FBC41")


mod.ngm <- (glm(ngm ~ Depression_zscore,data = dc_net))
mod.psm <- (glm(psm ~ Depression_zscore,data = dc_net))
mod.i <- (glm(i ~ Depression_zscore,data = dc_net))
mod.we <- (glm(we ~ Depression_zscore,data = dc_net))
mod.you <- (glm(you ~ Depression_zscore,data = dc_net))
mod.swr <- (glm(swr~ Depression_zscore,data = dc_net))
mod.art <- (glm(art ~ Depression_zscore,data = dc_net))
mod.ngt <- (glm(ngt~ Depression_zscore,data = dc_net))

ints <- c(coefficients(summary(mod.ngm))[1],coefficients(summary(mod.psm))[1],coefficients(summary(mod.i))[1],
          coefficients(summary(mod.we))[1],
          coefficients(summary(mod.you))[1],coefficients(summary(mod.swr))[1],coefficients(summary(mod.art))[1],
          coefficients(summary(mod.ngt))[1])


betas <- c(coefficients(summary(mod.ngm))[2],coefficients(summary(mod.psm))[2],coefficients(summary(mod.i))[2],
           coefficients(summary(mod.we))[2],
           coefficients(summary(mod.you))[2],coefficients(summary(mod.swr))[2],coefficients(summary(mod.art))[2],
           coefficients(summary(mod.ngt))[2])

lwr.ci <- c(confint(mod.ngm,parm = "Depression_zscore")[1],confint(mod.psm,parm = "Depression_zscore")[1],
            confint(mod.i,parm = "Depression_zscore")[1],confint(mod.we,parm = "Depression_zscore")[1],
            confint(mod.you,parm = "Depression_zscore")[1],confint(mod.swr,parm = "Depression_zscore")[1],
            confint(mod.art,parm = "Depression_zscore")[1],confint(mod.ngt,parm = "Depression_zscore")[1])

upr.ci <- c(confint(mod.ngm,parm = "Depression_zscore")[2],confint(mod.psm,parm = "Depression_zscore")[2],
            confint(mod.i,parm = "Depression_zscore")[2],confint(mod.we,parm = "Depression_zscore")[2],
            confint(mod.you,parm = "Depression_zscore")[2],confint(mod.swr,parm = "Depression_zscore")[2],
            confint(mod.art,parm = "Depression_zscore")[2],confint(mod.ngt,parm = "Depression_zscore")[2])



zscore_ci = data.frame(matrix(vector(), 8, 4,
                              dimnames=list(c(), c("Beta","lwr.ci","upr.ci","Sentiment"))),
                       stringsAsFactors=F)

zscore_ci$Beta <- betas; zscore_ci$lwr.ci <- lwr.ci; zscore_ci$upr.ci <- upr.ci
zscore_ci$Sentiment <- c("negemo", "posemo", "i", "we","you","swear","article","negate")

zscore.plot <- ggplot(data = zscore_ci,aes(x=Sentiment, y=Beta,group = Sentiment,color = cbpalette)) +
  episode_depression  + geom_point() + xlab("LIWC Sentiment\n") + 
  ylab(expression(beta*" (95% CI)")) + coord_flip() + geom_errorbar(aes(ymin=lwr.ci, ymax=upr.ci), width=.25,size=0.9) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "navyblue", size=1.5)
#-----------------------------------------------------------------------------------
#---------------------------------------------------------------------------
zscore_ci$Sentiment <- factor(zscore_ci$Sentiment, levels = zscore_ci$Sentiment[order(zscore_ci$Sentiment,decreasing = T)])

zscore.plot <- ggplot(data = zscore_ci,aes(x=Sentiment, y=Beta)) + 
  geom_errorbar(aes(ymin=lwr.ci, ymax=upr.ci), width=.25,size=0.9,color = cbpalette) +
  episode_depression  + geom_point(color = cbpalette) + xlab("LIWC Text Feature\n") + 
  ylab(expression("Regression Coefficient (95% CI)")) + coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "navyblue", size=1.5)  



g3 <- ggplot(dc_net,aes(y = Mean_Centrality,x=Depression_zscore)) + geom_point(alpha = 0.80,size=3) +  xlab("SDS Summed Score") + raincloud_theme2 + 
  geom_smooth(method = "lm",se=FALSE,size=3,color="navyblue") + xlab("Depression Symptom Severity (z-score)") + 
  ylab("Global Network Strength\n")+ episode_depression



combined <- zscore.plot + g3+plot_layout(widths = c(1, 1.25)) 

combined

tiff("Figures/Between_Subjects_nopro3.tiff", units="cm", width=70, height=30, res=600)
combined
dev.off()


